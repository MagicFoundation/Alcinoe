(**************************************************************
www:          http://sourceforge.net/projects/alcinoe/
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALJsonDocument
Version:      4.01

Description:  TALJsonDocument is a Delphi parser/writer for JSON / BSON data
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
                                                  const str: AnsiString;
                                                  const NodeSubType: TALJSONNodeSubType)
                                       begin
                                         Writeln(Path + '=' + str);
                                       end;
              MyJsonDoc.LoadFromJSON(AJsonStr, true{saxMode});


Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

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

Link :

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************)
unit ALJSONDoc;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     system.Classes,
     system.sysutils,
     {$ELSE}
     Classes,
     sysutils,
     {$IFEND}
     ALString,
     AlStringList,
     ALXmlDoc;

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

const
  cALJSONNodeMaxListSize = Maxint div 16;

type

  {class definition}
  TALJSONNode = Class;
  TALJSONNodeList= Class;
  TALJSONDocument= Class;

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

  TALJSONObjectID = Array[1..12] of AnsiChar; // ObjectId is a 12-byte BSON type, constructed using:
                                              // a 4-byte value representing the seconds since the Unix epoch,
                                              // a 3-byte machine identifier,
                                              // a 2-byte process id, and
                                              // a 3-byte counter, starting with a random value.

  TALBSONTimestamp = packed record      // Special internal type used by MongoDB replication and sharding.
    case X: integer of                  // First 4 bytes are an increment, second 4 are a timestamp. Setting the
      0: (I64: Int64);                  // timestamp to 0 has special semantics.
      1: (W1:  LongWord;
          W2:  LongWord);
  end;

  TALJSONRegEx = record
    Expression: AnsiString;
    Options:    AnsiString;
  end;

  TALJSONBinary = record
    Subtype: byte;
    Data:    AnsiString;
  end;

  {$IF CompilerVersion >= 23} {Delphi XE2}
  TAlJSONParseDocument = reference to procedure (Sender: TObject);
  TAlJSONParseTextEvent = reference to procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Str: AnsiString; const NodeSubType: TALJSONNodeSubType);
  TAlJSONParseObjectEvent = reference to procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString);
  TAlJSONParseArrayEvent = reference to procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString);
  {$ELSE}
  TAlJSONParseDocument = procedure (Sender: TObject) of object;
  TAlJSONParseTextEvent = procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Str: AnsiString; const NodeSubType: TALJSONNodeSubType) of object;
  TAlJSONParseObjectEvent = procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString) of object;
  TAlJSONParseArrayEvent = procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString) of object;
  {$IFEND}

  {$IF CompilerVersion >= 23} {Delphi XE2}
  TALJSONNodeListSortCompare = reference to function(List: TALJSONNodeList; Index1, Index2: Integer): Integer;
  {$ELSE}
  TALJSONNodeListSortCompare = function(List: TALJSONNodeList; Index1, Index2: Integer): Integer;
  {$IFEND}

  TALJSONDocOption = (doNodeAutoCreate, // create only ntText Node !
                      doNodeAutoIndent); // affect only the SaveToStream
  TALJSONDocOptions = set of TALJSONDocOption;

  TALJSONParseOption = (poIgnoreControlCharacters, // don't decode escaped characters (like \") and not encode them also (when save / load)
                        poAddNodeSubTypeHelperFunct); // By default json (ie: javascript) treats all numbers as floating-point values.
                                                      // To let other system (ie: mongoDB) understand the type of the number
                                                      // we provide the helper functions NumberLong() to handle 64-bit integers
                                                      // and NumberInt() to handle 64-bit integers. theses helper functions are
                                                      // used when saving the json document.
  TALJSONParseOptions = set of TALJSONParseOption;

  PALPointerJSONNodeList = ^TALPointerJSONNodeList;
  TALPointerJSONNodeList = array[0..cALJSONNodeMaxListSize - 1] of TALJSONNode;

  {Exception}
  EALJSONDocError = class(Exception)
  end;

  {TALJSONNodeList}
  {TALJSONNodeList is used to represent a set of related nodes (TALJSONNode object) in an JSON document. For example, TALJSONNodeList is used to
   represent all of the children of a node, or all of the attributes of a node. TALJSONNodeList can be used to add or delete nodes from the
   List, or to access specific nodes.}
  TALJSONNodeList = class(Tobject)
  Private
    FCapacity: Integer;
    FCount: integer;
    FList: PALPointerJSONNodeList;
    FOwner: TALJSONNode;
    procedure QuickSort(L, R: Integer; XCompare: TALJSONNodeListSortCompare);
  protected
    procedure Grow;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    property Owner: TALJSONNode read FOwner;
    function GetCount: Integer;
    function Get(Index: Integer): TALJSONNode;
    {$IF CompilerVersion < 18.5}
    function GetNode(const IndexOrName: OleVariant): TALJSONNode;
    {$IFEND}
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
    function FindNode(const NodeName: AnsiString): TALJSONNode; overload;
    function FindSibling(const Node: TALJSONNode; Delta: Integer): TALJSONNode;
    function First: TALJSONNode;
    function IndexOf(const Name: AnsiString): Integer; overload;
    function IndexOf(const Node: TALJSONNode): Integer; overload;
    function Last: TALJSONNode;
    function Remove(const Node: TALJSONNode): Integer;
    function ReplaceNode(const OldNode, NewNode: TALJSONNode): TALJSONNode;
    procedure Clear;
    procedure Insert(Index: Integer; const Node: TALJSONNode);
    property Count: Integer read GetCount;
    {$IF CompilerVersion < 18.5}
    property Nodes[const IndexOrName: OleVariant]: TALJSONNode read GetNode; default;
    {$ELSE}
    property Nodes[const Name: AnsiString]: TALJSONNode read GetNodeByName; default;
    property Nodes[const Index: integer]: TALJSONNode read GetNodeByIndex; default;
    {$IFEND}
  end;

  {TALJSONNode}
  {TALJSONNode represents a node in an JSON document.}
  TALJSONNode = class(TObject)
  private
    FDocument: TALJSONDocument;
    FParentNode: TALJSONNode;
    fNodeName: AnsiString;
  protected
    function CreateChildList: TALJSONNodeList;
    function InternalGetChildNodes: TALJSONNodeList; virtual;
    function GetChildNodes: TALJSONNodeList; virtual;
    procedure SetChildNodes(const Value: TALJSONNodeList); virtual;
    function GetHasChildNodes: Boolean;
    function GetNodeType: TALJSONNodeType; virtual; abstract;
    function GetNodeSubType: TALJSONNodeSubType; virtual; abstract;
    procedure SetNodeSubType(const Value: TALJSONNodeSubType); virtual; abstract;
    function GetNodeTypeStr: AnsiString;
    function GetNodeValue: ansiString; virtual;
    procedure SetNodeValue(const Value: AnsiString; const NodeSubType: TALJSONNodeSubType); virtual;
    function GetText: AnsiString;
    procedure SetText(const Value: AnsiString);
    function GetFloat: Double;
    procedure SetFloat(const Value: Double);
    function GetDateTime: TDateTime;
    procedure SetDateTime(const Value: TDateTime);
    function GetTimestamp: TALBSONTimestamp;
    procedure SetTimestamp(const Value: TALBSONTimestamp);
    function GetObjectID: TALJSONObjectID;
    procedure SetObjectID(const Value: TALJSONObjectID);
    function GetInt32: Integer;
    procedure SetInt32(const Value: Integer);
    function GetInt64: Int64;
    procedure SetInt64(const Value: Int64);
    function GetBool: Boolean;
    procedure SetBool(const Value: Boolean);
    function GetNull: Boolean;
    procedure SetNull(const Value: Boolean);
    function GetJavascript: AnsiString;
    procedure SetJavascript(const Value: AnsiString);
    function GetRegEx: TALJSONRegEx;
    procedure SetRegEx(const Value: TALJSONRegEx);
    function GetBinary: TALJSONBinary;
    procedure SetBinary(const Value: TALJSONBinary);
    function GetBinaryData: ansiString;
    procedure SetBinaryData(const Value: ansiString);
    function GetOwnerDocument: TALJSONDocument;
    procedure SetOwnerDocument(const Value: TALJSONDocument);
    function GetParentNode: TALJSONNode;
    procedure SetParentNode(const Value: TALJSONNode);
    function GetJSON: AnsiString;
    procedure SetJSON(const Value: AnsiString);
    function GetBSON: AnsiString;
    procedure SetBSON(const Value: AnsiString);
    function NestingLevel: Integer;
  public
    constructor Create(const NodeName: AnsiString); virtual;
    function AddChild(const NodeName: AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode; overload;
    function AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode; overload;
    function NextSibling: TALJSONNode;
    function PreviousSibling: TALJSONNode;
    procedure SaveToFile(const AFileName: AnsiString; const BSONFile: boolean = False);
    procedure SaveToStream(const Stream: TStream; const BSONStream: boolean = False);
    procedure SaveToJSON(var JSON: AnsiString);
    procedure SaveToBSON(var BSON: AnsiString);
    procedure LoadFromFile(const AFileName: AnsiString; const BSONFile: boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromStream(const Stream: TStream; const BSONStream: boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSON(const JSON: AnsiString; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSON(const BSON: AnsiString; Const ClearChildNodes: Boolean = True);
    property ChildNodes: TALJSONNodeList read GetChildNodes write SetChildNodes;
    property HasChildNodes: Boolean read GetHasChildNodes;
    property NodeName: AnsiString read fNodeName write fNodeName;
    property NodeType: TALJSONNodeType read GetNodeType;
    property NodeValue: AnsiString read GetNodeValue; // same as text property but without formating
    property NodeSubType: TALJSONNodeSubType read GetNodeSubType write SetNodeSubType;
    property OwnerDocument: TALJSONDocument read GetOwnerDocument Write SetOwnerDocument;
    property ParentNode: TALJSONNode read GetParentNode;
    property Text: AnsiString read GetText write SetText;
    property int32: integer read GetInt32 write SetInt32;
    property int64: int64 read Getint64 write Setint64;
    property Float: Double read GetFloat write SetFloat;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property Timestamp: TALBSONTimestamp read GetTimestamp write SetTimestamp; // Use only by MongoDB, do not use it, use DateTime instead !
    property ObjectID: TALJSONObjectID read GetObjectID write SetObjectID;
    property Bool: Boolean read GetBool write SetBool;
    property Null: Boolean read GetNull write SetNull;
    property Javascript: AnsiString read GetJavascript write SetJavascript;
    property RegEx: TALJSONRegEx read GetRegEx write SetRegEx;
    property Binary: TALJSONBinary read GetBinary write SetBinary;
    property BinaryData: ansiString read GetBinaryData write SetBinaryData;
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
    procedure SetNodeSubType(const Value: TALJSONNodeSubType); override;
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
    procedure SetNodeSubType(const Value: TALJSONNodeSubType); override;
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
    fNodeValue: AnsiString;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    procedure SetNodeSubType(const Value: TALJSONNodeSubType); override;
    function GetNodeValue: ansiString; override;
    procedure SetNodeValue(const Value: AnsiString; const NodeSubType: TALJSONNodeSubType); override;
  public
    constructor Create(const NodeName: AnsiString = ''); override;
    property NodeValue: AnsiString read fNodeValue; // it's just to have the property without any getter function to economize one string refcount
  end;

  {TALJSONDocument}
  TALJSONDocument = class(TObject)
  private
    FTag: NativeInt;
    FDocumentNode: TALJSONNode;
    FNodeIndentStr: AnsiString;
    FOptions: TALJSONDocOptions;
    FParseOptions: TALJSONParseOptions;
    fPathSeparator: AnsiString;
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
    procedure DoParseText(const Path: AnsiString; const name: AnsiString; const str: AnsiString; const NodeSubType: TALJSONNodeSubType);
    procedure DoParseStartObject(const Path: AnsiString; const Name: AnsiString);
    procedure DoParseEndObject(const Path: AnsiString; const Name: AnsiString);
    procedure DoParseStartArray(const Path: AnsiString; const Name: AnsiString);
    procedure DoParseEndArray(const Path: AnsiString; const Name: AnsiString);
    Procedure ParseJSONStream(Const RawJSONStream: TStream; Const ContainerNode: TALJSONNode);
    Procedure ParseBSONStream(Const RawBSONStream: TStream; Const ContainerNode: TALJSONNode);
    procedure ReleaseDoc;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetChildNodes: TALJSONNodeList;
    function GetDocumentNode: TALJSONNode;
    function GetNodeIndentStr: AnsiString;
    function GetOptions: TALJSONDocOptions;
    function GetParseOptions: TALJSONParseOptions;
    function GetPathSeparator: AnsiString;
    procedure SetPathSeparator(const Value: AnsiString);
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
    procedure Clear;
    function AddChild(const NodeName: AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode;
    function CreateNode(const NodeName: AnsiString; NodeType: TALJSONNodeType): TALJSONNode;
    function IsEmptyDoc: Boolean;
    procedure LoadFromFile(const AFileName: AnsiString; const saxMode: Boolean = False; const BSONFile: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromStream(const Stream: TStream; const saxMode: Boolean = False; const BSONStream: boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSON(const JSON: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSON(const BSON: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure SaveToFile(const AFileName: AnsiString; const BSONFile: boolean = False);
    procedure SaveToStream(const Stream: TStream; const BSONStream: boolean = False);
    procedure SaveToJSON(var JSON: AnsiString);
    procedure SaveToBSON(var BSON: AnsiString);
    property ChildNodes: TALJSONNodeList read GetChildNodes;
    property Node: TALJSONNode read GetDocumentNode;
    property Active: Boolean read GetActive write SetActive;
    property NodeIndentStr: AnsiString read GetNodeIndentStr write SetNodeIndentStr;
    property Options: TALJSONDocOptions read GetOptions write SetOptions;
    property ParseOptions: TALJSONParseOptions read GetParseOptions write SetParseOptions;
    property PathSeparator: AnsiString read GetPathSeparator write SetPathSeparator;
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
Const CALDefaultNodeIndent  = '  '; { 2 spaces }

{misc function}
{$IF CompilerVersion >= 23} {Delphi XE2}
Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
                           const aFormatSettings: TALFormatSettings;
                           const aPath: AnsiString;
                           aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false'); overload;
Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
                           const aFormatSettings: TALFormatSettings;
                           aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false'); overload;
{$ifend}
Procedure ALJSONToTStrings(const aJsonNode: TAlJsonNode;
                           Const aPath: AnsiString;
                           aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false'); overload;
Procedure ALJSONToTStrings(const aJsonNode: TAlJsonNode;
                           aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false'); overload;
procedure ALTStringsToJson(const aLst: TALStrings;
                           aJsonNode: TALJSONNode;
                           Const aPath: AnsiString = '';
                           Const aNameToLowerCase: boolean = false;
                           Const aNullStr: AnsiString = 'null');

Procedure ALJSONToXML(aJSONNode: TALJsonNode;
                      aXMLNode: TALXmlNode;
                      aXMLElementNameForJSONArrayEntries: TalStrings; // JSONArrayNodeName=XMLElementName | ex: transactions=transaction
                                                                      //                                  |     features=feature
                      const aDefaultXMLElementNameForJSONArrayEntries: AnsiString = 'rec'); overload;
Procedure ALJSONToXML(aJSONNode: TALJsonNode;
                      aXMLNode: TALXmlNode;
                      const aDefaultXMLElementNameForJSONArrayEntries: AnsiString = 'rec'); overload;

function ALJsonEncodeWithNodeSubTypeHelperFunction(const aValue: AnsiString;
                                                   aNodeSubType: TALJSONNodeSubType;
                                                   const aFormatSettings: TALFormatSettings): AnsiString;

function ALJSONDocTryStrToRegEx(const S: AnsiString; out Value: TALJSONRegEx): boolean;
function ALJSONDocTryStrTobinary(const S: AnsiString; out Subtype: byte; out Data: AnsiString): boolean;
function ALJSONDocTryStrToDateTime(const S: AnsiString; out Value: TDateTime): Boolean;
function ALJSONDocTryStrToObjectID(const S: AnsiString; out Value: TALJSONObjectID): Boolean;
function ALJSONDocTryStrToTimestamp(const S: AnsiString; out Value: TALBSONTimestamp): Boolean;
function ALJSONDocTryStrToInteger(const S: AnsiString; out Value: integer): Boolean;
function ALJSONDocTryStrToInt64(const S: AnsiString; out Value: int64): Boolean;

implementation

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     System.Math,
     System.Contnrs,
     System.DateUtils,
     {$ELSE}
     Math,
     Contnrs,
     DateUtils,
     {$IFEND}
     AlHTML,
     ALMime,
     ALMisc;

var _cALJsonISODateFormatSettings: TALFormatSettings;

{*************************************************************************************}
function ALJSONDocTryStrToRegEx(const S: AnsiString; out Value: TALJSONRegEx): boolean;
var aRegEx: TALPerlRegEx;
    P1: integer;
    i: integer;
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
      Value.Options    := ALCopyStr(S, P1 + 1, maxint);
      Value.Expression := ALCopyStr(S, 2, P1 - 2);

      // loop on all the options characters
      // to check if they are allowed.
      for i := 1 to Length(Value.Options ) do
        if not (Value.Options[i] in ['i','m','x','l','s','u']) then Exit;

      // check if it's compiling
      aRegEx := TALPerlRegEx.Create;
      try
        aRegEx.RegEx := Value.Expression;
        result := aRegEx.Compile(false{RaiseException});
      finally
        aRegEx.Free;
      end;

    end;

  end;

end;

{******************************************************************************************************}
function ALJSONDocTryStrTobinary(const S: AnsiString; out Subtype: byte; out Data: AnsiString): boolean;
var P1, P2: integer;
    aInt: integer;
begin

  // s must look like
  // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
  // BinData ( 0 , "JliB6gIMRuSphAD2KmhzgQ==" )
  result := false;
  P1 := 1;
  while (P1 <= length(s)) and (s[P1] in [#9, ' ']) do inc(P1);
  if (P1 + 6 > length(s)) or
     (s[P1] <> 'B') or
     (s[P1+1] <> 'i') or
     (s[P1+2] <> 'n') or
     (s[P1+3] <> 'D') or
     (s[P1+4] <> 'a') or
     (s[P1+5] <> 't') or
     (s[P1+6] <> 'a') then exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
                                 // ^
  P1 := p1 + 7{Length('BinData')}; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
                                   //        ^
  while (P1 <= length(s)) and (s[P1] in [#9, ' ']) do inc(P1);
  if (P1 > length(s)) or (s[P1] <> '(') then exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
                                                   //        ^P1
  inc(P1); // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
           //         ^P1
  if (P1 > length(s)) then exit;
  P2 := ALPosEx(',', s, P1);
  if P2 <= P1 then exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
                         //          ^P2

  // init Value.subtype
  if not ALTryStrToInt(ALTrim(ALCopyStr(s, P1, P2 - P1)), aInt) then Exit;
  subtype := aInt;

  // init Value.Data
  p1 := P2 + 1; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
                //           ^P1
  while (P1 <= length(s)) and (s[P1] in [#9, ' ']) do inc(P1);
  if (P1 > length(s)) or (not (s[P1] in ['"',''''])) then exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
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
  Data := ALMimeBase64DecodeString(ALTrim(ALCopyStr(s, P1, P2-P1)));

  // set the result
  result := true;

end;

{*************************************************************************************}
function ALJSONDocTryStrToDateTime(const S: AnsiString; out Value: TDateTime): Boolean;
var aDateStr: AnsiString;
    aQuoteChar: ansiChar;
    P1, P2: integer;
begin

  // s must look like
  // new Date('yyyy-mm-ddThh:nn:ss.zzzZ')
  // Date('yyyy-mm-ddThh:nn:ss.zzzZ')
  // new ISODate('yyyy-mm-ddThh:nn:ss.zzzZ')
  // ISODate('yyyy-mm-ddThh:nn:ss.zzzZ')
  result := false;
  aDateStr := ALTrim(S); // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
  if alpos('new', aDateStr) = 1 then P1 := 4{length('new') + 1} // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                                                //    ^P1
  else P1 := 1;// Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
               // ^P1
  while (P1 <= length(aDateStr)) and (aDateStr[P1] in [#9, ' ']) do inc(P1);
  if (P1 <= length(aDateStr) - 3) and
     (aDateStr[P1]   = 'D') and
     (aDateStr[P1+1] = 'a') and
     (aDateStr[P1+2] = 't') and
     (aDateStr[P1+3] = 'e') then inc(p1, 4)  // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                             //          ^P1
  else if (P1 <= length(aDateStr) - 6) and
          (aDateStr[P1]   = 'I') and
          (aDateStr[P1+1] = 'S') and
          (aDateStr[P1+2] = 'O') and
          (aDateStr[P1+3] = 'D') and
          (aDateStr[P1+4] = 'a') and
          (aDateStr[P1+5] = 't') and
          (aDateStr[P1+6] = 'e') then inc(p1, 7)  // ISODate ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                                  //        ^P1
  else exit;
  while (P1 <= length(aDateStr)) and (aDateStr[P1] in [#9, ' ']) do inc(P1);
  if (P1 > length(aDateStr)) or (aDateStr[P1] <> '(') then exit; // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                                                 //           ^P1
  inc(P1); // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
           //            ^P1
  while (P1 <= length(aDateStr)) and (aDateStr[P1] in [#9, ' ']) do inc(P1);
  if (P1 > length(aDateStr)) or (not (aDateStr[P1] in ['''','"'])) then exit; // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                                                              //             ^P1
  aQuoteChar := aDateStr[P1]; // "
  inc(p1); // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
           //              ^P1
  P2 := P1;
  while (P1 <= length(aDateStr)) and (aDateStr[P1] <> aQuoteChar) do inc(P1);
  if (P1 > length(aDateStr)) then exit; // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                        //                                      ^P1
  result := ALTryStrToDateTime(alStringReplace(alStringReplace(AlcopyStr(aDateStr,P2,P1-P2),
                                               'T',
                                               ' ',
                                               []),
                               'Z',
                               '',
                               []),
                               Value,
                               _cALJsonISODateFormatSettings);
  if not result then exit;

  inc(p1);  // // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
            //                                          ^P1
  while (P1 <= length(aDateStr)) and (aDateStr[P1] in [#9, ' ']) do inc(P1);
  if (P1 <> length(aDateStr)) or (aDateStr[P1] <> ')') then begin
    result := false;
    exit;
  end;

end;

{*******************************************************************************************}
function ALJSONDocTryStrToObjectID(const S: AnsiString; out Value: TALJSONObjectID): Boolean;
var aObjectIDStr: AnsiString;
    aObjectIDhex: AnsiString;
    aQuoteChar: ansiChar;
    P1: integer;
begin

  // s must look like
  // ObjectId ( "507f1f77bcf86cd799439011" )
  result := false;
  aObjectIDStr := ALTrim(S); // ObjectId ( "507f1f77bcf86cd799439011" )
  if alpos('ObjectId', aObjectIDStr) <> 1 then exit;
  P1 := 9{length('ObjectId') + 1}; // ObjectId ( "507f1f77bcf86cd799439011" )
                                   //         ^P1
  while (P1 <= length(aObjectIDStr)) and (aObjectIDStr[P1] in [#9, ' ']) do inc(P1);
  if (P1 > length(aObjectIDStr)) or (aObjectIDStr[P1] <> '(') then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                                                         //          ^P1
  inc(p1);  // ObjectId ( "507f1f77bcf86cd799439011" )
            //           ^P1
  while (P1 <= length(aObjectIDStr)) and (aObjectIDStr[P1] in [#9, ' ']) do inc(P1);
  if (P1 > length(aObjectIDStr)) or (not (aObjectIDStr[P1] in ['''','"'])) then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                                                                      //            ^P1
  aQuoteChar := aObjectIDStr[P1]; // "
  inc(p1); // ObjectId ( "507f1f77bcf86cd799439011" )
           //             ^P1
  if (P1 + 23{(length(aObjectIDhex)) - 1} > length(aObjectIDStr)) then exit;
  setlength(aObjectIDhex,24);
  aObjectIDhex := allowerCase(ALcopyStr(aObjectIDStr,P1,24{length(aObjectIDhex)})); // 507f1f77bcf86cd799439011
  inc(P1, 24{length(aObjectIDhex)}); // ObjectId ( "507f1f77bcf86cd799439011" )
                                     //                                     ^P1
  if (P1 > length(aObjectIDStr)) or (aObjectIDStr[P1] <> aQuoteChar) then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                                                                //                                     ^P1
  inc(p1);  // ObjectId ( "507f1f77bcf86cd799439011" )
            //                                      ^P1
  while (P1 <= length(aObjectIDStr)) and (aObjectIDStr[P1] in [#9, ' ']) do inc(P1);
  if (P1 <> length(aObjectIDStr)) or (aObjectIDStr[P1] <> ')') then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                                                          //                                       ^P1
  //convert 507f1f77bcf86cd799439011 to binary
  result := HexToBin(PansiChar(aObjectIDhex),@Value[1],length(value)) = length(Value);

end;

{*********************************************************************************************}
function ALJSONDocTryStrToTimestamp(const S: AnsiString; out Value: TALBSONTimestamp): Boolean;
var aTimestampStr: AnsiString;
    P1, P2:        integer;
    aArgs:         AnsiString;
    aArg1:         integer;
    aArg2:         integer;
begin

  // s must look like
  // Timestamp(0, 0)
  result        := false;
  aTimestampStr := ALTrim(S);
  if ALPos('Timestamp', aTimestampStr) <> 1 then Exit;
  P1 := 10{Length('Timestamp') + 1}; // Timestamp(0, 0)
                                     //          ^
  while (P1 <= length(aTimestampStr)) and (aTimestampStr[P1] in [#9, ' ']) do inc(P1);
  if (P1 > length(aTimestampStr)) or (aTimestampStr[P1] <> '(') then exit; // Timestamp(0, 0)
                                                                           //          ^P1
  P2 := ALPosEx(')', aTimestampStr, P1);
  if P2 <> Length(aTimestampStr) then exit; // Timestamp(0, 0)
                                            //               ^P2
  aArgs := ALTrim(ALCopyStr(aTimestampStr, P1, P2 - P1)); // 0, 0

  // take arguments of function Timestamp
  P1 := ALPos(',', aArgs);
  if not ALTryStrToInt(ALTrim(ALCopyStr(aArgs, 1,      P1 - 1)), aArg1) then Exit;
  if not ALTryStrToInt(ALTrim(ALCopyStr(aArgs, P1 + 1, maxint)), aArg2) then Exit;

  // build result
  result := true;
  Value.W1 := aArg1; // higher 4 bytes - increment
  Value.W2 := aArg2; // lower  4 bytes - timestamp
end;

{**********************************************************************************}
function ALJSONDocTryStrToInteger(const S: AnsiString; out Value: integer): Boolean;
var aNumberStr: AnsiString;
    aTmpStr: AnsiString;
    aQuoteChar: ansiChar;
    P1, P2: integer;
begin

  // s must look like
  // NumberInt ( "12391293" )
  // NumberInt ( 12391293 )
  // 12391293
  result := ALTryStrToInt(S, Value);
  if result then exit;
  aNumberStr := ALTrim(S); // NumberInt ( "12391293" )
  if alpos('NumberInt', aNumberStr) <> 1 then exit;
  P1 := 10{length('NumberInt') + 1}; // NumberInt ( "12391293" )
                                     //          ^P1
  while (P1 <= length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do inc(P1);
  if (P1 > length(aNumberStr)) or (aNumberStr[P1] <> '(') then exit; // NumberInt ( "12391293" )
                                                                     //           ^P1
  inc(p1);  // NumberInt ( "12391293" )
            //            ^P1
  while (P1 <= length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do inc(P1);
  if (P1 > length(aNumberStr)) then exit
  else if (not (aNumberStr[P1] in ['''','"'])) then begin // NumberInt ( 12391293 )
                                                          //             ^P1
    P2 := P1+1;
    while (P2 <= length(aNumberStr)) and (aNumberStr[P2] in ['0'..'9']) do inc(P2); // NumberInt ( 12391293 )
                                                                                    //                     ^P2
    if P2 > length(aNumberStr) then exit;
    aTmpStr := ALcopyStr(aNumberStr,P1,P2-P1); // 12391293
    P1 := P2; // NumberInt ( 12391293 )
              //                     ^P2

    while (P1 <= length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> length(aNumberStr)) or (aNumberStr[P1] <> ')') then exit; // NumberInt ( "12391293" )
                                                                        //                        ^P1
  end
  else begin // NumberInt ( "12391293" )
             //             ^P1

    aQuoteChar := aNumberStr[P1]; // "
    inc(p1); // NumberInt ( "12391293" )
             //              ^P1
    P2 := ALPosEx(aQuoteChar, aNumberStr, P1);
    if P2 <= P1 then exit;
    aTmpStr := ALcopyStr(aNumberStr,P1,P2-P1); // 12391293
    P1 := P2 + 1; // NumberInt ( "12391293" )
                  //                       ^P1
    while (P1 <= length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> length(aNumberStr)) or (aNumberStr[P1] <> ')') then exit; // NumberInt ( "12391293" )
                                                                        //                        ^P1
  end;

  //convert 12391293 to integer
  result := ALTryStrToInt(aTmpStr, Value);

end;

{******************************************************************************}
function ALJSONDocTryStrToInt64(const S: AnsiString; out Value: int64): Boolean;
var aNumberStr: AnsiString;
    aTmpStr: AnsiString;
    aQuoteChar: ansiChar;
    P1, P2: integer;
begin

  // s must look like
  // NumberLong ( "12391293" )
  // NumberLong ( 12391293 )
  // 12391293
  result := ALTryStrToInt64(S, Value);
  if result then exit;
  aNumberStr := ALTrim(S); // NumberLong ( "12391293" )
  if alpos('NumberLong', aNumberStr) <> 1 then exit;
  P1 := 11{length('NumberLong') + 1}; // NumberLong ( "12391293" )
                                      //           ^P1
  while (P1 <= length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do inc(P1);
  if (P1 > length(aNumberStr)) or (aNumberStr[P1] <> '(') then exit; // NumberLong ( "12391293" )
                                                                     //            ^P1
  inc(p1);  // NumberLong ( "12391293" )
            //             ^P1
  while (P1 <= length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do inc(P1);
  if (P1 > length(aNumberStr)) then exit
  else if (not (aNumberStr[P1] in ['''','"'])) then begin // NumberLong ( 12391293 )
                                                          //              ^P1
    P2 := P1+1;
    while (P2 <= length(aNumberStr)) and (aNumberStr[P2] in ['0'..'9']) do inc(P2); // NumberLong ( 12391293 )
                                                                                    //                      ^P2
    if P2 > length(aNumberStr) then exit;
    aTmpStr := ALcopyStr(aNumberStr,P1,P2-P1); // 12391293
    P1 := P2; // NumberLong ( 12391293 )
              //                      ^P2

    while (P1 <= length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> length(aNumberStr)) or (aNumberStr[P1] <> ')') then exit; // NumberLong ( "12391293" )
                                                                        //                         ^P1
  end
  else begin // NumberLong ( "12391293" )
             //              ^P1

    aQuoteChar := aNumberStr[P1]; // "
    inc(p1); // NumberLong ( "12391293" )
             //               ^P1
    P2 := ALPosEx(aQuoteChar, aNumberStr, P1);
    if P2 <= P1 then exit;
    aTmpStr := ALcopyStr(aNumberStr,P1,P2-P1); // 12391293
    P1 := P2 + 1; // NumberLong ( "12391293" )
                  //                        ^P1
    while (P1 <= length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> length(aNumberStr)) or (aNumberStr[P1] <> ')') then exit; // NumberLong ( "12391293" )
                                                                        //                         ^P1
  end;

  //convert 12391293 to integer
  result := ALTryStrToInt64(aTmpStr, Value);

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
  NodeIndentStr := CALDefaultNodeIndent;
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
  ReleaseDoc;
  inherited;
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
      FDocumentNode := TALJSONObjectNode.Create;
      FDocumentNode.OwnerDocument := Self;
    end
    else ReleaseDoc;
  end;
end;

{*****=********}
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
Procedure TALJSONDocument.ParseJSONStream(Const RawJSONStream: TStream; Const ContainerNode: TALJSONNode);

Const BufferSize: integer = 8192;
Var RawJSONString: AnsiString;
    RawJSONStringLength: Integer;
    RawJSONStringPos: Integer;
    NotSaxMode: Boolean;
    WorkingNode: TALJSONNode;
    DecodeJSONReferences: Boolean;
    UseContainerNodeInsteadOfAddingChildNode: Boolean;
    Paths: TALStringList;
    ArrayIdx: integer;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandRawJSONString: boolean; overload;
  Var ByteReaded, Byte2Read: Integer;
  Begin
    If (RawJSONStringLength > 0) and (RawJSONStringPos > 1) then begin
      if (RawJSONStringPos > RawJSONStringLength) then RawJSONStream.Position := RawJSONStream.Position - RawJSONStringLength + RawJSONStringPos - 1;
      Byte2Read := min(RawJSONStringPos - 1, RawJSONStringLength);
      if RawJSONStringPos <= length(RawJSONString) then ALMove(RawJSONString[RawJSONStringPos],
                                                               RawJSONString[1],
                                                               RawJSONStringLength-RawJSONStringPos+1);
      RawJSONStringPos := 1;
    end
    else begin
      Byte2Read := BufferSize;
      RawJSONStringLength := RawJSONStringLength + BufferSize;
      SetLength(RawJSONString, RawJSONStringLength);
    end;

    //range check error is we not do so
    if RawJSONStream.Position < RawJSONStream.Size then ByteReaded := RawJSONStream.Read(RawJSONString[RawJSONStringLength - Byte2Read + 1],Byte2Read)
    else ByteReaded := 0;

    If ByteReaded <> Byte2Read then begin
      RawJSONStringLength := RawJSONStringLength - Byte2Read + ByteReaded;
      SetLength(RawJSONString, RawJSONStringLength);
      Result := ByteReaded > 0;
    end
    else result := True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandRawJSONString(var PosToKeepSync: Integer): boolean; overload;
  var P1: integer;
  begin
    P1 := RawJSONStringPos;
    result := ExpandRawJSONString;
    PosToKeepSync := PosToKeepSync - (P1 - RawJSONStringPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetPathStr(Const ExtraItems: ansiString = ''): ansiString;
  var I, L, P, Size: Integer;
      S, LB: AnsiString;
  begin
    LB := PathSeparator;
    Size := length(ExtraItems);
    if size <> 0 then Inc(Size, Length(LB));
    for I := 1 to Paths.Count - 1 do Inc(Size, Length(Paths[I]) + Length(LB));
    SetLength(Result, Size);
    P := 1;
    for I := 1 to Paths.Count - 1 do begin
      S := Paths[I];
      L := Length(S);
      if L <> 0 then begin
        ALMove(S[1], Result[P], L);
        Inc(P, L);
      end;
      L := Length(LB);
      if (L <> 0) and
         ((i <> Paths.Count - 1) or
          (ExtraItems <> '')) and
         (((NotSaxMode) and (TALJSONNode(Paths.Objects[I]).nodetype <> ntarray)) or
          ((not NotSaxMode) and (integer(Paths.Objects[I]) <> 2{ntarray}))) then begin
        ALMove(LB[1], result[P], L);
        Inc(P, L);
      end;
    end;
    if ExtraItems <> '' then begin
      L := length(ExtraItems);
      ALMove(ExtraItems[1], result[P], L);
      Inc(P, L);
    end;
    setlength(result,P-1);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function GetNodeSubTypeFromStrValue(const aStrValue: AnsiString; const AQuotedValue: Boolean): TALJSONNodeSubType;
  var aDT:            TDateTime;
      aObjectID:      TALJSONObjectID;
      aTimestamp:     TALBSONTimestamp;
      aRegEx:         TALJSONRegEx;
      aBinarySubType: Byte;
      aBinaryData:    ansiString;
      aInt32:         integer;
      aInt64:         int64;
  begin
    if AQuotedValue then result := NstText
    else if ALIsFloat(aStrValue, ALDefaultFormatSettings) then result := nstFloat
    else if ALJSONDocTryStrToInteger(aStrValue, aInt32) then result := NstInt32
    else if ALJSONDocTryStrToInt64(aStrValue, aInt64) then result := NstInt64
    else if alSameText(aStrValue, 'null') then result := nstNull
    else if ALSametext(aStrValue,'true') or
            ALSametext(aStrValue,'false') then result := nstBoolean
    else if ALJSONDocTryStrToDateTime(aStrValue, aDT) then result := nstDateTime
    else if ALJSONDocTryStrToTimestamp(aStrValue, aTimestamp) then result := nstTimestamp
    else if ALJSONDocTryStrToObjectID(aStrValue, aObjectID) then result := nstObjectID
    else if ALJSONDocTryStrToBinary(aStrValue, aBinarySubType, aBinaryData) then result := nstBinary
    else if ALJSONDocTryStrToRegEx(aStrValue, aRegEx) then result := nstRegEx
    else result := nstJavascript;
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeNode;
  Var aName, aValue: AnsiString;
      aNode: TALJsonNode;
      aQuoteChar: ansiChar;
      aNameValueSeparator: ansiChar;
      aNodeTypeInt: integer;
      aNodeSubType: TALJSONNodeSubType;
      aInSingleQuote: boolean;
      aInDoubleQuote: boolean;
      aInSquareBracket: integer;
      aInRoundBracket: integer;
      aInCurlyBracket: integer;
      P1: Integer;
  Begin

    {$IFDEF undef}{$REGION 'end Object/Array'}{$ENDIF}
    // ... } ....
    // ... ] ....
    if RawJsonString[RawJsonStringPos] in ['}',']'] then begin // ... } ...
                                                               //     ^RawJsonStringPos

      //Reset the ArrayIdx
      ArrayIdx := -1;

      //error if Paths.Count = 0 (mean one end object/array without any starting)
      if (Paths.Count = 0) then ALJSONDocError(CALJSONParseError);

      //if we are not in sax mode
      if NotSaxMode then begin

        //init anode to one level up
        aNode := TALJSONNode(Paths.Objects[paths.Count - 1]);

        //if anode <> workingNode aie aie aie
        if (aNode <> WorkingNode) then ALJSONDocError(CALJSONParseError);

        //check that the end object/array correspond to the aNodeType
        if ((RawJsonString[RawJsonStringPos] = '}') and
            (aNode.NodeType <> ntObject)) or
           ((RawJsonString[RawJsonStringPos] = ']') and
            (aNode.NodeType <> ntarray)) then ALJSONDocError(CALJSONParseError);

        //if working node <> containernode then we can go to one level up
        If WorkingNode<>ContainerNode then begin

          //init WorkingNode to the parentNode
          WorkingNode := WorkingNode.ParentNode;

          //update ArrayIdx if WorkingNode.NodeType = ntArray
          if WorkingNode.NodeType = ntArray then ArrayIdx := ALStrToInt(AlCopyStr(Paths[paths.Count - 1], 2, length(Paths[paths.Count - 1]) - 2)) + 1;

        end

        //if working node = containernode then we can no go to the parent node so set WorkingNode to nil
        Else WorkingNode := nil;

      end

      //if we are in sax mode
      else begin

        //init aNodeTypeInt (we use this as flag in the Paths)
        if RawJsonString[RawJsonStringPos] = '}' then aNodeTypeInt := 1
        else aNodeTypeInt := 2;

        //check that the end object/array correspond to the aNodeType
        if integer(Paths.Objects[paths.Count - 1]) <> aNodeTypeInt then ALJSONDocError(CALJSONParseError);

        //update ArrayIdx if WorkingNode.NodeType = ntArray
        if (paths.Count >= 2) and
           (integer(Paths.Objects[paths.Count - 2]) = 2{ntarray}) then ArrayIdx := ALStrToInt(AlCopyStr(Paths[paths.Count - 1], 2, length(Paths[paths.Count - 1]) - 2)) + 1;

      end;

      //call the DoParseEndObject/array event
      if RawJsonString[RawJsonStringPos] = '}' then DoParseEndObject(GetPathStr, Paths[paths.Count - 1])
      else DoParseEndArray(GetPathStr, Paths[paths.Count - 1]);

      //delete the last entry from the path
      paths.Delete(paths.Count - 1);

      //update rawJSONStringPos
      RawJSONStringPos := RawJsonStringPos + 1; // ... } ...
                                                //      ^RawJsonStringPos

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Begin Object/Array Without NAME'}{$ENDIF}
    // ... { ....
    // ... [ ....
    if RawJsonString[RawJsonStringPos] in ['{','['] then begin // ... { ...
                                                               //     ^RawJsonStringPos

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside the containerNode
        if not assigned(WorkingNode) then ALJSONDocError(CALJSONParseError);

        //we are at the very beginning of the json document
        If UseContainerNodeInsteadOfAddingChildNode then begin

          //check that we are in object
          if RawJsonString[RawJsonStringPos] <> '{' then ALJSONDocError(CALJSONParseError);

          //inform to next loop that we will be not anymore at the very beginning of the json document
          UseContainerNodeInsteadOfAddingChildNode := False;

        end

        //we are inside the json document
        else begin

          //Node without name can be ONLY present inside an array node
          if (ArrayIdx < 0)  or
             (WorkingNode.nodetype <> ntarray) then ALJSONDocError(CALJSONParseError);

          //create the node according the the braket char and add it to the workingnode
          if RawJsonString[RawJsonStringPos] = '{' then aNode := CreateNode('', ntObject)
          else aNode := CreateNode('', ntarray);
          try
            WorkingNode.ChildNodes.Add(aNode);
          except
            aNode.Free;
            raise;
          end;

          //set that the current working node will be now the new node newly created
          WorkingNode := aNode;

        end;

        //update the path
        Paths.AddObject('[' + alinttostr(ArrayIdx) + ']', WorkingNode);

      end

      //if we are in sax mode
      else begin

        //we are at the very beginning of the json document
        If UseContainerNodeInsteadOfAddingChildNode then begin

          //check that we are in object
          if RawJsonString[RawJsonStringPos] <> '{' then ALJSONDocError(CALJSONParseError);

          //inform to next loop that we will be not anymore at the very beginning of the json document
          UseContainerNodeInsteadOfAddingChildNode := False;

        end

        //we are inside the json document
        else begin

          //Node without name can be ONLY present inside an array node
          if (ArrayIdx < 0) or
             ((Paths.Count = 0) or
              (integer(Paths.Objects[paths.Count - 1]) <> 2{array})) then ALJSONDocError(CALJSONParseError);

        end;

        //update the path
        if RawJsonString[RawJsonStringPos] = '{' then aNodeTypeInt := 1
        else aNodeTypeInt := 2;
        Paths.AddObject('[' + alinttostr(ArrayIdx) + ']', pointer(aNodeTypeInt));

      end;

      //call the DoParseStartObject/array event
      if RawJsonString[RawJsonStringPos] = '{' then begin
        DoParseStartObject(GetPathStr,'');
        ArrayIdx := -1;
      end
      else begin
        DoParseStartArray(GetPathStr, '');
        ArrayIdx := 0;
      end;

      //update rawJSONStringPos
      RawJSONStringPos := RawJsonStringPos + 1; // ... { ...
                                                //      ^RawJsonStringPos

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'error if UseContainerNodeInsteadOfAddingChildNode'}{$ENDIF}
    If UseContainerNodeInsteadOfAddingChildNode then ALJSONDocError(CALJSONParseError);
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'extract the quoted name part'}{$ENDIF}
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
    aQuoteChar := #0;
    if RawJsonString[RawJsonStringPos] in ['"',''''] then begin  // ... " ...
                                                                 //     ^RawJsonStringPos
      aQuoteChar := RawJsonString[RawJsonStringPos]; // "
      P1 := RawJsonStringPos + 1; // ... "...\"..."
                                  //      ^P1
      If P1 + 1 > RawJSONStringLength then ExpandRawJSONString(P1);
      While P1 <= RawJSONStringLength do begin

       If (P1 < RawJSONStringLength) and
          (RawJSONString[P1] = '\') and
          (RawJSONString[P1 + 1] = aQuoteChar) then inc(p1, 2) // ... "...\"..."
                                                               //         ^^^P1
       else if RawJSONString[P1] = aQuoteChar then begin
         if DecodeJSONReferences then aName := ALUTF8JavascriptDecode(ALCopyStr(RawJsonString,RawJsonStringPos + 1,P1-RawJsonStringPos - 1)) // ..."...
         else aName := ALCopyStr(RawJsonString,RawJsonStringPos + 1,P1-RawJsonStringPos - 1); // ...\"...
         break;
       end
       else inc(P1); // ... "...\"..."
                     //      ^^^^^^^^^P1

       if P1 + 1 > RawJSONStringLength then ExpandRawJSONString(P1);

      end;
      if P1 > RawJSONStringLength then ALJSONDocError(CALJSONParseError);
      RawJsonStringPos := P1 + 1; // ... "...\"..."
                                  //      ^^^^^^^^^^RawJsonStringPos
    end
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'extract the unquoted name part'}{$ENDIF}
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

      aInSingleQuote := False;
      aInDoubleQuote := False;
      aInSquareBracket := 0;
      aInRoundBracket := 0;
      aInCurlyBracket := 0;

      P1 := RawJsonStringPos; // ... new Date('Dec 03, 1924'), ....
                              //     ^P1
      If P1 > RawJSONStringLength then ExpandRawJSONString(P1);
      While P1 <= RawJSONStringLength do begin

        if (not aInSingleQuote) and
           (not aInDoubleQuote) and
           (aInSquareBracket = 0) and
           (aInRoundBracket = 0) and
           (aInCurlyBracket = 0) and
           (RawJSONString[P1] in [',', '}', ']', ':']) then begin
          aName := ALtrim(ALCopyStr(RawJsonString,RawJsonStringPos,P1-RawJsonStringPos)); // new Date('Dec 03, 1924')
          break;
        end
        else if (RawJSONString[P1] = '"') then begin
          if (P1 <= 1) or
             (RawJSONString[P1 - 1] <> '\') then aInDoubleQuote := (not aInDoubleQuote) and (not aInSingleQuote);
        end
        else if (RawJSONString[P1] = '''') then begin
          if (P1 <= 1) or
             (RawJSONString[P1 - 1] <> '\') then aInSingleQuote := (not aInSingleQuote) and (not aInDoubleQuote)
        end
        else if (not aInSingleQuote) and
                (not aInDoubleQuote) then begin
          if (RawJSONString[P1] = '[') then inc(aInSquareBracket)
          else if (RawJSONString[P1] = ']') then dec(aInSquareBracket)
          else if (RawJSONString[P1] = '(') then inc(aInRoundBracket)
          else if (RawJSONString[P1] = ')') then dec(aInRoundBracket)
          else if (RawJSONString[P1] = '}') then inc(aInCurlyBracket)
          else if (RawJSONString[P1] = '{') then dec(aInCurlyBracket);
        end;

        inc(P1); // ... new Date('Dec 03, 1924'), ....
                 //     ^^^^^^^^^^^^^^^^^^^^^^^^^P1

        if P1 > RawJSONStringLength then ExpandRawJSONString(P1);

      end;
      if P1 > RawJSONStringLength then ALJSONDocError(CALJSONParseError);
      RawJsonStringPos := P1; // ... new Date('Dec 03, 1924'), ....
                              //                             ^RawJsonStringPos

    end;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'extract the name value separator part'}{$ENDIF}
    aNameValueSeparator := #0;
    if RawJSONStringPos > RawJSONStringLength then ExpandRawJSONString;
    While RawJSONStringPos <= RawJSONStringLength do begin
      If RawJSONString[RawJSONStringPos] in [' ', #13, #10, #9] then inc(RawJSONStringPos)
      else begin
        aNameValueSeparator := RawJSONString[RawJSONStringPos];
        break;
      end;
      if RawJSONStringPos > RawJSONStringLength then ExpandRawJSONString;
    end;
    if RawJSONStringPos > RawJSONStringLength then ALJSONDocError(CALJSONParseError);  // .... : ....
                                                                                       //      ^RawJSONStringPos
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'if aNameValueSeparator is absent then it is just a value'}{$ENDIF}
    if aNameValueSeparator <> ':' then begin

      //init aNodeSubType
      aNodeSubType := GetNodeSubTypeFromStrValue(aName, aQuoteChar in ['"','''']);

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside the containerNode
        if not assigned(WorkingNode) then ALJSONDocError(CALJSONParseError);

        //Node without name can be ONLY present inside an array node
        if (ArrayIdx < 0)  or
           (WorkingNode.nodetype <> ntarray) then ALJSONDocError(CALJSONParseError);

        //create the node and add it to the workingnode
        aNode := CreateNode('', nttext);
        try
          aNode.SetNodeValue(aName, aNodeSubType);
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;

      end

      //we are inside the json document
      else begin

          //Node without name can be ONLY present inside an array node
          if (ArrayIdx < 0) or
             ((Paths.Count = 0) or
              (integer(Paths.Objects[paths.Count - 1]) <> 2{array})) then ALJSONDocError(CALJSONParseError);

      end;

      //call the DoParseText event
      DoParseText(GetPathStr('['+alinttostr(ArrayIdx))+']', '', aName, aNodeSubType);

      //increase the ArrayIdx
      inc(ArrayIdx);

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'remove the blank space between the name valueeparator and the value'}{$ENDIF}
    inc(RawJSONStringPos); // ... : ....
                           //      ^RawJSONStringPos
    if RawJSONStringPos > RawJSONStringLength then ExpandRawJSONString;
    While RawJSONStringPos <= RawJSONStringLength do begin
      If RawJSONString[RawJSONStringPos] in [' ', #13, #10, #9] then inc(RawJSONStringPos)
      else break;
      if RawJSONStringPos > RawJSONStringLength then ExpandRawJSONString;
    end;
    if RawJSONStringPos > RawJSONStringLength then ALJSONDocError(CALJSONParseError); // .... " ....
                                                                                      //      ^RawJSONStringPos
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'if the value is an object/array'}{$ENDIF}
    // name : { ... }
    // name : [ ... ]
    if RawJsonString[RawJsonStringPos] in ['{','['] then begin // ... { ...
                                                               //     ^RawJsonStringPos

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside the containerNode
        if not assigned(WorkingNode) then ALJSONDocError(CALJSONParseError);

        //Node withe name MUST be ONLY present inside an object node
        if (ArrayIdx >= 0)  or
           (WorkingNode.nodetype <> ntObject) then ALJSONDocError(CALJSONParseError);

        //create the node according the the braket char and add it to the workingnode
        if RawJsonString[RawJsonStringPos] = '{' then aNode := CreateNode(aName, ntObject)
        else aNode := CreateNode(aName, ntarray);
        try
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;

        //set that the current working node will be now the new node newly created
        WorkingNode := aNode;

        //update the path
        Paths.AddObject(aName, WorkingNode);

      end

      //if we are in sax mode
      else begin

        //Node withe name MUST be ONLY present inside an object node
        if (ArrayIdx >= 0) or
           ((Paths.Count > 0) and
            (integer(Paths.Objects[paths.Count - 1]) <> 1{object})) then ALJSONDocError(CALJSONParseError);

        //update the path
        if RawJsonString[RawJsonStringPos] = '{' then aNodeTypeInt := 1
        else aNodeTypeInt := 2;
        Paths.AddObject(aName, pointer(aNodeTypeInt));

      end;

      //call the DoParseStartObject/array event and update the ArrayIdx if it's an array
      if RawJsonString[RawJsonStringPos] = '{' then DoParseStartObject(GetPathStr,aName)
      else begin
        DoParseStartArray(GetPathStr, aName);
        ArrayIdx := 0;
      end;

      //update rawJSONStringPos
      RawJSONStringPos := RawJsonStringPos + 1; // ... { ...
                                                //      ^RawJsonStringPos

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'if the value is a quoted string'}{$ENDIF}
    // name : "value"
    // name : 'value'
    aQuoteChar := #0;
    if RawJsonString[RawJsonStringPos] in ['"',''''] then begin  // ... " ...
                                                                 //     ^RawJsonStringPos
      aQuoteChar := RawJsonString[RawJsonStringPos]; // "
      P1 := RawJsonStringPos + 1; // ... "...\"..."
                                  //      ^P1
      If P1 + 1 > RawJSONStringLength then ExpandRawJSONString(P1);
      While P1 <= RawJSONStringLength do begin

       If (P1 < RawJSONStringLength) and
          (RawJSONString[P1] = '\') and
          (RawJSONString[P1 + 1] = aQuoteChar) then inc(p1, 2) // ... "...\"..."
                                                               //         ^^^P1
       else if RawJSONString[P1] = aQuoteChar then begin
         if DecodeJSONReferences then aValue := ALUTF8JavascriptDecode(ALCopyStr(RawJsonString,RawJsonStringPos + 1,P1-RawJsonStringPos - 1)) // ..."...
         else aValue := ALCopyStr(RawJsonString,RawJsonStringPos + 1,P1-RawJsonStringPos - 1); // ...\"...
         break;
       end
       else inc(P1); // ... "...\"..."
                     //      ^^^^^^^^^P1

       if P1 + 1 > RawJSONStringLength then ExpandRawJSONString(P1);

      end;
      if P1 > RawJSONStringLength then ALJSONDocError(CALJSONParseError);
      RawJsonStringPos := P1 + 1; // ... "...\"..."
                                  //      ^^^^^^^^^^RawJsonStringPos


    end
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'if the value is a UNquoted string'}{$ENDIF}
    // name : 1.23
    // name : true
    // name : false
    // name : null
    // name : ISODATE('1/1/2001')
    // name : function(){return(new Date).getTime()}, ...}
    // name : new Date(''Dec 03, 1924'')
    else begin

      aInSingleQuote := False;
      aInDoubleQuote := False;
      aInSquareBracket := 0;
      aInRoundBracket := 0;
      aInCurlyBracket := 0;

      P1 := RawJsonStringPos; // ... new Date('Dec 03, 1924'), ....
                              //     ^P1
      If P1 > RawJSONStringLength then ExpandRawJSONString(P1);
      While P1 <= RawJSONStringLength do begin

        if (not aInSingleQuote) and
           (not aInDoubleQuote) and
           (aInSquareBracket = 0) and
           (aInRoundBracket = 0) and
           (aInCurlyBracket = 0) and
           (RawJSONString[P1] in [',', '}', ']']) then begin
          aValue := ALtrim(ALCopyStr(RawJsonString,RawJsonStringPos,P1-RawJsonStringPos)); // new Date('Dec 03, 1924')
          break;
        end
        else if (RawJSONString[P1] = '"') then begin
          if (P1 <= 1) or
             (RawJSONString[P1 - 1] <> '\') then aInDoubleQuote := (not aInDoubleQuote) and (not aInSingleQuote);
        end
        else if (RawJSONString[P1] = '''') then begin
          if (P1 <= 1) or
             (RawJSONString[P1 - 1] <> '\') then aInSingleQuote := (not aInSingleQuote) and (not aInDoubleQuote)
        end
        else if (not aInSingleQuote) and
                (not aInDoubleQuote) then begin
          if (RawJSONString[P1] = '[') then inc(aInSquareBracket)
          else if (RawJSONString[P1] = ']') then dec(aInSquareBracket)
          else if (RawJSONString[P1] = '(') then inc(aInRoundBracket)
          else if (RawJSONString[P1] = ')') then dec(aInRoundBracket)
          else if (RawJSONString[P1] = '}') then inc(aInCurlyBracket)
          else if (RawJSONString[P1] = '{') then dec(aInCurlyBracket);
        end;

        inc(P1); // ... new Date('Dec 03, 1924'), ....
                 //     ^^^^^^^^^^^^^^^^^^^^^^^^^P1

        if P1 > RawJSONStringLength then ExpandRawJSONString(P1);

      end;
      if P1 > RawJSONStringLength then ALJSONDocError(CALJSONParseError);
      RawJsonStringPos := P1; // ... new Date('Dec 03, 1924'), ....
                              //                             ^RawJsonStringPos


    end;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'create the named text node'}{$ENDIF}

    //init aNodeSubType
    aNodeSubType := GetNodeSubTypeFromStrValue(aValue, aQuoteChar in ['"','''']);

    //if we are not in sax mode
    if NotSaxMode then begin

      //if workingnode = nil then it's mean we are outside the containerNode
      if not assigned(WorkingNode) then ALJSONDocError(CALJSONParseError);

      //Node withe name MUST be ONLY present inside an object node
      if (ArrayIdx >= 0)  or
         (WorkingNode.nodetype <> ntObject) then ALJSONDocError(CALJSONParseError);

      //create the node and add it to the workingnode
      aNode := CreateNode(aName, nttext);
      try
        aNode.SetNodeValue(aValue, aNodeSubType);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;

    end

    //if we are in sax mode
    else begin

      //Node with name MUST be ONLY present inside an object node
      if (ArrayIdx >= 0) or
         ((Paths.Count > 0) and
          (integer(Paths.Objects[paths.Count - 1]) <> 1{object})) then ALJSONDocError(CALJSONParseError);

    end;

    //call the DoParseStartObject/array event
    DoParseText(GetPathStr(aName), aName, aValue, aNodeSubType);
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

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

  Paths := TALStringList.Create;
  Try

    DoParseStartDocument;

    ArrayIdx := -1;
    WorkingNode := ContainerNode;
    NotSaxMode := assigned(ContainerNode);
    UseContainerNodeInsteadOfAddingChildNode := True;
    DecodeJSONReferences := not (poIgnoreControlCharacters in ParseOptions);
    RawJSONString := '';
    RawJSONStringLength := 0;
    RawJSONStringPos := 1;
    ExpandRawJSONString;
    if AlUTF8DetectBOM(PansiChar(RawJSONString),length(RawJSONString)) then RawJSONStringPos := 4;

    While RawJSONStringPos <= RawJSONStringLength do begin

      If RawJSONString[RawJSONStringPos] in [' ', ',', #13, #10, #9] then inc(RawJSONStringPos)
      else AnalyzeNode;

      if RawJSONStringPos > RawJSONStringLength then ExpandRawJSONString;

    end;

    //some tags are not closed
    if Paths.Count > 0 then ALJSONDocError(CALJSONParseError);

    //mean the node was not update (empty stream?)
    if UseContainerNodeInsteadOfAddingChildNode then ALJSONDocError(CALJSONParseError);

    DoParseEndDocument;

  finally
    Paths.Free;
  end;

end;

{*************************************************************}
{Last version of the spec: http://bsonspec.org/#/specification}
procedure TALJSONDocument.ParseBSONStream(const RawBSONStream: TStream; const ContainerNode: TALJSONNode);

Const BufferSize: integer = 8192;
Var RawBSONString: AnsiString;
    RawBSONStringLength: Integer;
    RawBSONStringPos: Integer;
    NotSaxMode: Boolean;
    WorkingNode: TALJSONNode;
    Paths: TALStringList;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandRawBSONString: boolean; overload;
  Var ByteReaded, Byte2Read: Integer;
  Begin
    If (RawBSONStringLength > 0) and (RawBSONStringPos > 1) then begin
      if (RawBSONStringPos > RawBSONStringLength) then RawBSONStream.Position := RawBSONStream.Position - RawBSONStringLength + RawBSONStringPos - 1;
      Byte2Read := min(RawBSONStringPos - 1, RawBSONStringLength);
      if RawBSONStringPos <= length(RawBSONString) then ALMove(RawBSONString[RawBSONStringPos],
                                                               RawBSONString[1],
                                                               RawBSONStringLength-RawBSONStringPos+1);
      RawBSONStringPos := 1;
    end
    else begin
      Byte2Read := BufferSize;
      RawBSONStringLength := RawBSONStringLength + BufferSize;
      SetLength(RawBSONString, RawBSONStringLength);
    end;

    //range check error is we not do so
    if RawBSONStream.Position < RawBSONStream.Size then ByteReaded := RawBSONStream.Read(RawBSONString[RawBSONStringLength - Byte2Read + 1],Byte2Read)
    else ByteReaded := 0;

    If ByteReaded <> Byte2Read then begin
      RawBSONStringLength := RawBSONStringLength - Byte2Read + ByteReaded;
      SetLength(RawBSONString, RawBSONStringLength);
      Result := ByteReaded > 0;
    end
    else result := True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandRawBSONString(var PosToKeepSync: Integer): boolean; overload;
  var P1: integer;
  begin
    P1 := RawBSONStringPos;
    result := ExpandRawBSONString;
    PosToKeepSync := PosToKeepSync - (P1 - RawBSONStringPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetPathStr(Const ExtraItems: ansiString = ''): ansiString;
  var I, L, P, Size: Integer;
      S, LB: AnsiString;
  begin
    LB := PathSeparator;
    Size := length(ExtraItems);
    if size <> 0 then Inc(Size, Length(LB));
    for I := 1 to Paths.Count - 1 do Inc(Size, Length(Paths[I]) + Length(LB));
    SetLength(Result, Size);
    P := 1;
    for I := 1 to Paths.Count - 1 do begin
      S := Paths[I];
      L := Length(S);
      if L <> 0 then begin
        ALMove(S[1], Result[P], L);
        Inc(P, L);
      end;
      L := Length(LB);
      if (L <> 0) and
         ((i <> Paths.Count - 1) or
          (ExtraItems <> '')) and
         (((NotSaxMode) and (TALJSONNode(Paths.Objects[I]).nodetype <> ntarray)) or
          ((not NotSaxMode) and (integer(Paths.Objects[I]) <> 2{ntarray}))) then begin
        ALMove(LB[1], result[P], L);
        Inc(P, L);
      end;
    end;
    if ExtraItems <> '' then begin
      L := length(ExtraItems);
      ALMove(ExtraItems[1], result[P], L);
      Inc(P, L);
    end;
    setlength(result,P-1);
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeNode;
  Var aNode: TALJsonNode;
      aNodeTypeInt: integer;
      aNodeSubType: TALJSONNodeSubType;
      aName: AnsiString;
      aDouble: Double;
      aInt32: Integer;
      aInt64: Int64;
      aDateTime: TdateTime;
      aObjectID: TALJSONObjectID;
      aTimestamp: TALBSONTimestamp;
      aBool: Boolean;
      aTextValue: AnsiString;
      aRegEx: TALJSONRegEx;
      aBinary: TALJSONBinary;
      P1: Integer;
  Begin

    {$IFDEF undef}{$REGION 'End Object/Array'}{$ENDIF}
    // ... } ....
    // ... ] ....
    if RawBSONString[RawBSONStringPos] = #$00 then begin

      //error if Paths.Count = 0 (mean one end object/array without any starting)
      if (Paths.Count = 0) then ALJSONDocError(cALBSONParseError);

      //fucking warning
      anodeTypeInt := 0;

      //if we are not in sax mode
      if NotSaxMode then begin

        //init anode to one level up
        aNode := TALJSONNode(Paths.Objects[paths.Count - 1]);

        //if anode <> workingNode aie aie aie
        if (aNode <> WorkingNode) then ALJSONDocError(cALBSONParseError);

        //calculate anodeTypeInt
        if aNode.NodeType = ntObject then aNodeTypeInt := 1
        else if aNode.NodeType = ntarray then aNodeTypeInt := 2
        else ALJSONDocError(cALBSONParseError);

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
        anodeTypeInt := integer(Paths.Objects[paths.Count - 1]);
        if not (anodeTypeInt in [1,2]) then ALJSONDocError(cALBSONParseError);

      end;

      //call the DoParseEndObject/array event
      if anodeTypeInt = 1 then DoParseEndObject(GetPathStr, Paths[paths.Count - 1])
      else DoParseEndArray(GetPathStr, Paths[paths.Count - 1]);

      //delete the last entry from the path
      paths.Delete(paths.Count - 1);

      //update RawBSONStringPos
      RawBSONStringPos := RawBSONStringPos + 1;

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Get the node sub type'}{$ENDIF}
    aNodeSubType := nstText; // to hide fucking warning
    case RawBSONString[RawBSONStringPos] of
      #$01: aNodeSubType := nstFloat;
      #$02: aNodeSubType := nstText;
      #$03: aNodeSubType := nstObject;
      #$04: aNodeSubType := nstArray;
      #$05: aNodeSubType := nstbinary;
      #$07: aNodeSubType := nstObjectID;
      #$08: aNodeSubType := nstBoolean;
      #$09: aNodeSubType := nstDateTime;
      #$0A: aNodeSubType := nstNull;
      #$0B: aNodeSubType := nstRegEx;
      #$0D: aNodeSubType := nstJavascript;
      #$10: aNodeSubType := nstint32;
      #$11: aNodeSubType := nstTimestamp;
      #$12: aNodeSubType := nstint64;
      else ALJSONDocError(cALBSONParseError);
    end;
    RawBSONStringPos := RawBSONStringPos + 1;
    If RawBSONStringPos > RawBSONStringLength then ExpandRawBSONString;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Get the node name'}{$ENDIF}
    P1 := RawBSONStringPos;
    While P1 <= RawBSONStringLength do begin
      If RawBSONString[P1] <> #$00 then inc(P1)
      else begin
        aName := AlCopyStr(RawBSONString, RawBSONStringPos, P1-RawBSONStringPos);
        break;
      end;
      if P1 > RawBSONStringLength then ExpandRawBSONString(P1);
    end;
    if P1 > RawBSONStringLength then ALJSONDocError(cALBSONParseError);
    RawBSONStringPos := P1 + 1;
    if RawBSONStringPos > RawBSONStringLength then ExpandRawBSONString;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Begin Object/Array'}{$ENDIF}
    // ... { ....
    // ... [ ....
    if aNodeSubType in [nstObject,nstArray] then begin

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside the containerNode
        if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);

        //create the node according the the braket char and add it to the workingnode
        if aNodeSubType = nstObject then aNode := CreateNode(ALIfThen(WorkingNode.nodetype=ntarray, '', aName), ntObject)
        else aNode := CreateNode(ALIfThen(WorkingNode.nodetype=ntarray, '', aName), ntarray);
        try
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;

        //set that the current working node will be now the new node newly created
        WorkingNode := aNode;

        //update the path
        Paths.AddObject(ALIfThen(WorkingNode.nodetype=ntarray, '[' + aName + ']', aName), WorkingNode);

      end

      //if we are in sax mode
      else begin

        //update the path
        if aNodeSubType = nstObject then aNodeTypeInt := 1
        else aNodeTypeInt := 2;
        Paths.AddObject(ALIfThen((Paths.Count > 0) and (integer(Paths.Objects[paths.Count - 1]) = 2{array}), '[' + aName + ']', aName), pointer(aNodeTypeInt));

      end;

      //call the DoParseStartObject/array event
      if aNodeSubType = nstObject then DoParseStartObject(GetPathStr,'')
      else DoParseStartArray(GetPathStr, '');

      //update RawBSONStringPos
      RawBSONStringPos := RawBSONStringPos + 4; // we don't need the size of the object/array (4 bytes)

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Remove Delphi warning'}{$ENDIF}
    aBool := False;
    aDateTime := 0;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Extract value: Floating point'}{$ENDIF}
    // \x01 + name + \x00 + double
    if aNodeSubType = nstFloat then begin
      if RawBSONStringPos > RawBSONStringLength - sizeof(Double) + 1 then begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(Double) + 1 then ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aDouble, sizeof(Double));
      aTextValue := ALFloatToStr(aDouble, ALDefaultFormatSettings);
      RawBSONStringPos := RawBSONStringPos + sizeof(Double);
    end
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Extract value: UTF-8 string'}{$ENDIF}
    // \x02 + name + \x00 + length (int32) + string + \x00
    else if aNodeSubType = nstText then begin
      if RawBSONStringPos > RawBSONStringLength - sizeof(aInt32) + 1 then begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(aInt32) + 1 then ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aInt32, sizeof(aInt32));
      RawBSONStringPos := RawBSONStringPos + sizeof(aInt32);
      while (RawBSONStringPos + aInt32 - 1 > RawBSONStringLength) do
        if not ExpandRawBSONString then ALJSONDocError(cALBSONParseError);
      aTextValue := ALCopyStr(RawBSONString,RawBSONStringPos,aInt32 - 1{for the trailing #0});
      RawBSONStringPos := RawBSONStringPos + aInt32;
    end
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Extract value: Binary data'}{$ENDIF}
    // \x05 + name + \x00 + int32 + subtype + (byte*)
    else if aNodeSubType = nstbinary then begin

      {$REGION 'Get size'}
      if RawBSONStringPos > RawBSONStringLength - sizeof(aInt32) + 1 then begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(aInt32) + 1 then ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aInt32, sizeof(aInt32));
      RawBSONStringPos := RawBSONStringPos + sizeof(aInt32);
      {$ENDREGION}

      {$REGION 'Get the subtype'}
      if RawBSONStringPos > RawBSONStringLength then begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength then ALJSONDocError(cALBSONParseError);
      end;
      aBinary.Subtype := Byte(RawBSONString[RawBSONStringPos]);
      RawBSONStringPos := RawBSONStringPos + 1;
      {$ENDREGION}

      {$REGION 'Get the data'}
      while (RawBSONStringPos + aInt32 - 1 > RawBSONStringLength) do
        if not ExpandRawBSONString then ALJSONDocError(cALBSONParseError);
      aBinary.Data := ALCopyStr(RawBSONString,RawBSONStringPos,aInt32);
      RawBSONStringPos := RawBSONStringPos + aInt32;
      {$ENDREGION}

    end
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Extract value: ObjectId'}{$ENDIF}
    // \x07 + name + \x00 + (byte*12)
    else if aNodeSubType = nstObjectID then begin
      if RawBSONStringPos > RawBSONStringLength - length(aObjectID) + 1 then begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - length(aObjectID) + 1 then ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aObjectID[1], length(aObjectID));
      setlength(aTextValue, length(aObjectID) * 2);
      BintoHex(@aObjectID[1],pansichar(aTextValue),length(aObjectID));
      aTextValue := 'ObjectId("'+ AllowerCase(aTextValue) + '")';
      RawBSONStringPos := RawBSONStringPos + length(aObjectID);
    end
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Extract value: Boolean'}{$ENDIF}
    // \x08 + name + \x00 + \x00 => Boolean "false"
    // \x08 + name + \x00 + \x01	=> Boolean "true"
    else if aNodeSubType = nstBoolean then begin
      if RawBSONStringPos > RawBSONStringLength then begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength then ALJSONDocError(cALBSONParseError);
      end;
      if RawBSONString[RawBSONStringPos] = #$00 then begin
        aBool := False;
        aTextValue := 'false';
      end
      else if RawBSONString[RawBSONStringPos] = #$01 then begin
        aBool := true;
        aTextValue := 'true';
      end
      else ALJSONDocError(cALBSONParseError);
      RawBSONStringPos := RawBSONStringPos + 1;
    end
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Extract value: UTC datetime'}{$ENDIF}
    // \x09 + name + \x00 + int64
    else if aNodeSubType = nstDateTime then begin
      if RawBSONStringPos > RawBSONStringLength - sizeof(aInt64) + 1 then begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(aInt64) + 1 then ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aInt64, sizeof(aInt64));
      aDateTime := ALUnixMsToDateTime(aInt64);
      aTextValue := ALFormatDateTime('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', aDateTime, ALDefaultFormatSettings);
      RawBSONStringPos := RawBSONStringPos + sizeof(aInt64);
    end
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Extract value: Timestamp'}{$ENDIF}
    // \x11 + name + \x00 + int64
    else if aNodeSubType = nstTimestamp then begin
      if RawBSONStringPos > RawBSONStringLength - sizeof(aInt64) + 1 then begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(aInt64) + 1 then ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aInt64, sizeof(aInt64));
      aTimestamp.I64 := aInt64;
      aTextValue := 'Timestamp(' + ALUIntToStr(aTimestamp.W1) + ', ' + ALUIntToStr(aTimestamp.W2) + ')';
      RawBSONStringPos := RawBSONStringPos + sizeof(aInt64);
    end
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Extract value: Null value'}{$ENDIF}
    // \x0A + name + \x00
    else if aNodeSubType = nstnull then begin
      aTextValue := 'null';
    end
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Extract value: Regular expression'}{$ENDIF}
    // \x0B + name + \x00 + (byte*) + \x00 + (byte*) + \x00
    else if aNodeSubType = nstRegEx then begin

      {$REGION 'Get pattern'}
      P1 := RawBSONStringPos;
      While P1 <= RawBSONStringLength do begin
        If RawBSONString[P1] <> #$00 then inc(P1)
        else begin
          aRegEx.Expression := AlCopyStr(RawBSONString, RawBSONStringPos, P1 - RawBSONStringPos);
          break;
        end;
        if P1 > RawBSONStringLength then ExpandRawBSONString(P1);
      end;
      if P1 > RawBSONStringLength then ALJSONDocError(cALBSONParseError);
      RawBSONStringPos := P1 + 1;
      if RawBSONStringPos > RawBSONStringLength then ExpandRawBSONString;
      {$ENDREGION}

      {$REGION 'Get options'}
      P1 := RawBSONStringPos;
      While P1 <= RawBSONStringLength do begin
        If RawBSONString[P1] <> #$00 then inc(P1)
        else begin
          aRegEx.Options := AlCopyStr(RawBSONString, RawBSONStringPos, P1 - RawBSONStringPos);
          break;
        end;
        if P1 > RawBSONStringLength then ExpandRawBSONString(P1);
      end;
      if P1 > RawBSONStringLength then ALJSONDocError(cALBSONParseError);
      RawBSONStringPos := P1 + 1;
      if RawBSONStringPos > RawBSONStringLength then ExpandRawBSONString;
      {$ENDREGION}

    end
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Extract value: JavaScript code'}{$ENDIF}
    // \x0D + name + \x00 + length (int32) + string + \x00
    else if aNodeSubType = nstJavascript then begin
      if RawBSONStringPos > RawBSONStringLength - sizeof(aInt32) + 1 then begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(aInt32) + 1 then ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aInt32, sizeof(aInt32));
      RawBSONStringPos := RawBSONStringPos + sizeof(aInt32);
      while (RawBSONStringPos + aInt32 - 1 > RawBSONStringLength) do
        if not ExpandRawBSONString then ALJSONDocError(cALBSONParseError);
      aTextValue := ALCopyStr(RawBSONString,RawBSONStringPos,aInt32 - 1{for the trailing #0});
      RawBSONStringPos := RawBSONStringPos + aInt32;
    end
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Extract value: 32-bit Integer'}{$ENDIF}
    // \x10 + name + \x00 + int32
    else if aNodeSubType = nstint32 then begin
      if RawBSONStringPos > RawBSONStringLength - sizeof(aint32) + 1 then begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(aint32) + 1 then ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aint32, sizeof(aint32));
      aTextValue := 'NumberInt(' + ALIntToStr(aint32) + ')';
      RawBSONStringPos := RawBSONStringPos + sizeof(aint32);
    end
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Extract value: 64-bit integer'}{$ENDIF}
    // \x12 + name + \x00 + int64
    else if aNodeSubType = nstint64 then begin
      if RawBSONStringPos > RawBSONStringLength - sizeof(aInt64) + 1 then begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(aInt64) + 1 then ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aInt64, sizeof(aInt64));
      aTextValue := 'NumberLong(' + ALIntToStr(aint64) + ')';
      RawBSONStringPos := RawBSONStringPos + sizeof(aInt64);
    end
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Extract value: Undefined'}{$ENDIF}
    else ALJSONDocError(cALBSONParseError);
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'create the named text node'}{$ENDIF}
    if NotSaxMode then begin

      //if workingnode = nil then it's mean we are outside the containerNode
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);

      //create the node and add it to the workingnode
      aNode := CreateNode(ALIfThen(WorkingNode.nodetype=ntarray, '', aName), nttext);
      try
        case aNodeSubType of
          nstFloat: aNode.Float := ADouble;
          nstText: aNode.text := aTextValue;
          nstObjectID: aNode.ObjectID := aObjectID;
          nstBoolean: aNode.Bool := aBool;
          nstDateTime: aNode.DateTime := aDateTime;
          nstTimestamp: aNode.Timestamp := aTimestamp;
          nstNull: aNode.null := true;
          nstRegEx: aNode.RegEx := aRegEx;
          nstBinary: aNode.binary := aBinary;
          nstJavascript: aNode.Javascript := aTextValue;
          nstInt32: aNode.int32 := aInt32;
          nstInt64: aNode.int64 := aInt64;
          else ALJSONDocError(cALBSONParseError);
        end;
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;

      //call the DoParseStartObject/array event
      if WorkingNode.nodetype=ntarray then DoParseText(GetPathStr('[' + aName + ']'), '', aTextValue, aNodeSubType)
      else DoParseText(GetPathStr(aName), aName, aTextValue, aNodeSubType);

    end

    //if we are in sax mode
    else begin

      //call the DoParseStartObject/array event
      if (Paths.Count > 0) and (integer(Paths.Objects[paths.Count - 1]) = 2{array}) then DoParseText(GetPathStr('[' + aName + ']'), '', aTextValue, aNodeSubType)
      else DoParseText(GetPathStr(aName), aName, aTextValue, aNodeSubType);

    end;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

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

  Paths := TALStringList.Create;
  Try

    DoParseStartDocument;

    WorkingNode := ContainerNode;
    NotSaxMode := assigned(ContainerNode);
    RawBSONString := '';
    RawBSONStringLength := 0;
    RawBSONStringPos := 5; // the first 4 bytes are the length of the document and we don't need it
    ExpandRawBSONString;
    if NotSaxMode then Paths.AddObject('[-1]', WorkingNode)
    else Paths.AddObject('[-1]', pointer(1));

    While RawBSONStringPos <= RawBSONStringLength do begin
      AnalyzeNode;
      if RawBSONStringPos > RawBSONStringLength then ExpandRawBSONString;
    end;

    //some tags are not closed
    if Paths.Count > 0 then ALJSONDocError(cALBSONParseError);

    //mean the node was not update (empty stream?) or not weel closed
    if WorkingNode <> nil then ALJSONDocError(cALBSONParseError);

    DoParseEndDocument;

  finally
    Paths.Free;
  end;

end;

{***********************************}
procedure TALJSONDocument.ReleaseDoc;
begin
  if assigned(FDocumentNode) then FreeAndNil(FDocumentNode);
end;

{**************************************}
{Loads an JSON document and activates it.
 Call LoadFromFile to load the JSON document specified by AFileName and set the Active property to true so
 that you can examine or modify the document.
 *AFileName is the name of the JSON document to load from disk. If AFileName is an empty string, TALJSONDocument uses the value of the
  FileName property. If AFileName is not an empty string, TALJSONDocument changes the FileName property to AFileName.
 Once you have loaded an JSON document, any changes you make to the document are not saved back to disk until you call the SaveToFile method.}
procedure TALJSONDocument.LoadFromFile(const AFileName: AnsiString; const saxMode: Boolean = False; const BSONFile: Boolean = False; Const ClearChildNodes: Boolean = True);
var FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(string(aFileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FileStream, saxMode, BSONFile, ClearChildNodes);
  finally
    FileStream.Free;
  end;
end;

{****************************************************}
{Loads an JSON document from a stream and activates it.
 Call LoadFromStream to load the JSON document from a stream.
 *Stream is a stream object that can be used to read the string of JSON that makes up the document.
 After loading the document from Stream, LoadFromStream sets the Active property to true.}
procedure TALJSONDocument.LoadFromStream(const Stream: TStream; const saxMode: Boolean = False; const BSONStream: boolean = False; Const ClearChildNodes: Boolean = True);
begin
  if saxMode then SetActive(False)
  else begin
    if ClearChildNodes then releaseDoc;
    SetActive(True);
  end;
  if BSONStream then ParseBSONStream(Stream, FDocumentNode)
  else ParseJSONStream(Stream, FDocumentNode);
end;

{*****************************************************************}
{Loads a string representation of an JSON document and activates it.
 Call LoadFromJSON to assign a string as the value of the JSON document. Unlike the JSON property, which lets you assign JSON on a line-by-line
 basis, LoadFromJSON treats the text of the JSON document as a whole.
 The JSON parameter is a string containing the text of an JSON document. It should represent the JSON text encoded using 8 bits char (utf-8, iso-8859-1, etc)
 After assigning the JSON property as the contents of the document, LoadFromJSON sets the Active property to true.}
procedure TALJSONDocument.LoadFromJSON(const JSON: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
var StringStream: TALStringStream;
begin
  StringStream := TALStringStream.Create(JSON);
  try
    LoadFromStream(StringStream, saxMode, False {BSONStream}, ClearChildNodes);
  finally
    StringStream.Free;
  end;
end;

{************************************************************************************************************************************}
procedure TALJSONDocument.LoadFromBSON(const BSON: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
var StringStream: TALStringStream;
begin
  StringStream := TALStringStream.Create(BSON);
  try
    LoadFromStream(StringStream, saxMode, True {BSONStream}, ClearChildNodes);
  finally
    StringStream.Free;
  end;
end;

{******************************}
{Saves the JSON document to disk.
 Call SaveToFile to save any modifications you have made to the parsed JSON document.
 AFileName is the name of the file to save.}
procedure TALJSONDocument.SaveToFile(const AFileName: AnsiString; const BSONFile: boolean = False);
Var afileStream: TfileStream;
begin
  aFileStream := TfileStream.Create(String(AFileName),fmCreate);
  Try
    SaveToStream(aFileStream, BSONFile);
  finally
    aFileStream.Free;
  end;
end;

{************************************************}
{Saves the JSON document to a string-type variable.
 Call SaveToJSON to save the contents of the JSON document to the string-type variable specified by JSON. SaveToJSON writes the contents of JSON document
 using 8 bits char (utf-8, iso-8859-1, etc) as an encoding system, depending on the type of the JSON parameter.
 Unlike the JSON property, which lets you write individual lines from the JSON document, SaveToJSON writes the entire text of the JSON document.}
procedure TALJSONDocument.SaveToJSON(var JSON: AnsiString);
Var StringStream: TALStringStream;
begin
  StringStream := TALstringStream.Create('');
  Try
    SaveToStream(StringStream, False {BSONStream});
    JSON := StringStream.DataString;
  finally
    StringStream.Free;
  end;
end;

{**********************************************************}
{Saves the JSON document to binary representation of JSON -
 BSON. Specification of this format can be found here:
 http://bsonspec.org/#/specification.
 Format BSON is mostly using for the purposes of
 MongoDB interconnection.}
procedure TALJSONDocument.SaveToBSON(var BSON: AnsiString);
Var StringStream: TALStringStream;
begin
  StringStream := TALstringStream.Create('');
  Try
    SaveToStream(StringStream, True {BSONStream});
    BSON := StringStream.DataString;
  finally
    StringStream.Free;
  end;
end;

{**********************************}
{Saves the JSON document to a stream.
 Call SaveToStream to save the contents of the JSON document to the stream specified by Stream.}
procedure TALJSONDocument.SaveToStream(const Stream: TStream; const BSONStream: boolean = False);
begin
  CheckActive;
  node.SaveToStream(Stream, BSONStream);
end;

{*************************************}
{Returns the value of the JSON property.
 GetJSON is the read implementation of the JSON property.}
function TALJSONDocument.GetJSON: AnsiString;
begin
  SaveToJSON(Result);
end;

{*************************************}
{Returns the value of the BSON property.
 GetBSON is the read implementation of the BSON property.}
function TALJSONDocument.GetBSON: AnsiString;
begin
  SaveToBSON(Result);
end;

{**********************************}
{Sets the value of the JSON property.
 SetJSON is the write implementation of the JSON property.
 *Value contains the raw (unparsed) JSON to assign.}
procedure TALJSONDocument.SetJSON(const Value: AnsiString);
begin
  LoadFromJSON(Value, False{saxMode}, true{ClearChildNodes});
end;

{**********************************}
{Sets the value of the BSON property.
 SetBSON is the write implementation of the BSON property.
 *Value contains the raw (unparsed) BSON to assign.}
procedure TALJSONDocument.SetBSON(const Value: AnsiString);
begin
  LoadFromBSON(Value, False{saxMode}, true{ClearChildNodes});
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

{******************************************************************}
procedure TALJSONDocument.SetPathSeparator(const Value: AnsiString);
begin
  FPathSeparator := Value;
end;

{****************************************************}
function TALJSONDocument.GetPathSeparator: AnsiString;
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

{**************************************************************************************************************************************************}
procedure TALJSONDocument.DoParseText(const Path: AnsiString; const name: AnsiString; const str: AnsiString; const NodeSubType: TALJSONNodeSubType);
begin
  if Assigned(fonParseText) then begin
    case NodeSubType of
      nstFloat: begin
                  if fFormatSettings <> @ALDefaultFormatSettings then fonParseText(Self, Path, name, ALFloatToStr(ALStrToFloat(str, ALDefaultFormatSettings),FormatSettings^), NodeSubType)
                  else fonParseText(Self, Path, name, str, NodeSubType);
                end;
      nstDateTime: begin
                     if fFormatSettings <> @ALDefaultFormatSettings then fonParseText(Self, Path, name, ALDateTimeToStr(ALStrToDateTime(Str, ALDefaultFormatSettings),FormatSettings^), NodeSubType)
                     else fonParseText(Self, Path, name, Str, NodeSubType)
                   end
      else fonParseText(Self, Path, name, str, NodeSubType);
    end;
  end;
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
  ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr])
end;

{****************************************************************}
procedure TALJSONNode.SetChildNodes(const Value: TALJSONNodeList);
begin
  ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr])
end;

{***********************************************}
{Indicates whether this node has any child nodes}
function TALJSONNode.GetHasChildNodes: Boolean;
Var aNodeList: TALJSONNodeList;
begin
  aNodeList := InternalGetChildNodes;
  Result := assigned(aNodeList) and (aNodeList.Count > 0);
end;

{**********************************************}
function TALJSONNode.GetNodeTypeStr: AnsiString;
begin
  case NodeType of
    ntObject: result := 'ntObject';
    ntArray: result := 'ntArray';
    ntText: result := 'ntText';
    else begin
      Result := ''; //for hide warning
      AlJSONDocError(cAlJSONInvalidNodeType);
    end;
  end;
end;

{********************************************}
function TALJSONNode.GetNodeValue: ansiString;
begin
  ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr]);
  result := ''; // hide warning
end;

{*************************************************************************************************}
procedure TALJSONNode.SetNodeValue(const Value: AnsiString; const NodeSubType: TALJSONNodeSubType);
begin
  ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr])
end;

{***********************************}
{Returns the text value of the node.}
function TALJSONNode.GetText: AnsiString;
begin

  case NodeSubType of
    nstFloat: begin
                if Assigned(FDocument) and (Fdocument.FormatSettings <> @ALDefaultFormatSettings) then result := ALFloatToStr(GetFloat, Fdocument.FormatSettings^)
                else result := GetNodeValue;
              end;
    nstText: result := GetNodeValue;
    nstObject: result := GetNodeValue;
    nstArray: result := GetNodeValue;
    nstObjectID: result := GetObjectID; // value of the node like ObjectId ( "507f1f77bcf86cd799439011" ) so we must parse it via GetObjectID
    nstBoolean: result := GetNodeValue;
    nstDateTime: begin
                   if Assigned(FDocument) and (Fdocument.FormatSettings <> @ALDefaultFormatSettings) then result := ALDateTimeToStr(GetDateTime, Fdocument.FormatSettings^)
                   else result := ALDateTimeToStr(GetDateTime, ALDefaultFormatSettings); // value of the node like ISODate('yyyy-mm-ddThh:nn:ss.zzzZ') so we must parse it via GetDateTime
                 end;
    nstNull: result := GetNodeValue;
    nstRegEx: result := GetNodeValue;
    nstBinary: result := GetbinaryData;  // value of the node like BinData(0, "JliB6gIMRuSphAD2KmhzgQ==") so we must parse it via GetbinaryData
    nstJavascript: result := GetNodeValue;
    nstInt32: result := GetNodeValue;
    nstTimestamp: result := GetNodeValue;
    nstInt64: result := GetNodeValue;
    else AlJSONDocError(cALJSONInvalidBSONNodeSubType);
  end;

end;

{********************************}
{Sets the text value of the node.}
procedure TALJSONNode.SetText(const Value: AnsiString);
begin
  setNodeValue(Value, nstText);
end;

{************************************}
function TALJSONNode.GetFloat: Double;
begin
  if NodeSubType = nstText then ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr]);
  If not ALTryStrToFloat(GetNodeValue, Result, ALDefaultFormatSettings) then ALJSONDocError('%s is not a valid float', [GetNodeValue]);
end;

{**************************************************}
procedure TALJSONNode.SetFloat(const Value: Double);
begin
  setNodeValue(ALFloatToStr(Value, ALDefaultFormatSettings), nstFloat);
end;

{******************************************}
function TALJSONNode.GetDateTime: TDateTime;
begin
  if NodeSubType = nstText then ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr]);
  if not ALJSONDocTryStrToDateTime(GetNodeValue, result) then ALJSONDocError('%s is not a valid date and time', [GetNodeValue]);
end;

{********************************************************}
procedure TALJSONNode.SetDateTime(const Value: TDateTime);
begin
  setNodeValue(ALFormatDateTime('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Value, ALDefaultFormatSettings), nstDateTime);
end;

{**************************************************}
function TALJSONNode.GetTimestamp: TALBSONTimestamp;
begin
  if NodeSubType = nstText then ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr]);
  if not ALJSONDocTryStrToTimestamp(GetNodeValue, result) then ALJSONDocError('%s is not a valid BSON-Timestamp', [GetNodeValue]);
end;

{***************************************************************}
procedure TALJSONNode.SetTimestamp(const Value: TALBSONTimestamp);
begin
  setNodeValue('Timestamp(' + ALUIntToStr(Value.W1) + ', ' + ALUIntToStr(Value.W2) + ')', nstTimestamp);
end;

{************************************************}
function TALJSONNode.GetObjectID: TALJSONObjectID;
begin
  if NodeSubType = nstText then ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr]);
  if not ALJSONDocTryStrToObjectID(GetNodeValue, result) then ALJSONDocError('%s is not a valid ObjectID', [GetNodeValue]);
end;

{*************************************************************}
procedure TALJSONNode.SetObjectID(const Value: TALJSONObjectID);
var aStr: AnsiString;
begin
  setlength(aStr, length(Value) * 2);
  BintoHex(@Value[1],pansiChar(aStr),length(Value));
  setNodeValue('ObjectId("'+ALLowerCase(aStr)+'")', nstObjectID);
end;

{*************************************}
function TALJSONNode.GetInt32: Integer;
begin
  if NodeSubType = nstText then ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr]);
  if not ALJSONDocTryStrToInteger(GetNodeValue, result) then ALJSONDocError('%s is not a valid Int32', [GetNodeValue]);
end;

{***************************************************}
procedure TALJSONNode.SetInt32(const Value: Integer);
begin
  setNodeValue(ALIntToStr(Value), nstInt32);
end;

{***********************************}
function TALJSONNode.GetInt64: Int64;
begin
  if NodeSubType = nstText then ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr]);
  if not ALJSONDocTryStrToInt64(GetNodeValue, result) then ALJSONDocError('%s is not a valid Int64', [GetNodeValue]);
end;

{*************************************************}
procedure TALJSONNode.SetInt64(const Value: Int64);
begin
  setNodeValue(ALIntToStr(Value), nstInt64);
end;

{************************************}
function TALJSONNode.GetBool: Boolean;
begin
  if NodeSubType = nstText then ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr]);
  If not ALTryStrToBool(GetNodeValue, result) then ALJSONDocError('%s is not a valid Boolean', [GetNodeValue]);
end;

{**************************************************}
procedure TALJSONNode.SetBool(const Value: Boolean);
begin
  if Value then setNodeValue('true', nstBoolean)
  else setNodeValue('false', nstBoolean);
end;

{************************************}
function TALJSONNode.GetNull: Boolean;
begin
  Result := (NodeSubType <> nstText) and
            (AlSameText(GetNodeValue, 'null'));
end;

{**************************************************}
procedure TALJSONNode.SetNull(const Value: Boolean);
begin
  if Value then setNodeValue('null', nstNull)
  else ALJSONDocError('Only "true" is allowed for setNull property');
end;

{*********************************************}
function TALJSONNode.GetJavascript: AnsiString;
begin
  if NodeSubType = nstText then ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr]);
  Result := GetNodeValue;
end;

{***********************************************************}
procedure TALJSONNode.SetJavascript(const Value: AnsiString);
begin
  setNodeValue(Value, nstJavascript);
end;

{******************************************}
function TALJSONNode.GetRegEx: TALJSONRegEx;
begin
  if NodeSubType = nstText then ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr]);
  if not ALJSONDocTryStrToRegEx(GetNodeValue, result) then ALJSONDocError('%s is not a valid regular expression', [GetNodeValue]);
end;

{********************************************************}
procedure TALJSONNode.SetRegEx(const Value: TALJSONRegEx);
begin
  setNodeValue('/' + Value.Expression + '/' + Value.Options, nstRegEx);
end;

{********************************************}
function TALJSONNode.GetBinary: TALJSONBinary;
begin
  if NodeSubType = nstText then ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr]);
  if not ALJSONDocTryStrToBinary(GetNodeValue, result.Subtype, Result.Data) then ALJSONDocError('%s is not a valid binary', [GetNodeValue]);
end;

{**********************************************************}
procedure TALJSONNode.SetBinary(const Value: TALJSONBinary);
begin
  setNodeValue('BinData(' + ALInttostr(Value.Subtype) + ', "' + ALMimeBase64EncodeStringNoCRLF(Value.Data) + '")', nstBinary);
end;

{*********************************************}
function TALJSONNode.GetBinaryData: ansiString;
var aSubtype: byte;
begin
  if NodeSubType = nstText then ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr]);
  if not ALJSONDocTryStrToBinary(GetNodeValue, aSubtype, Result) then ALJSONDocError('%s is not a valid binary', [GetNodeValue]);
end;

{***********************************************************}
procedure TALJSONNode.SetBinaryData(const Value: ansiString);
begin
  setNodeValue('BinData(0, "' + ALMimeBase64EncodeStringNoCRLF(Value) + '")', nstBinary); // 0 = Default BSON type
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
    aNodeList: TALJSONNodeList;
begin
  FDocument := Value;
  aNodeList := InternalGetChildNodes;
  if Assigned(aNodeList) then
    for I := 0 to aNodeList.Count - 1 do
      aNodeList[I].SetOwnerDocument(Value);
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
  If assigned(Value) then SetOwnerDocument(Value.OwnerDocument)
  else SetOwnerDocument(nil);
  FParentNode := Value
end;

{*******************************************************************}
{Returns the JSON that corresponds to the subtree rooted at this node.
 GetJSON returns the JSON that corresponds to this node and any child nodes it contains.}
function TALJSONNode.GetJSON: AnsiString;
begin
  SaveToJSON(result);
end;

{************************************************}
{SetJSON reload the node with the new given value }
procedure TALJSONNode.SetJSON(const Value: AnsiString);
Begin
  LoadFromJSON(Value, true{ClearChildNodes});
end;

{*******************************************************************}
{Returns the BSON that corresponds to the subtree rooted at this node.
 GetBSON returns the BSON that corresponds to this node and any child nodes it contains.}
function TALJSONNode.GetBSON: AnsiString;
begin
  SaveToBSON(result);
end;

{************************************************}
{SetBSON reload the node with the new given value }
procedure TALJSONNode.SetBSON(const Value: AnsiString);
Begin
  LoadFromBSON(Value, true{ClearChildNodes});
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

{******************************************************************************************************************************************}
function TALJSONNode.AddChild(const NodeName: AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode;
begin
  Result := ALCreateJSONNode(NodeName,NodeType);
  Try
    ChildNodes.Insert(Index, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{**************************************************************************************************************}
function TALJSONNode.AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode;
begin
  Result := AddChild('', NodeType, Index);
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

{******************************************************************************}
{Returns the JSON that corresponds to this node and any child nodes it contains.}
procedure TALJSONNode.SaveToStream(const Stream: TStream; const BSONStream: boolean = False);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _SaveToJSONStream;

  Const BufferSize: integer = 32768;
  Var NodeStack: Tstack;
      CurrentNode: TalJSONNode;
      CurrentParentNode: TalJSONNode;
      CurrentIndentStr: AnsiString;
      IndentStr: AnsiString;
      EncodeControlCharacters: Boolean;
      AddNodeSubTypeHelperFunct: Boolean;
      AutoIndentNode: Boolean;
      BufferString: AnsiString;
      BufferStringPos: Integer;
      LastWrittenChar: AnsiChar;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Procedure WriteBuffer2Stream(const buffer: ansiString; BufferLength: Integer);
    Begin
      If BufferLength > 0 then stream.Write(buffer[1],BufferLength);
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Procedure WriteStr2Buffer(const str:AnsiString);
    var l: integer;
    Begin
      L := Length(Str);
      if l = 0 then exit;
      LastWrittenChar := Str[L];
      if L >= BufferSize then begin
        WriteBuffer2Stream(BufferString,BufferStringPos);
        BufferStringPos := 0;
        WriteBuffer2Stream(str, l)
      end
      else begin
        if L + BufferStringPos > length(BufferString) then setlength(BufferString, L + BufferStringPos);
        ALMove(str[1], BufferString[BufferStringPos + 1], L);
        BufferStringPos := BufferStringPos + L;
        if BufferStringPos >= BufferSize then begin
          WriteBuffer2Stream(BufferString,BufferStringPos);
          BufferStringPos := 0;
        end;
      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Procedure WriteTextNode2Stream(aTextNode:TALJSONNode);
    var aInt32: integer;
        aInt64: system.int64;
    Begin
      with aTextNode do begin

        if not (LastWrittenChar in ['{','[']) then WriteStr2Buffer(',');

        if AutoIndentNode then WriteStr2Buffer(#13#10 + CurrentIndentStr);

        if (assigned(ParentNode)) and
           (ParentNode.NodeType <> ntArray) then begin
          if EncodeControlCharacters then WriteStr2Buffer('"'+ALJavascriptEncode(NodeName)+'":')
          else WriteStr2Buffer('"'+NodeName+'":');
        end;

        if NodeSubType = NstText then begin
          if EncodeControlCharacters then WriteStr2Buffer('"'+ALJavascriptEncode(Text)+'"')
          else WriteStr2Buffer('"'+Text+'"');
        end
        else if (NodeSubType = NstInt32) and
                (AddNodeSubTypeHelperFunct) and
                (ALTryStrToInt(NodeValue, aInt32)) then WriteStr2Buffer('NumberInt('+ALInttostr(aInt32)+')')
        else if (NodeSubType = NstInt64) and
                (AddNodeSubTypeHelperFunct) and
                (ALTryStrToInt64(NodeValue, aInt64)) then WriteStr2Buffer('NumberLong('+ALInttostr(aInt64)+')')
        else WriteStr2Buffer(NodeValue);

      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Procedure WriteStartObjectNode2Stream(aObjectNode:TALJSONNode);
    var ANodeList: TALJSONNodeList;
        aEmptyNode: Boolean;
        i: integer;
    Begin
      with aObjectNode do begin

        if not (LastWrittenChar in ['{','[']) then WriteStr2Buffer(',');

        if AutoIndentNode and (CurrentIndentStr <> '') then WriteStr2Buffer(#13#10 + CurrentIndentStr);

        if aObjectNode = self then WriteStr2Buffer('{')
        else if (assigned(ParentNode)) and
                (ParentNode.NodeType <> ntArray) then begin
          if EncodeControlCharacters then WriteStr2Buffer('"'+ALJavascriptEncode(NodeName)+'":{')
          else WriteStr2Buffer('"'+NodeName+'":{');
        end
        else WriteStr2Buffer('{');

        aEmptyNode := True;
        aNodeList := InternalGetChildNodes;
        If assigned(aNodeList) then begin
          with aNodeList do
            If count > 0 then begin
              aEmptyNode := False;
              NodeStack.Push(aObjectNode);
              For i := Count - 1 downto 0 do NodeStack.Push(Nodes[i]);
            end
        end;

        If aEmptyNode then WriteStr2Buffer('}')
        else CurrentIndentStr := CurrentIndentStr + IndentStr;

      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Procedure WriteEndObjectNode2Stream(aObjectNode:TALJSONNode);
    Begin
      if AutoIndentNode then begin
        delete(CurrentIndentStr,length(CurrentIndentStr) - length(IndentStr)+1, maxint);
        WriteStr2Buffer(#13#10 + CurrentIndentStr);
      end;
      WriteStr2Buffer('}');
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Procedure WriteStartArrayNode2Stream(aArrayNode:TALJSONNode);
    var ANodeList: TALJSONNodeList;
        aEmptyNode: Boolean;
        i: integer;
    Begin
      with aArrayNode do begin

        if not (LastWrittenChar in ['{','[']) then WriteStr2Buffer(',');

        if AutoIndentNode and (CurrentIndentStr <> '') then WriteStr2Buffer(#13#10 + CurrentIndentStr);

        if aArrayNode = self then WriteStr2Buffer('[')
        else if (assigned(ParentNode)) and
                (ParentNode.NodeType <> ntArray) then begin
          if EncodeControlCharacters then WriteStr2Buffer('"'+ALJavascriptEncode(NodeName)+'":[')
          else WriteStr2Buffer('"'+NodeName+'":[');
        end
        else WriteStr2Buffer('[');

        aEmptyNode := True;
        aNodeList := InternalGetChildNodes;
        If assigned(aNodeList) then begin
          with aNodeList do
            If count > 0 then begin
              aEmptyNode := False;
              NodeStack.Push(aArrayNode);
              For i := Count - 1 downto 0 do NodeStack.Push(Nodes[i]);
            end
        end;

        If aEmptyNode then WriteStr2Buffer(']')
        else CurrentIndentStr := CurrentIndentStr + IndentStr;

      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Procedure WriteEndArrayNode2Stream(aArrayNode:TALJSONNode);
    Begin
      if AutoIndentNode then begin
        delete(CurrentIndentStr,length(CurrentIndentStr) - length(IndentStr) + 1, maxint);
        WriteStr2Buffer(#13#10 + CurrentIndentStr);
      end;
      WriteStr2Buffer(']');
    end;

  begin
    If not (NodeType in [ntObject, ntArray]) then exit;  // normally only Object node can gave a valid json stream
                                                         // but their is some situation where the array (containing json node)
                                                         // is also usefull
    CurrentParentNode := nil;
    NodeStack := Tstack.Create;
    Try

      {init buffer string}
      Setlength(BufferString, BufferSize * 2);
      BufferStringPos := 0;
      LastWrittenChar := '{';
      EncodeControlCharacters := not (poIgnoreControlCharacters in FDocument.ParseOptions);
      AddNodeSubTypeHelperFunct := (poAddNodeSubTypeHelperFunct in FDocument.ParseOptions);
      AutoIndentNode := (doNodeAutoIndent in FDocument.Options);
      IndentStr := FDocument.NodeIndentStr;
      CurrentIndentStr := '';

      {SaveOnlyChildNode}
      NodeStack.Push(self);

      {loop on all nodes}
      While NodeStack.Count > 0 Do begin
        CurrentNode := TAlJSONNode(NodeStack.Pop);

        with CurrentNode do
          case NodeType of
            ntObject: begin
                        if currentNode = CurrentParentNode then WriteEndObjectNode2Stream(CurrentNode)
                        else WriteStartObjectNode2Stream(CurrentNode);
                      end;
            ntArray: begin
                        if currentNode = CurrentParentNode then WriteEndArrayNode2Stream(CurrentNode)
                        else WriteStartArrayNode2Stream(CurrentNode);
                     end;
            ntText: WriteTextNode2Stream(CurrentNode);
            else AlJSONDocError(cAlJSONInvalidNodeType);
          end;

        CurrentParentNode := CurrentNode.ParentNode;
      end;

      {Write the buffer}
      WriteBuffer2Stream(BufferString, BufferStringPos);

    finally
      NodeStack.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _SaveToBSONStream;

  Const BufferSize: integer = 32768;
  Var NodeStack: Tstack;
      NodeIndexStack: Tstack;
      NodeStartPosStack: Tstack;
      CurrentNode: TalJSONNode;
      CurrentParentNode: TalJSONNode;
      CurrentNodeIndex: integer;
      CurrentNodeStartPos: System.int64;
      BufferString: AnsiString;
      BufferStringPos: Integer;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Procedure WriteBuffer2Stream(const buffer: ansiString; BufferLength: Integer);
    Begin
      If BufferLength > 0 then stream.Write(buffer[1],BufferLength);
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Procedure WriteStr2Buffer(const str:AnsiString);
    var l: integer;
    Begin
      L := Length(Str);
      if l = 0 then exit;
      if L >= BufferSize then begin
        WriteBuffer2Stream(BufferString,BufferStringPos);
        BufferStringPos := 0;
        WriteBuffer2Stream(str, l);
      end
      else begin
        if L + BufferStringPos > length(BufferString) then setlength(BufferString, L + BufferStringPos);
        ALMove(str[1], BufferString[BufferStringPos + 1], L);
        BufferStringPos := BufferStringPos + L;
        if BufferStringPos >= BufferSize then begin
          WriteBuffer2Stream(BufferString,BufferStringPos);
          BufferStringPos := 0;
        end;
      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    //take care because fucking TStringStream (for exemple) do not permit
    //to write previous to the current position (it's set the size of the
    //new stream to the current position ... unbelievable!)
    Procedure WriteInt2Pos(const aInt:integer; const aPos: system.Int64);
    var aTmpPos: System.int64;
    Begin
      if aPos < Stream.position then begin
        aTmpPos := Stream.position;
        Stream.position := aPos;
        stream.Write(aInt,sizeof(aInt));
        Stream.position := aTmpPos;
      end
      else ALMove(aInt, BufferString[aPos - Stream.position + 1], sizeOf(aInt));
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Procedure WriteTextNode2Stream(aTextNode:TALJSONNode; aNodeIndex: integer);
    var aBinStr: AnsiString;
        aNodeName: AnsiString;
        aDouble: Double;
        aInt32: Integer;
        aInt64: system.Int64;
        aObjectID: TALJSONObjectID;
        aTimestamp: TALBSONTimestamp;
        aRegEx: TALJSONRegEx;
        aBinary: TALJSONBinary;
    Begin
      with aTextNode do begin

        // calculate the aNodeName
        if (assigned(ParentNode)) and
           (ParentNode.NodeType = ntArray) then aNodeName := ALInttostr(aNodeIndex)
        else aNodeName := NodeName;

        // add the nodevalue to the buffer
        case NodeSubType of

          // \x01 + name + \x00 + double
          nstFloat: begin
                      aDouble := Float;
                      setlength(aBinStr,sizeOf(aDouble));
                      ALMove(aDouble, aBinStr[1], sizeOf(aDouble));
                      WriteStr2Buffer(#$01 + aNodeName + #$00 + aBinStr);
                    end;

          // \x02 + name + \x00 + length (int32) + string + \x00
          nstText: begin
                      aInt32 := length(text) + 1 {for the trailing #0};
                      setlength(aBinStr,sizeOf(aInt32));
                      ALMove(aInt32, aBinStr[1], sizeOf(aInt32));
                      WriteStr2Buffer(#$02 + aNodeName + #$00 + aBinStr + text + #$00);
                   end;

          // \x05 + name + \x00 + int32 + subtype + (byte*)
          nstbinary: begin
                       aBinary := Binary;
                       aInt32 := length(aBinary.Data);
                       setlength(aBinStr,sizeOf(aInt32));
                       ALMove(aInt32, aBinStr[1], sizeOf(aInt32));
                       WriteStr2Buffer(#$05 + aNodeName + #$00 + aBinStr + ansiChar(aBinary.Subtype) + aBinary.Data);
                     end;

          // \x07 + name + \x00 + (byte*12)
          nstObjectID: begin
                         aObjectID := ObjectID;
                         setlength(aBinStr,sizeOf(aObjectID));
                         ALMove(aObjectID[1], aBinStr[1], sizeOf(aObjectID));
                         WriteStr2Buffer(#$07 + aNodeName + #$00 + aBinStr);
                       end;

          // \x08 + name + \x00 + \x00 => Boolean "false"
          // \x08 + name + \x00 + \x01	=> Boolean "true"
          nstBoolean: begin
                        if not bool then WriteStr2Buffer(#$08 + aNodeName + #$00 + #$00)
                        else WriteStr2Buffer(#$08 + aNodeName + #$00 + #$01);
                      end;

          // \x09 + name + \x00 + int64
          nstDateTime: begin
                         aInt64 := ALDateTimeToUnixMs(DateTime);
                         setlength(aBinStr,sizeOf(aInt64));
                         ALMove(aInt64, aBinStr[1], sizeOf(aInt64));
                         WriteStr2Buffer(#$09 + aNodeName + #$00 + aBinStr);
                       end;

          // \x11 + name + \x00 + int64
          nstTimestamp: begin
                          aTimestamp := Timestamp;
                          setlength(aBinStr,sizeOf(aInt64));
                          ALMove(aTimestamp.I64, aBinStr[1], sizeOf(aInt64));
                          WriteStr2Buffer(#$11 + aNodeName + #$00 + aBinStr);
                        end;

          // \x0A + name + \x00
          nstNull: begin
                     WriteStr2Buffer(#$0A + aNodeName + #$00);
                   end;

          // \xOB + name + \x00 + (byte*) + \x00 + (byte*) + \x00
          nstRegEx: begin
                      aRegEx := RegEx;
                      WriteStr2Buffer(#$0B + aNodeName + #$00 + aRegEx.Expression + #$00 + aRegEx.Options + #$00);
                    end;

          // \x0D + name + \x00 + length (int32) + string + \x00
          nstJavascript: begin
                           aInt32 := length(text) + 1 {for the trailing #0};
                           setlength(aBinStr,sizeOf(aInt32));
                           ALMove(aInt32, aBinStr[1], sizeOf(aInt32));
                           WriteStr2Buffer(#$0D + aNodeName + #$00 + aBinStr + text + #$00);
                         end;

          // \x10 + name + \x00 + int32
          nstInt32: begin
                      aInt32 := int32;
                      setlength(aBinStr,sizeOf(aInt32));
                      ALMove(aInt32, aBinStr[1], sizeOf(aInt32));
                      WriteStr2Buffer(#$10 + aNodeName + #$00 + aBinStr);
                    end;

          // \x12 + name + \x00 + int64
          nstInt64: begin
                      aInt64 := int64;
                      setlength(aBinStr,sizeOf(aInt64));
                      ALMove(aInt64, aBinStr[1], sizeOf(aInt64));
                      WriteStr2Buffer(#$12 + aNodeName + #$00 + aBinStr);
                    end

          else AlJSONDocError(cALJSONInvalidBSONNodeSubType);

        end;

      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Procedure WriteStartObjectNode2Stream(aObjectNode:TALJSONNode; aNodeIndex: integer);
    var ANodeList: TALJSONNodeList;
        aEmptyNode: Boolean;
        aPos: system.int64;
        i: integer;
    Begin
      with aObjectNode do begin

        if aObjectNode = self then WriteStr2Buffer(#$00+#$00+#$00+#$00)
        else if (assigned(ParentNode)) and
                (ParentNode.NodeType = ntArray) then WriteStr2Buffer(#$03 + ALInttostr(aNodeIndex) + #$00 + #$00+#$00+#$00+#$00)
        else WriteStr2Buffer(#$03 + NodeName + #$00 + #$00+#$00+#$00+#$00);

        aPos := Stream.Position + BufferStringPos - 4{length of the #$00+#$00+#$00+#$00};

        aEmptyNode := True;
        aNodeList := InternalGetChildNodes;
        If assigned(aNodeList) then begin
          with aNodeList do
            If count > 0 then begin
              aEmptyNode := False;
              NodeStack.Push(aObjectNode);
              NodeIndexStack.Push(pointer(aNodeIndex));
              NodeStartPosStack.Push(pointer(aPos));
              For i := Count - 1 downto 0 do begin
                NodeStack.Push(Nodes[i]);
                NodeIndexStack.Push(pointer(i));
                NodeStartPosStack.Push(pointer(-1));
              end;
            end
        end;

        If aEmptyNode then begin
          WriteStr2Buffer(#$00);
          WriteInt2Pos(5{length of the object},aPos);
        end;

      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Procedure WriteEndObjectNode2Stream(aObjectNode:TALJSONNode; aNodeStartPos: system.Int64);
    Begin
      WriteStr2Buffer(#$00);
      WriteInt2Pos(Stream.Position + BufferStringPos - aNodeStartPos, aNodeStartPos);
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Procedure WriteStartArrayNode2Stream(aArrayNode:TALJSONNode; aNodeIndex: integer);
    var ANodeList: TALJSONNodeList;
        aEmptyNode: Boolean;
        aPos: system.int64;
        i: integer;
    Begin
      with aArrayNode do begin

        if (assigned(ParentNode)) and
           (ParentNode.NodeType = ntArray) then WriteStr2Buffer(#$04 + ALInttostr(aNodeIndex) + #$00 + #$00+#$00+#$00+#$00)
        else WriteStr2Buffer(#$04 + NodeName + #$00 + #$00+#$00+#$00+#$00);

        aPos := Stream.Position + BufferStringPos - 4{length of the #$00+#$00+#$00+#$00};

        aEmptyNode := True;
        aNodeList := InternalGetChildNodes;
        If assigned(aNodeList) then begin
          with aNodeList do
            If count > 0 then begin
              aEmptyNode := False;
              NodeStack.Push(aArrayNode);
              NodeIndexStack.Push(pointer(aNodeIndex));
              NodeStartPosStack.Push(pointer(aPos));
              For i := Count - 1 downto 0 do begin
                NodeStack.Push(Nodes[i]);
                NodeIndexStack.Push(pointer(i));
                NodeStartPosStack.Push(pointer(-1));
              end;
            end
        end;

        If aEmptyNode then begin
          WriteStr2Buffer(#$00);
          WriteInt2Pos(5{length of the object},aPos);
        end;

      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Procedure WriteEndArrayNode2Stream(aArrayNode:TALJSONNode; aNodeStartPos: system.Int64);
    Begin
      WriteStr2Buffer(#$00);
      WriteInt2Pos(Stream.Position + BufferStringPos - aNodeStartPos, aNodeStartPos);
    end;

  begin
    If NodeType <> ntobject then exit;

    CurrentParentNode := nil;
    NodeStack := Tstack.Create;
    NodeIndexStack := Tstack.Create;
    NodeStartPosStack := Tstack.Create;
    Try

      {init buffer string}
      Setlength(BufferString, BufferSize * 2);
      BufferStringPos := 0;

      {SaveOnlyChildNode}
      NodeStack.Push(self);
      NodeIndexStack.Push(pointer(0));
      NodeStartPosStack.Push(pointer(Stream.Position));


      {loop on all nodes}
      While NodeStack.Count > 0 Do begin
        CurrentNode := TAlJSONNode(NodeStack.Pop);
        CurrentNodeIndex := integer(NodeIndexStack.Pop);
        CurrentNodeStartPos := system.int64(NodeStartPosStack.Pop);

        with CurrentNode do
          case NodeType of
            ntObject: begin
                        if currentNode = CurrentParentNode then WriteEndObjectNode2Stream(CurrentNode, CurrentNodeStartPos)
                        else WriteStartObjectNode2Stream(CurrentNode, CurrentNodeIndex);
                      end;
            ntArray: begin
                        if currentNode = CurrentParentNode then WriteEndArrayNode2Stream(CurrentNode, CurrentNodeStartPos)
                        else WriteStartArrayNode2Stream(CurrentNode, CurrentNodeIndex);
                     end;
            ntText: WriteTextNode2Stream(CurrentNode, CurrentNodeIndex);
            else AlJSONDocError(cAlJSONInvalidNodeType);
          end;

        CurrentParentNode := CurrentNode.ParentNode;
      end;

      {Write the buffer}
      WriteBuffer2Stream(BufferString, BufferStringPos);

    finally
      NodeStack.Free;
      NodeIndexStack.Free;
      NodeStartPosStack.Free;
    end;
  end;

begin
  if BSONStream then _SaveToBSONStream
  else _SaveToJSONStream;
end;

{************************************************************************************************************************************}
procedure TALJSONNode.LoadFromStream(const Stream: TStream; const BSONStream: boolean = False; Const ClearChildNodes: Boolean = True);
Begin
  If NodeType <> ntObject then ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr]);
  if ClearChildNodes then ChildNodes.Clear;
  Try
    if BSONStream then FDocument.ParseBSONStream(Stream, self)
    else FDocument.ParseJSONStream(Stream, self);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{*********************************************************************************************}
procedure TALJSONNode.SaveToFile(const AFileName: AnsiString; const BSONFile: boolean = False);
Var afileStream: TfileStream;
begin
  aFileStream := TfileStream.Create(String(AFileName),fmCreate);
  Try
    SaveToStream(aFileStream, BSONFile);
  finally
    aFileStream.Free;
  end;
end;

{**************************************************************************************************************************************}
procedure TALJSONNode.LoadFromFile(const AFileName: AnsiString; const BSONFile: boolean = False; Const ClearChildNodes: Boolean = True);
Var afileStream: TfileStream;
Begin
  aFileStream := TfileStream.Create(string(AFileName), fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromStream(aFileStream, BSONFile, ClearChildNodes);
  finally
    aFileStream.Free;
  end;
end;

{*****************************************************}
procedure TALJSONNode.SaveToJSON(var JSON: AnsiString);
Var aStringStream: TALStringStream;
begin
  aStringStream := TALStringStream.Create('');
  Try
    SaveToStream(aStringStream, false {BSONStream});
    JSON := aStringStream.DataString;
  finally
    aStringStream.Free;
  end;
end;

{*****************************************************}
procedure TALJSONNode.SaveToBSON(var BSON: AnsiString);
Var aStringStream: TALStringStream;
begin
  aStringStream := TALStringStream.Create('');
  Try
    SaveToStream(aStringStream, True {BSONStream});
    BSON := aStringStream.DataString;
  finally
    aStringStream.Free;
  end;
end;

{************************************************************************************************}
procedure TALJSONNode.LoadFromJSON(const JSON: AnsiString; Const ClearChildNodes: Boolean = True);
Var aStringStream: TALStringStream;
Begin
  aStringStream := TALStringStream.Create(JSON);
  Try
    LoadFromStream(aStringStream, False {BSONStream}, ClearChildNodes);
  finally
    aStringStream.Free;
  end;
end;

{************************************************************************************************}
procedure TALJSONNode.LoadFromBSON(const BSON: AnsiString; Const ClearChildNodes: Boolean = True);
Var aStringStream: TALStringStream;
Begin
  aStringStream := TALStringStream.Create(BSON);
  Try
    LoadFromStream(aStringStream, True {BSONStream}, ClearChildNodes);
  finally
    aStringStream.Free;
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

{**************************************************************************}
procedure TALJSONObjectNode.SetNodeSubType(const Value: TALJSONNodeSubType);
begin
  ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr])
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

{*************************************************************************}
procedure TALJSONArrayNode.SetNodeSubType(const Value: TALJSONNodeSubType);
begin
  ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr])
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
  fNodeValue := '';
end;

{**************************************************}
function TALJSONTextNode.GetNodeType: TALJSONNodeType;
begin
  Result := NtText;
end;

{**********************************************************}
function TALJSONTextNode.GetNodeSubType: TALJSONNodeSubType;
begin
  Result := fNodeSubType;
end;

{************************************************************************}
procedure TALJSONTextNode.SetNodeSubType(const Value: TALJSONNodeSubType);
begin
  if Value in [nstObject, nstArray] then ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr]);
  fNodeSubType := Value;
end;

{************************************************}
function TALJSONTextNode.GetNodeValue: ansiString;
begin
  result := fNodeValue;
end;

{*****************************************************************************************************}
procedure TALJSONTextNode.SetNodeValue(const Value: AnsiString; const NodeSubType: TALJSONNodeSubType);
begin
  fNodeSubType := NodeSubType;
  fNodeValue := Value;
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
end;

{***************************************}
{Returns the number of nodes in the list.
 GetCount is the read implementation of the Count property.}
function TALJSONNodeList.GetCount: Integer;
begin
  Result := FCount;
end;

{*************************************}
{Returns the index of a specified node.
 Call IndexOf to locate a node in the list.
 *Node is the object node to locate.
 IndexOf returns the index of the specified node, where 0 is the index of the first node, 1 is the
 index of the second node, and so on. If the specified node is not in the list, IndexOf returns -1.}
function TALJSONNodeList.IndexOf(const Node: TALJSONNode): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Node) do Inc(Result);
  if Result = FCount then Result := -1;
end;

{*************************************}
{Returns the index of a specified node.
 Call IndexOf to locate a node in the list.
 *Name is the NodeName property of the node to locate.
 IndexOf returns the index of the specified node, where 0 is the index of the first node, 1 is the
 index of the second node, and so on. If the specified node is not in the list, IndexOf returns -1.}
function TALJSONNodeList.IndexOf(const Name: AnsiString): Integer;
begin
  for Result := 0 to Count - 1 do
    if ALNodeMatches(Get(Result), Name) then Exit;
  Result := -1;
end;

{**************************************}
{Returns a specified node from the list.
 Call FindNode to access a particular node in the list.
 *NodeName is the node to access. It specifies the NodeName property of the desired node.
 FindNode returns the object of the node if it is in the list. If NodeName does not specify a node in the list,
 FindNode returns nil (Delphi) or NULL (C++).}
function TALJSONNodeList.FindNode(const NodeName: AnsiString): TALJSONNode;
var Index: Integer;
begin
  Index := IndexOf(NodeName);
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
  Result := FList^[Index];
end;

{**************************}
{$IF CompilerVersion < 18.5}
{Returns a specified node from the list.
 GetNode is the read implementation of the Nodes property.
 *IndexOrName identifies the desired node. It can be The index of the node, where 0 is the index of the first node,
  1 is the index of the second node, and so on. The NodeName property of a node in the list.
 If IndexOrName does not identify a node in the list, GetNode tries to create a new node with the name specified by
 IndexOrName. If it can’t create the new node, GetNode raises an exception.}
function TALJSONNodeList.GetNode(const IndexOrName: OleVariant): TALJSONNode;
begin
  if VarIsOrdinal(IndexOrName) then Result := GetNodeByIndex(IndexOrName)
  else Result := GetNodeByName(AnsiString(IndexOrName));
end;
{$IFEND}

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
procedure TALJSONNodeList.QuickSort(L, R: Integer; XCompare: TALJSONNodeListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while XCompare(Self, I, P) < 0 do Inc(I);
      while XCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        Exchange(I, J);
        if P = I then P := J
        else if P = J then P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, XCompare);
    L := I;
  until I >= R;
end;

{**********************************************************************}
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
    if Index < FCount then ALMove(FList^[Index],
                                  FList^[Index + 1],
                                  (FCount - Index) * SizeOf(Pointer));
  end;
  FList^[index] := Node;
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
  Dec(FCount);
  if Index < FCount then ALMove(FList^[Index + 1],
                                FList^[Index],
                                (FCount - Index) * SizeOf(Pointer));
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

{************************************************************}
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
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

{***********************************************************}
{Removes a specified object from the list without freeing it.
 Call Extract to remove an object from the list without freeing the object itself.
 After an object is removed, all the objects that follow it are moved up in index position and Count is decremented.}
function TALJSONNodeList.Extract(const index: integer): TALJSONNode;
begin
  Result := Get(index);
  Result.SetParentNode(nil);
  FList^[index] := nil;
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
  if (NewCapacity < FCount) or (NewCapacity > cALJSONNodeMaxListSize) then ALJSONDocError(CALJSONListCapacityError, [NewCapacity]);
  if NewCapacity <> FCapacity then begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

{****************************************************}
procedure TALJSONNodeList.SetCount(NewCount: Integer);
var I: Integer;
begin
  if (NewCount < 0) or (NewCount > cALJSONNodeMaxListSize) then ALJSONDocError(CALJSONListCountError, [NewCount]);
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else for I := FCount - 1 downto NewCount do Delete(I);
  FCount := NewCount;
end;

{**************************************}
{$IF CompilerVersion >= 23} {Delphi XE2}
Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
                           const aFormatSettings: TALFormatSettings;
                           const aPath: AnsiString;
                           aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false');

var aALJsonDocument: TALJsonDocument;
    aContainChilds: boolean;
begin
  aALJsonDocument := TALJsonDocument.Create(aFormatSettings);
  try

    aALJsonDocument.onParseText := procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const str: AnsiString; const NodeSubType: TALJSONNodeSubType)
                                   begin
                                     if (NodeSubType = nstBoolean) and (ALSameText(str, 'true')) then aLst.Add(aPath + Path + aLst.NameValueSeparator + aTrueStr)
                                     else if (NodeSubType = nstBoolean) and (ALSameText(str, 'false')) then aLst.Add(aPath + Path + aLst.NameValueSeparator + aFalseStr)
                                     else if (NodeSubType = nstnull) then aLst.Add(aPath + Path + aLst.NameValueSeparator + aNullStr)
                                     else aLst.Add(aPath+ Path + aLst.NameValueSeparator + str);
                                     aContainChilds := True;
                                   end;

    aALJsonDocument.onParseStartObject := procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
                                          begin
                                            aContainChilds := False;
                                          end;

    aALJsonDocument.onParseEndObject := procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
                                        begin
                                          if (not aContainChilds) and (aPath + Path <> ''{Path = '' mean it's the root object}) then aLst.Add(aPath+ Path + aLst.NameValueSeparator + '{}');
                                          aContainChilds := True;
                                        end;

    aALJsonDocument.onParseStartArray := procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
                                         begin
                                           aContainChilds := False;
                                         end;

    aALJsonDocument.onParseEndArray := procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
                                       begin
                                         if not aContainChilds then aLst.Add(aPath+ Path + aLst.NameValueSeparator + '[]');
                                         aContainChilds := True;
                                       end;

    aALJsonDocument.LoadFromJSON(AJsonStr, true{saxMode});
  finally
    aALJsonDocument.Free;
  end;
end;
{$ifend}

{**************************************}
{$IF CompilerVersion >= 23} {Delphi XE2}
Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
                           const aFormatSettings: TALFormatSettings;
                           aLst: TALStrings;
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
{$ifend}

{******************************************************}
Procedure ALJSONToTStrings(const aJsonNode: TAlJsonNode;
                           Const aPath: AnsiString;
                           aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false');
var aTmpPath: AnsiString;
    i: integer;
begin
  if aJsonNode.ChildNodes.Count > 0 then begin
    for I := 0 to aJsonNode.ChildNodes.Count - 1 do begin

      if aJsonNode.NodeType = ntArray then aTmpPath := aPath + '[' + alinttostr(i) + ']'
      else begin
        if aJsonNode.ChildNodes[i].NodeName = '' then raise Exception.Create('Nodename can not be empty');
        aTmpPath := aPath + alIfThen(aPath <> '', '.', '') + aJsonNode.ChildNodes[i].NodeName;
      end;

      case aJsonNode.ChildNodes[i].NodeType of

        ntObject: ALJSONToTStrings(aJsonNode.ChildNodes[i],
                                   aTmpPath,
                                   aLst,
                                   aNullStr,
                                   aTrueStr,
                                   aFalseStr);

        ntArray: ALJSONToTStrings(aJsonNode.ChildNodes[i],
                                  aTmpPath,
                                  aLst,
                                  aNullStr,
                                  aTrueStr,
                                  aFalseStr);

        ntText: begin
                  if (aJsonNode.ChildNodes[i].NodeSubType = nstBoolean) then begin
                    if aJsonNode.ChildNodes[i].Bool then                       aLst.Add(aTmpPath + aLst.NameValueSeparator + aTrueStr)
                    else                                                       aLst.Add(aTmpPath + aLst.NameValueSeparator + aFalseStr);
                  end
                  else if (aJsonNode.ChildNodes[i].NodeSubType = nstnull) then aLst.Add(aTmpPath + aLst.NameValueSeparator + aNullStr)
                  else                                                         aLst.Add(aTmpPath + aLst.NameValueSeparator + aJsonNode.ChildNodes[i].Text);
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
                           aLst: TALStrings;
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
                           aJsonNode: TALJSONNode;
                           Const aPath: AnsiString = '';
                           Const aNameToLowerCase: boolean = false;
                           Const aNullStr: AnsiString = 'null');

var aIndex: Integer;
    aNames:  TALStringList;
    aLowerName: AnsiString;
    aCurrJsonNode, aTmpJsonNode: TALJSONNode;
    i, j: integer;

begin

  // create list of the part of name,
  // from "aggregated_data.properties.types[3].translations.usa" =>
  //   aggregated_data
  //   properties
  //   types
  //   [3]
  //   translations
  //   usa
  aNames := TALStringList.Create;
  try

    //init aNames.linebreak
    aNames.LineBreak := '.';

    // scroll the aLst
    for i := 0 to aLst.Count - 1 do begin

      //if it's contain path
      if (aPath = '') or
         (alposExIgnoreCase(aPath + '.',aLst.Names[i]) = 1) then begin

        // path.aggregated_data.properties.types[3].translations.usa =>
        //   aggregated_data
        //   properties
        //   types
        //   [3]
        //   translations
        //   usa
        if (aPath <> '') then aNames.Text := ALStringReplace(ALStringReplace(aLst.Names[i],
                                                                             aPath + '.',
                                                                             '',
                                                                             [rfIgnoreCase]),
                                                             '[',
                                                             '.[',
                                                             [rfReplaceAll])
        else aNames.Text := ALStringReplace(aLst.Names[i],
                                            '[',
                                            '.[',
                                            [rfReplaceAll]);

        //loop on all the name
        aCurrJsonNode := aJsonNode;
        for j := 0 to aNames.Count - 1 do begin

          //if we are in array
          if aCurrJsonNode.NodeType = ntArray then begin
            if (length(aNames[j]) <= 2) or
               (aNames[j][1] <> '[') or
               (aNames[j][length(aNames[j])] <> ']') or
               (not ALTryStrToInt(ALCopyStr(aNames[j], 2, Length(aNames[j]) - 2), aIndex)) then raise EALException.CreateFmt('Wrong path: "%s"', [aLst.Names[i]]);
            while aIndex > aCurrJsonNode.ChildNodes.Count - 1 do begin
              if j = aNames.Count - 1 then aCurrJsonNode.AddChild(ntText)
              else if (aNames[j+1] <> '') and
                      (aNames[j+1][1] = '[') then aCurrJsonNode.AddChild(ntarray)
              else aCurrJsonNode.AddChild(ntObject);
            end;
            aCurrJsonNode := aCurrJsonNode.ChildNodes[aIndex];
          end

          //if we are not in array
          else begin
            aLowerName := alifThen(aNameToLowerCase, allowercase(aNames[j]), aNames[j]);
            aTmpJsonNode := aCurrJsonNode.ChildNodes.FindNode(aLowerName);
            if not assigned(aTmpJsonNode) then begin
              if j = aNames.Count - 1 then aCurrJsonNode := aCurrJsonNode.AddChild(aLowerName, ntText)
              else if (aNames[j+1] <> '') and
                      (aNames[j+1][1] = '[') then aCurrJsonNode := aCurrJsonNode.AddChild(aLowerName, ntarray)
              else aCurrJsonNode := aCurrJsonNode.AddChild(aLowerName, ntObject);
            end
            else aCurrJsonNode := aTmpJsonNode;
          end;

          //set the value
          if J = aNames.Count - 1 then begin
            if aLst.ValueFromIndex[i] = aNullStr then aCurrJsonNode.Null := true
            else aCurrJsonNode.Text := aLst.ValueFromIndex[i];
          end;

        end;

      end;

    end;

  finally
    aNames.Free;
  end;

end;

{*******************************************}
Procedure ALJSONToXML(aJSONNode: TALJsonNode;
                      aXMLNode: TALXmlNode;
                      aXMLElementNameForJSONArrayEntries: TalStrings; // JSONArrayNodeName=XMLElementName | ex: transactions=transaction
                                                                      //                                  |     features=feature
                      const aDefaultXMLElementNameForJSONArrayEntries: AnsiString = 'rec');
var aNodeName: AnsiString;
    i: integer;
begin
  for I := 0 to aJSONNode.ChildNodes.Count - 1 do begin

    if (aJSONNode.NodeType = ntarray) then begin
      if assigned(aXMLElementNameForJSONArrayEntries) then aNodeName := aXMLElementNameForJSONArrayEntries.Values[aJSONNode.NodeName]
      else aNodeName := '';
      if aNodeName = '' then aNodeName := aDefaultXMLElementNameForJSONArrayEntries;
    end
    else aNodeName := aJSONNode.ChildNodes[i].NodeName;

    if aJSONNode.ChildNodes[i].NodeType = ntText then aXMLNode.AddChild(aNodeName).text := aJSONNode.ChildNodes[i].text
    else ALJsonToXML(aJSONNode.ChildNodes[i], aXMLNode.AddChild(aNodeName));

  end;
end;

{*******************************************}
Procedure ALJSONToXML(aJSONNode: TALJsonNode;
                      aXMLNode: TALXmlNode;
                      const aDefaultXMLElementNameForJSONArrayEntries: AnsiString = 'rec');
begin
  ALJSONToXML(aJSONNode,
              aXMLNode,
              nil,
              aDefaultXMLElementNameForJSONArrayEntries);
end;

{**************************************************************************}
function ALJsonEncodeWithNodeSubTypeHelperFunction(const aValue: AnsiString;
                                                   aNodeSubType: TALJSONNodeSubType;
                                                   const aFormatSettings: TALFormatSettings): AnsiString;
var aStr: AnsiString;
begin
  case aNodeSubType of
    nstFloat:      begin
                     if @aFormatSettings <> @ALDefaultFormatSettings then result := ALFloatToStr(ALStrToFloat(aValue, aFormatSettings), ALDefaultFormatSettings)
                     else result := aValue;
                   end;
    nstText:       result := '"'+ALJavascriptEncode(aValue)+'"';
    nstBinary:     result := 'BinData(0, "' + ALMimeBase64EncodeStringNoCRLF(aValue) + '")';
    nstObjectID:   begin
                     setlength(aStr, length(aValue) * 2);
                     BintoHex(@aValue[1],pansiChar(aStr),length(aValue));
                     result := 'ObjectId("'+ALLowerCase(aStr)+'")';
                   end;
    nstBoolean:    if ALStrToBool(aValue) then result := 'true'
                   else result := 'false';
    nstDateTime:   begin
                     if ALPos('ISODate', aValue) <= 0 then result := ALFormatDateTime('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', ALStrToDateTime(aValue, aFormatSettings), ALDefaultFormatSettings)
                     else result := aValue;
                   end;
    nstJavascript: result := aValue;
    nstInt32:      result := 'NumberInt(' + ALIntToStr(ALstrToInt(aValue)) + ')';
    nstInt64:      result := 'NumberLong(' + ALIntToStr(ALstrToInt64(aValue)) + ')';
    nstNull:       result := 'null';
    nstObject:     raise Exception.Create('Unsupported Node SubType');
    nstArray:      raise Exception.Create('Unsupported Node SubType');
    nstRegEx:      raise Exception.Create('Unsupported Node SubType');
    nstTimestamp:  raise Exception.Create('Unsupported Node SubType');
    else raise Exception.Create('Unknown Node SubType');
  end;
end;

initialization
  ALGetLocaleFormatSettings(1033{en-US}, _cALJsonISODateFormatSettings);
  _cALJsonISODateFormatSettings.DateSeparator := '-';
  _cALJsonISODateFormatSettings.TimeSeparator := ':';
  _cALJsonISODateFormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  _cALJsonISODateFormatSettings.ShortTimeFormat := 'hh:nn:ss.zzz';


end.
