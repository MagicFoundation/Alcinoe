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
                                           nstBinary: Writeln(Path + '=' + 'BinData('+inttostr(Args[1].VInteger)+', "'+ansiString(ALMimeBase64EncodeStringNoCRLF(ansiString(Args[0].VAnsiString)))+'")');
                                           nstJavascript: Writeln(Path + '=' + ansiString(Args[0].VAnsiString));
                                           nstInt32: Writeln(Path + '=' + 'NumberInt('+inttostr(Args[0].VInteger)+')');
                                           nstTimestamp: Writeln(Path + '=' + 'Timestamp('+inttostr(int64(cardinal(Args[0].VInteger)))+', '+inttostr(int64(cardinal(Args[1].VInteger)))+')');
                                           nstInt64: Writeln(Path + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
                                         end;
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

  TALBSONTimestamp = packed record      // Special internal type used by MongoDB replication and sharding.
    case integer of                     // First 4 bytes are an increment, second 4 are a timestamp. Setting the
      0: (I64: Int64);                  // timestamp to 0 has special semantics.
      1: (W1:  LongWord;
          W2:  LongWord);
  end;

  {$IF CompilerVersion >= 23} {Delphi XE2}
  TAlJSONParseDocument = reference to procedure (Sender: TObject);
  TAlJSONParseTextEvent = reference to procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType);
  TAlJSONParseObjectEvent = reference to procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString);
  TAlJSONParseArrayEvent = reference to procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString);
  {$ELSE}
  TAlJSONParseDocument = procedure (Sender: TObject) of object;
  TAlJSONParseTextEvent = procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType) of object;
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
                        poSkipNodeSubTypeHelper4Int);  // don't use helper functions NumberLong() to handle 64-bit integers and NumberInt()
                                                       // to handle 32-bit integers
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
    function GetNodeValueStr: ansiString; virtual;
    function GetNodeValueInt64: int64; virtual;
    function GetNodeValueWithNodeSubTypeHelper(const SkipNodeSubTypeHelper4Int: boolean = False): AnsiString;
    procedure SetNodeValue(const Value: AnsiString; const NodeSubType: TALJSONNodeSubType); overload; virtual;
    procedure SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType); overload; virtual;
    procedure SetNodeValue(const StrValue: AnsiString; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType); overload; virtual;
    function GetText: AnsiString;
    procedure SetText(const Value: AnsiString);
    function GetFloat: Double;
    procedure SetFloat(const Value: Double);
    function GetDateTime: TDateTime;
    procedure SetDateTime(const Value: TDateTime);
    function GetTimestamp: TALBSONTimestamp;
    procedure SetTimestamp(const Value: TALBSONTimestamp);
    function GetObjectID: AnsiString;
    procedure SetObjectID(const Value: AnsiString);
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
    function GetRegEx: ansiString;
    procedure SetRegEx(const Pattern: ansiString); overload;
    procedure SetRegEx(const Pattern: ansiString; const Options: TALPerlRegExOptions); overload;
    function GetRegExOptions: TALPerlRegExOptions;
    procedure SetRegExOptions(const Value: TALPerlRegExOptions);
    function GetBinary: AnsiString;
    procedure SetBinary(const Data: AnsiString); overload;
    procedure SetBinary(const Data: AnsiString; const Subtype: byte); overload;
    function GetBinarySubType: byte;
    procedure SetBinarySubType(const Subtype: byte);
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
    function AddChild(const NodeName: AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode; overload;
    function AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode; overload;
    function NextSibling: TALJSONNode;
    function PreviousSibling: TALJSONNode;
    procedure SaveToJSONStream(const Stream: TStream);
    procedure SaveToJSONFile(const FileName: AnsiString);
    procedure SaveToJSONString(var Str: AnsiString);
    procedure SaveToBSONStream(const Stream: TStream);
    procedure SaveToBSONFile(const FileName: AnsiString);
    procedure SaveToBSONString(var Str: AnsiString);
    procedure LoadFromJSONString(const Str: AnsiString; Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSONStream(Stream: TStream; Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSONFile(const FileName: AnsiString; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONString(const Str: AnsiString; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONStream(Stream: TStream; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONFile(const FileName: AnsiString; Const ClearChildNodes: Boolean = True);
    property ChildNodes: TALJSONNodeList read GetChildNodes write SetChildNodes;
    property HasChildNodes: Boolean read GetHasChildNodes;
    property NodeName: AnsiString read fNodeName write fNodeName;
    property NodeType: TALJSONNodeType read GetNodeType;
    property NodeValue: AnsiString read GetNodeValueStr; // same as text property but without formating
    property NodeSubType: TALJSONNodeSubType read GetNodeSubType;
    property OwnerDocument: TALJSONDocument read GetOwnerDocument Write SetOwnerDocument;
    property ParentNode: TALJSONNode read GetParentNode;
    property Text: AnsiString read GetText write SetText;
    property int32: integer read GetInt32 write SetInt32;
    property int64: int64 read Getint64 write Setint64;
    property Float: Double read GetFloat write SetFloat;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property Timestamp: TALBSONTimestamp read GetTimestamp write SetTimestamp; // Use only by MongoDB, do not use it, use DateTime instead !
    property ObjectID: AnsiString read GetObjectID write SetObjectID;
    property Bool: Boolean read GetBool write SetBool;
    property Null: Boolean read GetNull write SetNull;
    property Javascript: AnsiString read GetJavascript write SetJavascript;
    property RegEx: AnsiString read GetRegEx write SetRegEx;
    property RegExOptions: TALPerlRegExOptions read GetRegExOptions write SetRegExOptions;
    property Binary: ansiString read GetBinary write SetBinary;
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

  TALJSONTextNodeValueDefined = set of (nvStr, nvInt64);

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
    Procedure ParseJSON(RawJSONStream: TStream;
                        const RawJSONString: AnsiString;
                        ContainerNode: TALJSONNode);
    Procedure ParseBSON(RawBSONStream: TStream;
                        const RawBSONString: AnsiString;
                        ContainerNode: TALJSONNode);
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
    function AddChild(const NodeName: AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode;
    function CreateNode(const NodeName: AnsiString; NodeType: TALJSONNodeType): TALJSONNode;
    function IsEmptyDoc: Boolean;
    procedure LoadFromJSONString(const Str: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSONStream(Stream: TStream; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSONFile(const FileName: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONString(const Str: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONStream(Stream: TStream; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONFile(const FileName: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure SaveToJSONStream(const Stream: TStream);
    procedure SaveToJSONFile(const FileName: AnsiString);
    procedure SaveToJSONString(var Str: AnsiString);
    procedure SaveToBSONStream(const Stream: TStream);
    procedure SaveToBSONFile(const FileName: AnsiString);
    procedure SaveToBSONString(var Str: AnsiString);
    property ChildNodes: TALJSONNodeList read GetChildNodes;
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

function ALJSONTryStrToRegEx(const S: AnsiString; out RegEx: AnsiString; out RegExOptions: TALPerlRegExOptions): boolean;
function ALJSONTryStrTobinary(const S: AnsiString; out Data: AnsiString; out Subtype: byte): boolean;
function ALJSONTryStrToDateTime(const S: AnsiString; out Value: TDateTime): Boolean;
function ALJSONTryStrToObjectID(const S: AnsiString; out Value: ansiString): Boolean;
function ALJSONTryStrToTimestamp(const S: AnsiString; out Value: TALBSONTimestamp): Boolean;
function ALJSONTryStrToInt32(const S: AnsiString; out Value: integer): Boolean;
function ALJSONTryStrToInt64(const S: AnsiString; out Value: int64): Boolean;

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
     ALquickSortList,
     AlHTML,
     ALMime,
     ALMisc;

var _cALJsonISODateFormatSettings: TALFormatSettings;

{***********************************************************************************************************************}
function ALJSONTryStrToRegEx(const S: AnsiString; out RegEx: AnsiString; out RegExOptions: TALPerlRegExOptions): boolean;
var P1: integer;
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
      RegEx := ALCopyStr(S, 2, P1 - 2);
      RegExOptions := [];

      // loop on all the options characters
      // to check if they are allowed.
      for i := P1 + 1 to Length(S) do
        case s[i] of
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
var aInt: integer;
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
  if not ALTryStrToInt(ALcopyStr(S,P1,P2-P1), aInt) then Exit;
  subtype := aInt;

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
  Data := ALMimeBase64DecodeString(ALCopyStr(s, P1, P2-P1));

  // set the result
  result := true;

end;

{**********************************************************************************}
function ALJSONTryStrToDateTime(const S: AnsiString; out Value: TDateTime): Boolean;
var aQuoteChar: ansiChar;
    aTmpStr: AnsiString;
    aTmpLn: integer;
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
  aQuoteChar := S[P1]; // "
  inc(p1); // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
           //              ^P1
  P2 := P1;
  while (P1 <= ln) and (S[P1] <> aQuoteChar) do inc(P1);
  if (P1 > ln) then exit; // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                          //                                      ^P1
  dec(P1);
  if S[P1] <> 'Z' then exit;
  aTmpStr := AlcopyStr(S,P2,P1-P2); // yyyy-mm-ddThh:nn:ss.zzz

  P2 := 1;
  aTmpLn := length(aTmpStr);
  while (P2 <= aTmpLn) and (aTmpStr[P2] <> 'T') do inc(P2);
  if P2 > aTmpLn then exit;
  aTmpStr[P2] := ' '; // yyyy-mm-dd hh:nn:ss.zzz

  result := ALTryStrToDateTime(aTmpStr, Value, _cALJsonISODateFormatSettings);
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
var aObjectIDhex: AnsiString;
    aQuoteChar: ansiChar;
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
  aQuoteChar := S[P1]; // "
  inc(p1); // ObjectId ( "507f1f77bcf86cd799439011" )
           //             ^P1
  if (P1 + 23{(length(aObjectIDhex)) - 1} > ln) then exit;
  aObjectIDhex := ALcopyStr(S,P1,24{length(aObjectIDhex)}); // 507f1f77bcf86cd799439011
  inc(P1, 24{length(aObjectIDhex)}); // ObjectId ( "507f1f77bcf86cd799439011" )
                                     //                                     ^P1
  if (P1 > ln) or (S[P1] <> aQuoteChar) then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                                   //                                     ^P1
  inc(p1);  // ObjectId ( "507f1f77bcf86cd799439011" )
            //                                      ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 <> ln) or (S[P1] <> ')') then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                             //                                       ^P1
  //convert 507f1f77bcf86cd799439011 to binary
  Setlength(Value,12);
  result := HexToBin(PansiChar(aObjectIDhex),@Value[1],length(value)) = length(Value);

end;

{******************************************************************************************}
function ALJSONTryStrToTimestamp(const S: AnsiString; out Value: TALBSONTimestamp): Boolean;
var P1, P2:        integer;
    aArgs:         AnsiString;
    aArg1:         integer;
    aArg2:         integer;
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
  aArgs := ALCopyStr(S, P1+1, P2 - P1-1); // 0, 0

  // take arguments of function Timestamp
  P1 := ALPos(',', aArgs);
  if not ALTryStrToInt(ALTrim(ALCopyStr(aArgs, 1,      P1 - 1)), aArg1) then Exit;
  if not ALTryStrToInt(ALTrim(ALCopyStr(aArgs, P1 + 1, maxint)), aArg2) then Exit;

  // build result
  result := true;
  Value.W1 := aArg1; // higher 4 bytes - increment
  Value.W2 := aArg2; // lower  4 bytes - timestamp
end;

{*****************************************************************************}
function ALJSONTryStrToInt32(const S: AnsiString; out Value: integer): Boolean;
var aTmpStr: AnsiString;
    aQuoteChar: ansiChar;
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
    aTmpStr := ALcopyStr(S,P1,P2-P1); // 12391293
    P1 := P2; // NumberInt ( 12391293 )
              //                     ^P2

    while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> ln) or (S[P1] <> ')') then exit; // NumberInt ( "12391293" )
                                               //                        ^P1
  end
  else begin // NumberInt ( "12391293" )
             //             ^P1

    aQuoteChar := S[P1]; // "
    inc(p1); // NumberInt ( "12391293" )
             //              ^P1
    P2 := P1;
    while P2 <= Ln do
      if S[P2] = aQuoteChar then break
      else inc(P2);
    if P2 > ln then exit;
    aTmpStr := ALcopyStr(S,P1,P2-P1); // 12391293
    P1 := P2 + 1; // NumberInt ( "12391293" )
                  //                       ^P1
    while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> ln) or (S[P1] <> ')') then exit; // NumberInt ( "12391293" )
                                               //                        ^P1
  end;

  //convert 12391293 to integer
  result := ALTryStrToInt(aTmpStr, Value);

end;

{***************************************************************************}
function ALJSONTryStrToInt64(const S: AnsiString; out Value: int64): Boolean;
var aTmpStr: AnsiString;
    aQuoteChar: ansiChar;
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
    aTmpStr := ALcopyStr(S,P1,P2-P1); // 12391293
    P1 := P2; // NumberLong ( 12391293 )
              //                      ^P2

    while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> ln) or (S[P1] <> ')') then exit; // NumberLong ( "12391293" )
                                               //                         ^P1
  end
  else begin // NumberLong ( "12391293" )
             //              ^P1

    aQuoteChar := S[P1]; // "
    inc(p1); // NumberLong ( "12391293" )
             //               ^P1
    P2 := P1;
    while P2 <= Ln do
      if S[P2] = aQuoteChar then break
      else inc(P2);
    if P2 > ln then exit;
    aTmpStr := ALcopyStr(S,P1,P2-P1); // 12391293
    P1 := P2 + 1; // NumberLong ( "12391293" )
                  //                        ^P1
    while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> ln) or (S[P1] <> ')') then exit; // NumberLong ( "12391293" )
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
  ReleaseDoc;
  inherited;
end;

{*****************************************************************************}
//will create all the nodevalue to be sure that multiple thread can safely read
//at the same time the jsondocument
procedure TALJSONDocument.MultiThreadPrepare;

  procedure _doMultiThreadPrepare(aNode: TALJsonNode);
  var i: integer;
  begin

    if aNode.NodeType = ntText then begin

      case aNode.NodeSubType of
        nstFloat,
        nstBoolean,
        nstDateTime,
        nstNull,
        nstInt32,
        nstTimestamp,
        nstInt64: aNode.GetNodeValueStr;
        //nstText: can not be retrieve from int64
        //nstObject: can not be retrieve from int64
        //nstArray: can not be retrieve from int64
        //nstBinary: only the binarysubtype is store in int64
        //nstObjectID: can not be retrieve from int64
        //nstRegEx: only the regex option is store in the int64
        //nstJavascript: can not be retrieve from int64
      end;

      case aNode.NodeSubType of
        nstFloat,
        nstBoolean,
        nstDateTime,
        nstNull,
        nstInt32,
        nstTimestamp,
        nstInt64: aNode.GetNodeValueInt64;
        //nstText: can not be retrieve from int64
        //nstObject: can not be retrieve from int64
        //nstArray: can not be retrieve from int64
        //nstBinary: only the binarysubtype is store in int64
        //nstObjectID: can not be retrieve from int64
        //nstRegEx: only the regex option is store in the int64
        //nstJavascript: can not be retrieve from int64
      end;

    end

    else begin
      For i := 0 to aNode.ChildNodes.Count - 1 do
        _doMultiThreadPrepare(aNode.ChildNodes[i]);
    end;

  end;

begin
  _doMultiThreadPrepare(node);
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
Procedure TALJSONDocument.ParseJSON(RawJSONStream: TStream;
                                    const RawJSONString: AnsiString;
                                    ContainerNode: TALJSONNode);

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
    if size <> 0 then Inc(Size, sizeof(LB));
    for I := 1 to NamePaths.Count - 1 do Inc(Size, Length(NamePaths.Names[I]) + sizeof(LB));
    SetLength(Result, Size);
    P := 1;
    for I := 1 to NamePaths.Count - 1 do begin
      S := NamePaths.Names[I];
      L := Length(S);
      if L <> 0 then begin
        ALMove(pointer(S)^, Pbyte(Result)[P-1], L);
        Inc(P, L);
      end;
      L := sizeof(LB);
      if ((i <> NamePaths.Count - 1) or
          (ExtraItems <> '')) and
         (((NotSaxMode) and (TALJSONNode(NamePaths.Objects[I]).nodetype <> ntarray)) or
          ((not NotSaxMode) and (TALJSONNodeType(NamePaths.Objects[I]) <> ntarray))) then begin
        ALMove(LB, Pbyte(Result)[P-1], L);
        Inc(P, L);
      end;
    end;
    if ExtraItems <> '' then begin
      L := length(ExtraItems);
      ALMove(pointer(ExtraItems)^, Pbyte(Result)[P-1], L);
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
    setlength(S1,sizeOf(Integer));
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
  var aNode: TALJsonNode;
      aInt64: Int64;
  begin
    if ALJSONTryStrToInt64(value, aInt64) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
        else aNode := CreateNode(Name, nttext);
        try
          aNode.SetInt64(aInt64);
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;
        _DoParseText(index, Name, [aint64], nstInt64)
      end
      else begin
        _DoParseText(index, Name, [aint64], nstInt64)
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createInt32Node(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var aNode: TALJsonNode;
      aint32: Int32;
  begin
    if ALJSONTryStrToInt32(value, aInt32) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
        else aNode := CreateNode(Name, nttext);
        try
          aNode.Setint32(aInt32);
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;
        _DoParseText(index, Name, [aint32], nstInt32)
      end
      else begin
        _DoParseText(index, Name, [aint32], nstInt32)
      end
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createTextNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var aNode: TALJsonNode;
  begin
    result := true;
    if NotSaxMode then begin
      if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
      else aNode := CreateNode(Name, nttext);
      try
        aNode.Settext(value);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
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
  var aNode: TALJsonNode;
      aDouble: Double;
  begin
    if ALTryStrToFloat(value, aDouble, ALDefaultFormatSettings) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
        else aNode := CreateNode(Name, nttext);
        try
          aNode.SetFloat(ADouble);
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;
        _DoParseText(index, Name, [aDouble], nstFloat)
      end
      else begin
        _DoParseText(index, Name, [aDouble], nstFloat)
      end
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createBinaryNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var aNode: TALJsonNode;
      aBinSubtype: byte;
      aBinData: ansiString;
  begin
    if ALJSONTryStrToBinary(value, aBinData, aBinSubtype) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
        else aNode := CreateNode(Name, nttext);
        try
          aNode.setbinary(aBinData, aBinSubtype);
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;
        _DoParseText(index, Name, [aBinData, aBinSubtype], nstBinary);
      end
      else begin
        _DoParseText(index, Name, [aBinData, aBinSubtype], nstBinary);
      end
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createObjectIDNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var aNode: TALJsonNode;
      aObjectID: AnsiString;
  begin
    if ALJSONTryStrToObjectID(value, aObjectID) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
        else aNode := CreateNode(Name, nttext);
        try
          aNode.SetObjectID(aObjectID);
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;
        _DoParseText(index, Name, [aObjectID], nstObjectID)
      end
      else begin
        _DoParseText(index, Name, [aObjectID], nstObjectID)
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createBooleanNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var aNode: TALJsonNode;
      aBool: Boolean;
  begin
    if value = 'true' then aBool := true
    else if value = 'false' then aBool := false
    else begin
      result := False;
      exit;
    end;
    result := true;
    if NotSaxMode then begin
      if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
      else aNode := CreateNode(Name, nttext);
      try
        aNode.Setbool(aBool);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;
      _DoParseText(index, Name, [aBool], nstBoolean);
    end
    else begin
      _DoParseText(index, Name, [aBool], nstBoolean);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createDateTimeNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var aNode: TALJsonNode;
      aDateTime: TdateTime;
  begin
    if ALJSONTryStrToDateTime(value, aDateTime) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
        else aNode := CreateNode(Name, nttext);
        try
          aNode.Setdatetime(aDateTime);
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;
        _DoParseText(index, Name, [aDateTime], nstDateTime);
      end
      else begin
        _DoParseText(index, Name, [aDateTime], nstDateTime);
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createTimestampNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var aNode: TALJsonNode;
      aTimestamp: TALBSONTimestamp;
  begin
    if ALJSONTryStrToTimestamp(value, aTimestamp) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
        else aNode := CreateNode(Name, nttext);
        try
          aNode.SetTimestamp(aTimestamp);
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;
        _DoParseText(index, Name, [aTimestamp.W1, aTimestamp.W2], nstTimeStamp);
      end
      else begin
        _DoParseText(index, Name, [aTimestamp.W1, aTimestamp.W2], nstTimeStamp);
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createnullNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var aNode: TALJsonNode;
  begin
    if value = 'null' then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
        else aNode := CreateNode(Name, nttext);
        try
          aNode.Setnull(true);
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
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
  var aNode: TALJsonNode;
      aRegEx: ansiString;
      aRegExOptions: TALPerlRegExOptions;
  begin
    if ALJSONTryStrToRegEx(value, aRegEx, aRegExOptions) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
        else aNode := CreateNode(Name, nttext);
        try
          aNode.SetRegEx(aRegEx, aRegExOptions);
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;
        _DoParseText(index, Name, [aRegEx, Byte(aRegExOptions)], nstRegEx)
      end
      else begin
        _DoParseText(index, Name, [aRegEx, Byte(aRegExOptions)], nstRegEx)
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createJavascriptNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var aNode: TALJsonNode;
  begin
    result := true;
    if NotSaxMode then begin
      if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
      else aNode := CreateNode(Name, nttext);
      try
        aNode.SetJavascript(value);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
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
    if _createFloatNode(index, Name, Value) then exit;
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
  Var aNode: TALJsonNode;
      aNodeType: TALJSONNodeType;
      aQuoteChar: AnsiChar;
      aNameValueSeparator: ansiChar;
      aInSingleQuote: boolean;
      aInDoubleQuote: boolean;
      aInSquareBracket: integer;
      aInRoundBracket: integer;
      aInCurlyBracket: integer;
      P1, P2: Integer;
      c: ansiChar;
  Begin

    {$IFDEF undef}{$REGION 'init current char (c)'}{$ENDIF}
    c := Buffer[BufferPos];
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'end Object/Array'}{$ENDIF}
    // ... } ....
    // ... ] ....
    if c in ['}',']'] then begin // ... } ...
                                 //     ^BufferPos

      //Reset the CurrIndex
      CurrIndex := -1;

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
        if assigned(ObjectPaths) then aNode := TALJSONNode(ObjectPaths.Objects[ObjectPaths.Count - 1])
        else aNode := TALJSONNode(NamePaths.Objects[NamePaths.Count - 1]);

        //if anode <> workingNode aie aie aie
        if (aNode <> WorkingNode) then ALJSONDocError(CALJSONParseError);

        //calculate anodeTypeInt
        aNodeType := aNode.NodeType;
        if not (aNodeType in [ntObject, ntarray]) then ALJSONDocError(cALBSONParseError);

        //check that the end object/array correspond to the aNodeType
        if ((c = '}') and
            (aNodeType <> ntObject)) or
           ((c = ']') and
            (aNodeType <> ntarray)) then ALJSONDocError(CALJSONParseError);

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
        aNodeType := TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]);
        if not (anodeType in [ntObject,ntarray]) then ALJSONDocError(cALBSONParseError);

        //check that the end object/array correspond to the aNodeType
        if ((c = '}') and
            (aNodeType <> ntObject)) or
           ((c = ']') and
            (aNodeType <> ntarray)) then ALJSONDocError(CALJSONParseError);

        //update CurrIndex if WorkingNode.NodeType = ntArray
        if (Namepaths.Count >= 2) and
           (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 2]) = ntarray) then CurrIndex := _extractLastIndexFromNamePath + 1;

      end;

      //call the DoParseEndObject/array event
      if Assigned(fonParseEndObject) then begin
        if anodeType = ntObject then _DoParseEndObject
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
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Begin Object/Array Without NAME'}{$ENDIF}
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
        if c = '{' then aNode := CreateNode('', ntObject)
        else aNode := CreateNode('', ntarray);
        try
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;

        //set that the current working node will be now the new node newly created
        WorkingNode := aNode;

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
        if c = '{' then aNodeType := ntObject
        else aNodeType := ntArray;
        _AddItemToNamePath(CurrIndex, '', pointer(aNodeType));

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
    if c in ['"',''''] then begin  // ... " ...
                                   //     ^BufferPos
      aQuoteChar := c; // "
      P1 := BufferPos + 1; // ... "...\"..."
                           //      ^P1
      If P1 + 1 > BufferLength then ExpandBuffer(P1);
      While P1 <= BufferLength do begin

       c := Buffer[P1];

       If (c = '\') and
          (P1 < BufferLength) and
          (Buffer[P1 + 1] = aQuoteChar) then inc(p1, 2) // ... "...\"..."
                                                        //         ^^^P1
       else if c = aQuoteChar then begin
         ALCopyStr(Buffer,CurrName,BufferPos + 1,P1-BufferPos - 1);
         if DecodeJSONReferences then ALUTF8JavascriptDecodeV(CurrName); // ..."...
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

      While (BufferPos <= BufferLength) or ExpandBuffer do begin
        If Buffer[BufferPos] <= ' ' then inc(bufferPos)
        else break;
      end;
      if BufferPos > BufferLength then ALJSONDocError(CALJSONParseError);

      P1 := BufferPos; // ... new Date('Dec 03, 1924'), ....
                       //     ^P1
      While (P1 <= BufferLength) or ExpandBuffer(P1) do begin

        c := Buffer[P1];

        if (not aInSingleQuote) and
           (not aInDoubleQuote) and
           (aInSquareBracket = 0) and
           (aInRoundBracket = 0) and
           (aInCurlyBracket = 0) and
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
             (Buffer[P1 - 1] <> '\') then aInDoubleQuote := (not aInDoubleQuote) and (not aInSingleQuote);
        end
        else if (c = '''') then begin
          if (P1 <= 1) or
             (Buffer[P1 - 1] <> '\') then aInSingleQuote := (not aInSingleQuote) and (not aInDoubleQuote)
        end
        else if (not aInSingleQuote) and
                (not aInDoubleQuote) then begin
          if (c = '[') then inc(aInSquareBracket)
          else if (c = ']') then dec(aInSquareBracket)
          else if (c = '(') then inc(aInRoundBracket)
          else if (c = ')') then dec(aInRoundBracket)
          else if (c = '}') then inc(aInCurlyBracket)
          else if (c = '{') then dec(aInCurlyBracket);
        end;

        inc(P1); // ... new Date('Dec 03, 1924'), ....
                 //     ^^^^^^^^^^^^^^^^^^^^^^^^^P1

      end;
      if P1 > BufferLength then ALJSONDocError(CALJSONParseError);
      BufferPos := P1; // ... new Date('Dec 03, 1924'), ....
                       //                             ^BufferPos

    end;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'extract the name value separator part'}{$ENDIF}
    aNameValueSeparator := #0;
    While (BufferPos <= BufferLength) or ExpandBuffer do begin
      If Buffer[BufferPos] <= ' ' then inc(BufferPos)
      else begin
        aNameValueSeparator := Buffer[BufferPos];
        break;
      end;
    end;
    if BufferPos > BufferLength then ALJSONDocError(CALJSONParseError);  // .... : ....
                                                                         //      ^BufferPos
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'if aNameValueSeparator is absent then it is just a value'}{$ENDIF}
    if aNameValueSeparator <> ':' then begin

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
      _createNode(CurrIndex,'',CurrName,aQuoteChar in ['"','''']);

      //increase the CurrIndex
      inc(CurrIndex);

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'remove the blank space between the name valueeparator and the value'}{$ENDIF}
    inc(BufferPos); // ... : ....
                    //      ^BufferPos
    While (BufferPos <= BufferLength) or ExpandBuffer do begin
      If Buffer[BufferPos] <= ' ' then inc(BufferPos)
      else break;
    end;
    if BufferPos > BufferLength then ALJSONDocError(CALJSONParseError); // .... " ....
                                                                        //      ^BufferPos
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'init current char (c)'}{$ENDIF}
    c := Buffer[BufferPos];
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'if the value is an object/array'}{$ENDIF}
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
        if c = '{' then aNode := CreateNode(CurrName, ntObject)
        else aNode := CreateNode(CurrName, ntarray);
        try
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;

        //set that the current working node will be now the new node newly created
        WorkingNode := aNode;

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
        if c = '{' then aNodeType := ntObject
        else aNodeType := ntArray;
        _AddItemToNamePath(-1, CurrName, pointer(aNodeType));

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
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'if the value is a quoted string'}{$ENDIF}
    // name : "value"
    // name : 'value'
    aQuoteChar := #0;
    if c in ['"',''''] then begin  // ... " ...
                                   //     ^BufferPos

      aQuoteChar := c; // "
      P1 := BufferPos + 1; // ... "...\"..."
                           //      ^P1
      If P1 + 1 > BufferLength then ExpandBuffer(P1);
      While P1 <= BufferLength do begin

       c := Buffer[P1];

       If (c = '\') and
          (P1 < BufferLength) and
          (Buffer[P1 + 1] = aQuoteChar) then inc(p1, 2) // ... "...\"..."
                                                        //         ^^^P1
       else if c = aQuoteChar then begin
         ALCopyStr(Buffer,currValue,BufferPos + 1,P1-BufferPos - 1);
         if DecodeJSONReferences then ALUTF8JavascriptDecodeV(currValue); // ..."...
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

      While (BufferPos <= BufferLength) or ExpandBuffer do begin
        If Buffer[BufferPos] <= ' ' then inc(bufferPos)
        else break;
      end;
      if BufferPos > BufferLength then ALJSONDocError(CALJSONParseError);

      P1 := BufferPos; // ... new Date('Dec 03, 1924'), ....
                       //     ^P1
      While (P1 <= BufferLength) or ExpandBuffer(P1) do begin

        c := Buffer[P1];

        if (not aInSingleQuote) and
           (not aInDoubleQuote) and
           (aInSquareBracket = 0) and
           (aInRoundBracket = 0) and
           (aInCurlyBracket = 0) and
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
             (Buffer[P1 - 1] <> '\') then aInDoubleQuote := (not aInDoubleQuote) and (not aInSingleQuote);
        end
        else if (c = '''') then begin
          if (P1 <= 1) or
             (Buffer[P1 - 1] <> '\') then aInSingleQuote := (not aInSingleQuote) and (not aInDoubleQuote)
        end
        else if (not aInSingleQuote) and
                (not aInDoubleQuote) then begin
          if (c = '[') then inc(aInSquareBracket)
          else if (c = ']') then dec(aInSquareBracket)
          else if (c = '(') then inc(aInRoundBracket)
          else if (c = ')') then dec(aInRoundBracket)
          else if (c = '}') then inc(aInCurlyBracket)
          else if (c = '{') then dec(aInCurlyBracket);
        end;

        inc(P1); // ... new Date('Dec 03, 1924'), ....
                 //     ^^^^^^^^^^^^^^^^^^^^^^^^^P1

      end;
      if P1 > BufferLength then ALJSONDocError(CALJSONParseError);
      BufferPos := P1; // ... new Date('Dec 03, 1924'), ....
                       //                             ^BufferPos


    end;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'create the named text node'}{$ENDIF}

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
    _createNode(currIndex,CurrName,CurrValue,aQuoteChar in ['"','''']);

    {$IFDEF undef}{$ENDREGION}{$ENDIF}

  end;

var c: ansiChar;

Begin

  //
  // NOTE: the childNodes of the ContainerNode
  //       must have been cleared by the calling function!
  //
  // NOTE: ContainerNode must have fDocument assigned
  //
  // NOTE: ContainerNode must be ntobject or nil (sax mode)
  //

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

    //skip utf8 bom
    //ExpandBuffer;
    //if AlUTF8DetectBOM(PansiChar(Buffer),length(Buffer)) then BufferPos := 4;
    //useless now

    //add first node in ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ObjectPaths.AddObject(-1, WorkingNode)
    else begin
      if NotSaxMode then _AddNameItemToNamePath('', WorkingNode)
      else _AddNameItemToNamePath('', pointer(ntObject));
    end;

    //skip the first {
    While (BufferPos <= BufferLength) or ExpandBuffer do begin
      c := Buffer[BufferPos];
      If c <= ' ' then inc(bufferPos)
      else begin
        if c <> '{' then ALJSONDocError(cALJSONParseError);
        inc(bufferPos);
        break;
      end;
    end;

    //analyze all the nodes
    While (BufferPos <= BufferLength) or ExpandBuffer do begin
      c := Buffer[BufferPos];
      If (c <= ' ') or (c = ',') then inc(bufferPos)
      else AnalyzeNode;
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
procedure TALJSONDocument.ParseBSON(RawBSONStream: TStream;
                                    const RawBSONString: AnsiString;
                                    ContainerNode: TALJSONNode);

Const BufferSize: integer = 8192;

Var Buffer: AnsiString;
    BufferLength: Integer;
    BufferPos: Integer;
    CurrName: AnsiString;
    NotSaxMode: Boolean;
    WorkingNode: TALJSONNode;
    NamePaths: TALStringList;
    ObjectPaths: TObjectList;

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
    if size <> 0 then Inc(Size, sizeof(LB));
    for I := 1 to NamePaths.Count - 1 do Inc(Size, Length(NamePaths[I]) + sizeof(LB));
    SetLength(Result, Size);
    P := 1;
    for I := 1 to NamePaths.Count - 1 do begin
      S := NamePaths[I];
      L := Length(S);
      if L <> 0 then begin
        ALMove(pointer(S)^, Pbyte(Result)[P-1], L);
        Inc(P, L);
      end;
      L := sizeof(LB);
      if ((i <> NamePaths.Count - 1) or
          (ExtraItems <> '')) and
         (((NotSaxMode) and (TALJSONNode(NamePaths.Objects[I]).nodetype <> ntarray)) or
          ((not NotSaxMode) and (TALJSONNodeType(NamePaths.Objects[I]) <> ntarray))) then begin
        ALMove(LB, Pbyte(Result)[P-1], L);
        Inc(P, L);
      end;
    end;
    if ExtraItems <> '' then begin
      L := length(ExtraItems);
      ALMove(pointer(ExtraItems)^, Pbyte(Result)[P-1], L);
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
  var aNode: TALJsonNode;
      aInt64: Int64;
  begin
    if BufferPos > BufferLength - sizeof(aInt64) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(aInt64) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], aInt64, sizeof(aInt64));
    BufferPos := BufferPos + sizeof(aInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
      else aNode := CreateNode(Name, nttext);
      try
        aNode.SetInt64(aInt64);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;
      _DoParseText(Name, [aint64], NodeSubType)
    end
    else begin
      _DoParseText(Name, [aint64], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createInt32Node(const name: AnsiString;
                             const NodeSubType: TALJSONNodeSubType);
  var aNode: TALJsonNode;
      aint32: Int32;
  begin
    if BufferPos > BufferLength - sizeof(aint32) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(aint32) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], aint32, sizeof(aint32));
    BufferPos := BufferPos + sizeof(aint32);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
      else aNode := CreateNode(Name, nttext);
      try
        aNode.Setint32(aInt32);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;
      _DoParseText(Name, [aint32], NodeSubType)
    end
    else begin
      _DoParseText(Name, [aint32], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createTextNode(const name: AnsiString;
                            const NodeSubType: TALJSONNodeSubType);
  var aNode: TALJsonNode;
      aInt32: Int32;
      aText: ansiString;
  begin
    if BufferPos > BufferLength - sizeof(aInt32) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(aInt32) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], aInt32, sizeof(aInt32));
    BufferPos := BufferPos + sizeof(aInt32);
    while (BufferPos + aInt32 - 1 > BufferLength) do
      if not ExpandBuffer then ALJSONDocError(cALBSONParseError);
    ALCopyStr(Buffer,aText,BufferPos,aInt32 - 1{for the trailing #0});
    BufferPos := BufferPos + aInt32;

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
      else aNode := CreateNode(Name, nttext);
      try
        aNode.Settext(aText);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;
      _DoParseText(Name, [aText], NodeSubType)
    end
    else begin
      _DoParseText(Name, [aText], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createFloatNode(const name: AnsiString;
                             const NodeSubType: TALJSONNodeSubType);
  var aNode: TALJsonNode;
      aDouble: Double;
  begin
    if BufferPos > BufferLength - sizeof(Double) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(Double) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(pbyte(Buffer)[BufferPos-1], aDouble, sizeof(Double));
    BufferPos := BufferPos + sizeof(Double);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
      else aNode := CreateNode(Name, nttext);
      try
        aNode.SetFloat(ADouble);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;
      _DoParseText(Name, [aDouble], NodeSubType)
    end
    else begin
      _DoParseText(Name, [aDouble], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createBinaryNode(const name: AnsiString;
                              const NodeSubType: TALJSONNodeSubType);
  var aNode: TALJsonNode;
      aInt32: Int32;
      aBinSubtype: byte;
      aBinData: ansiString;
  begin
    //Get size
    if BufferPos > BufferLength - sizeof(aInt32) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(aInt32) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], aInt32, sizeof(aInt32));
    BufferPos := BufferPos + sizeof(aInt32);

    //Get the subtype
    if BufferPos > BufferLength then begin
      ExpandBuffer;
      if BufferPos > BufferLength then ALJSONDocError(cALBSONParseError);
    end;
    aBinSubtype := Byte(Buffer[BufferPos]);
    BufferPos := BufferPos + 1;

    //Get the data
    while (BufferPos + aInt32 - 1 > BufferLength) do
      if not ExpandBuffer then ALJSONDocError(cALBSONParseError);
    ALCopyStr(Buffer,aBinData,BufferPos,aInt32);
    BufferPos := BufferPos + aInt32;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
      else aNode := CreateNode(Name, nttext);
      try
        aNode.setbinary(aBinData, aBinSubtype);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;
      _DoParseText(Name, [aBinData, aBinSubtype], NodeSubType);
    end
    else begin
      _DoParseText(Name, [aBinData, aBinSubtype], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createObjectIDNode(const name: AnsiString;
                                const NodeSubType: TALJSONNodeSubType);
  var aNode: TALJsonNode;
      aObjectID: AnsiString;
  begin
    if BufferPos > BufferLength - length(aObjectID) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - length(aObjectID) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    Setlength(aObjectID, 12); // ObjectId is a 12-byte BSON type
    ALMove(Pbyte(Buffer)[BufferPos-1], pbyte(aObjectID)[0], length(aObjectID)); // pbyte(aObjectID)[0] to not have a jump in uniqueString (aObjectID is already unique thanks to Setlength)
    BufferPos := BufferPos + length(aObjectID);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
      else aNode := CreateNode(Name, nttext);
      try
        aNode.SetObjectID(aObjectID);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;
      _DoParseText(Name, [aObjectID], NodeSubType)
    end
    else begin
      _DoParseText(Name, [aObjectID], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createBooleanNode(const name: AnsiString;
                               const NodeSubType: TALJSONNodeSubType);
  var aNode: TALJsonNode;
      aBool: Boolean;
  begin
    if BufferPos > BufferLength then begin
      ExpandBuffer;
      if BufferPos > BufferLength then ALJSONDocError(cALBSONParseError);
    end;
    if Buffer[BufferPos] = #$00 then aBool := False
    else if Buffer[BufferPos] = #$01 then aBool := true
    else begin
      ALJSONDocError(cALBSONParseError);
      aBool := False; // to hide a warning;
    end;
    BufferPos := BufferPos + 1;

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
      else aNode := CreateNode(Name, nttext);
      try
        aNode.Setbool(aBool);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;
      _DoParseText(Name, [aBool], NodeSubType);
    end
    else begin
      _DoParseText(Name, [aBool], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createDateTimeNode(const name: AnsiString;
                                const NodeSubType: TALJSONNodeSubType);
  var aNode: TALJsonNode;
      aDateTime: TdateTime;
      aInt64: Int64;
  begin
    if BufferPos > BufferLength - sizeof(aInt64) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(aInt64) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], aInt64, sizeof(aInt64));
    aDateTime := ALUnixMsToDateTime(aInt64);
    BufferPos := BufferPos + sizeof(aInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
      else aNode := CreateNode(Name, nttext);
      try
        aNode.Setdatetime(aDateTime);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;
      _DoParseText(Name, [aDateTime], NodeSubType);
    end
    else begin
      _DoParseText(Name, [aDateTime], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createTimestampNode(const name: AnsiString;
                                 const NodeSubType: TALJSONNodeSubType);
  var aNode: TALJsonNode;
      aTimestamp: TALBSONTimestamp;
      aInt64: Int64;
  begin
    if BufferPos > BufferLength - sizeof(aInt64) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(aInt64) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], aInt64, sizeof(aInt64));
    aTimestamp.I64 := aInt64;
    BufferPos := BufferPos + sizeof(aInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
      else aNode := CreateNode(Name, nttext);
      try
        aNode.SetTimestamp(aTimestamp);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;
      _DoParseText(Name, [aTimestamp.W1, aTimestamp.W2], NodeSubType);
    end
    else begin
      _DoParseText(Name, [aTimestamp.W1, aTimestamp.W2], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createnullNode(const name: AnsiString;
                            const NodeSubType: TALJSONNodeSubType);
  var aNode: TALJsonNode;
  begin
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
      else aNode := CreateNode(Name, nttext);
      try
        aNode.Setnull(true);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
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
  var aNode: TALJsonNode;
      aRegEx: ansiString;
      aRegExOptions: TALPerlRegExOptions;
      P1: integer;
  begin
    //Get pattern
    P1 := BufferPos;
    While (P1 <= BufferLength) or ExpandBuffer(P1) do begin
      If Buffer[P1] <> #$00 then inc(P1)
      else begin
        aRegEx := AlCopyStr(Buffer, BufferPos, P1 - BufferPos);
        break;
      end;
    end;
    if P1 > BufferLength then ALJSONDocError(cALBSONParseError);
    BufferPos := P1 + 1;
    if BufferPos > BufferLength then ExpandBuffer;

    //Get options
    aRegExOptions := [];
    While (BufferPos <= BufferLength) or ExpandBuffer do begin
      case Buffer[BufferPos] of
        'i': aRegExOptions := aRegExOptions + [preCaseLess];
        'm': aRegExOptions := aRegExOptions + [preMultiLine];
        'x': aRegExOptions := aRegExOptions + [preExtended];
        'l':;
        's': aRegExOptions := aRegExOptions + [preSingleLine];
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
      if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
      else aNode := CreateNode(Name, nttext);
      try
        aNode.SetRegEx(aRegEx, aRegExOptions);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;
      _DoParseText(Name, [aRegEx, Byte(aRegExOptions)], NodeSubType)
    end
    else begin
      _DoParseText(Name, [aRegEx, Byte(aRegExOptions)], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createJavascriptNode(const name: AnsiString;
                                  const NodeSubType: TALJSONNodeSubType);
  var aNode: TALJsonNode;
      aJavascript: ansiString;
      aInt32: Int32;
  begin
    if BufferPos > BufferLength - sizeof(aInt32) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(aInt32) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], aInt32, sizeof(aInt32));
    BufferPos := BufferPos + sizeof(aInt32);
    while (BufferPos + aInt32 - 1 > BufferLength) do
      if not ExpandBuffer then ALJSONDocError(cALBSONParseError);
    ALCopyStr(Buffer,aJavascript,BufferPos,aInt32 - 1{for the trailing #0});
    BufferPos := BufferPos + aInt32;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then aNode := CreateNode('', nttext)
      else aNode := CreateNode(Name, nttext);
      try
        aNode.SetJavascript(aJavascript);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;
      _DoParseText(Name, [aJavascript], NodeSubType);
    end
    else begin
      _DoParseText(Name, [aJavascript], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeNode;
  Var aNode: TALJsonNode;
      aNodeType: TALJSONNodeType;
      aNodeSubType: TALJSONNodeSubType;
      P1: Integer;
      c: ansiChar;
  Begin

    {$IFDEF undef}{$REGION 'init current char (c)'}{$ENDIF}
    c := Buffer[BufferPos];
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'End Object/Array'}{$ENDIF}
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
        if assigned(ObjectPaths) then aNode := TALJSONNode(ObjectPaths[ObjectPaths.Count - 1])
        else aNode := TALJSONNode(NamePaths.Objects[NamePaths.Count - 1]);

        //if anode <> workingNode aie aie aie
        if (aNode <> WorkingNode) then ALJSONDocError(cALBSONParseError);

        //calculate anodeTypeInt
        aNodeType := aNode.NodeType;
        if not (aNodeType in [ntObject, ntarray]) then ALJSONDocError(cALBSONParseError);

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
        aNodeType := TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]);
        if not (anodeType in [ntObject,ntarray]) then ALJSONDocError(cALBSONParseError);

      end;

      //call the DoParseEndObject/array event
      if Assigned(fonParseEndObject) then begin
        if anodeType = ntObject then _DoParseEndObject
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
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Get the node sub type'}{$ENDIF}
    aNodeSubType := nstText; // to hide fucking warning
    case c of
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
    BufferPos := BufferPos + 1;
    If BufferPos > BufferLength then ExpandBuffer;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Get the node name'}{$ENDIF}
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
        if aNodeSubType = nstObject then begin
          if WorkingNode.nodetype=ntarray then aNode := CreateNode('', ntObject)
          else aNode := CreateNode(CurrName, ntObject);
        end
        else begin
          if WorkingNode.nodetype=ntarray then aNode := CreateNode('', ntarray)
          else aNode := CreateNode(CurrName, ntarray);
        end;
        try
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;

        //set that the current working node will be now the new node newly created
        WorkingNode := aNode;

        //update the path
        if assigned(ObjectPaths) then ObjectPaths.Add(WorkingNode)
        else _AddItemToNamePath(CurrName, WorkingNode);

      end

      //if we are in sax mode
      else begin

        //update the path
        if aNodeSubType = nstObject then aNodeType := ntObject
        else aNodeType := ntArray;
        _AddItemToNamePath(CurrName, pointer(aNodeType));

      end;

      //call the DoParseStartObject/array event
      if aNodeSubType = nstObject then begin
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
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'create the node'}{$ENDIF}
    case aNodeSubType of
      // \x01 + name + \x00 + double
      nstFloat: _createFloatNode(CurrName, aNodeSubType);

      // \x02 + name + \x00 + length (int32) + string + \x00
      nstText: _createTextNode(CurrName, aNodeSubType);

      // \x05 + name + \x00 + int32 + subtype + (byte*)
      nstbinary: _createBinaryNode(CurrName, aNodeSubType);

      // \x07 + name + \x00 + (byte*12)
      nstObjectID: _createObjectIDNode(CurrName, aNodeSubType);

      // \x08 + name + \x00 + \x00 => Boolean "false"
      // \x08 + name + \x00 + \x01	=> Boolean "true"
      nstBoolean: _createBooleanNode(CurrName, aNodeSubType);

      // \x09 + name + \x00 + int64
      nstDateTime: _createDateTimeNode(CurrName, aNodeSubType);

      // \x11 + name + \x00 + int64
      nstTimestamp: _createTimestampNode(CurrName, aNodeSubType);

      // \x0A + name + \x00
      nstnull: _createNullNode(CurrName, aNodeSubType);

      // \x0B + name + \x00 + (byte*) + \x00 + (byte*) + \x00
      nstRegEx: _createRegExNode(CurrName, aNodeSubType);

      // \x0D + name + \x00 + length (int32) + string + \x00
      nstJavascript: _createJavascriptNode(CurrName, aNodeSubType);

      // \x10 + name + \x00 + int32
      nstint32: _createInt32Node(CurrName, aNodeSubType);

      // \x12 + name + \x00 + int64
      nstint64: _createInt64Node(CurrName, aNodeSubType);

      else ALJSONDocError(cALBSONParseError);
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
    ObjectPaths := TObjectList.Create(false{OwnsObjects});
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
procedure TALJSONDocument.LoadFromJSONStream(Stream: TStream; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
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

{***********************************************************************************************************************************}
procedure TALJSONDocument.LoadFromBSONStream(Stream: TStream; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
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

{***********************************************}
{Indicates whether this node has any child nodes}
function TALJSONNode.GetHasChildNodes: Boolean;
Var aNodeList: TALJSONNodeList;
begin
  aNodeList := InternalGetChildNodes;
  Result := assigned(aNodeList) and (aNodeList.Count > 0);
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
function TALJSONNode.GetNodeValueWithNodeSubTypeHelper(const SkipNodeSubTypeHelper4Int: boolean = False): AnsiString;

  procedure _GetObjectID;
  begin
    result := 'ObjectId("'+ALBinToHex(ObjectID)+'")';
  end;

  procedure _GetBinary;
  begin
    result := 'BinData('+alinttostr(BinarySubType)+', "'+ALMimeBase64EncodeStringNoCRLF(Binary)+'")';
  end;

  procedure _GetDateTime;
  begin
    result := ALFormatDateTime('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', DateTime, ALDefaultFormatSettings)
  end;

  procedure _Getint32;
  begin
    result := 'NumberInt(' + text + ')'
  end;

  procedure _Getint64;
  begin
    result := 'NumberLong(' + text + ')';
  end;

  procedure _GetRegEx;
  var aRegExOptions: TALPerlRegExOptions;
      aRegExOptionsStr: ansiString;
  begin
    aRegExOptionsStr := '';
    aRegExOptions := RegExOptions;
    if preCaseLess in aRegExOptions then aRegExOptionsStr := aRegExOptionsStr + 'i';
    if preMultiLine in aRegExOptions then aRegExOptionsStr := aRegExOptionsStr +'m';
    if preExtended in aRegExOptions then aRegExOptionsStr := aRegExOptionsStr +'x';
    //'l':;
    if preSingleLine in aRegExOptions then aRegExOptionsStr := aRegExOptionsStr + 's';
    //'u':;
    result := '/'+regex+'/' + aRegExOptionsStr;
  end;

  procedure _GetTimestamp;
  begin
    result := 'Timestamp('+alinttostr(GetTimeStamp.W1)+', '+alinttostr(GetTimeStamp.W2)+')';
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
    nstInt32:      if SkipNodeSubTypeHelper4Int then result := GetNodeValueStr
                   else _Getint32;
    nstInt64:      if SkipNodeSubTypeHelper4Int then result := GetNodeValueStr
                   else _Getint64;
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

{*********************************************************}
procedure TALJSONNode.SetObjectID(const Value: AnsiString);
begin
  if length(Value) <> 12 then AlJSONDocError('ObjectID must have 12 bytes');
  setNodeValue(Value, nstObjectID);
end;

{*************************************}
function TALJSONNode.GetInt32: Integer;
var aDouble: Double;
    aint64: system.int64;
begin
  case NodeSubType of
    nstFloat: begin
                PInt64(@aDouble)^ := GetNodeValueInt64;
                aInt64 := trunc(aDouble);
                if (aInt64 <> aDouble) or
                   (aInt64 > system.int32.MaxValue) or
                   (aInt64 < system.int32.MinValue) then AlJSONDocError(cALJSONInvalidBSONNodeSubType);
                result := aint64;
              end;
    nstInt32: begin
                aInt64 := GetNodeValueInt64;
                if (aInt64 > system.int32.MaxValue) or
                   (aInt64 < system.int32.MinValue) then AlJSONDocError(cALJSONInvalidBSONNodeSubType);
                result := aInt64;
              end;
    nstInt64: Result := GetNodeValueInt64;
    else begin
      AlJSONDocError(cALJSONInvalidBSONNodeSubType);
      result := 0; // to hide a warning;
    end;
  end;
end;

{***************************************************}
procedure TALJSONNode.SetInt32(const Value: Integer);
begin
  setNodeValue(Value, nstInt32);
end;

{***********************************}
function TALJSONNode.GetInt64: Int64;
var aDouble: Double;
begin
  case NodeSubType of
    nstFloat: begin
                PInt64(@aDouble)^ := GetNodeValueInt64;
                result := trunc(aDouble);
                if result <> aDouble then AlJSONDocError(cALJSONInvalidBSONNodeSubType);
              end;
    nstInt32,
    nstInt64: Result := GetNodeValueInt64;
    else begin
      AlJSONDocError(cALJSONInvalidBSONNodeSubType);
      result := 0; // to hide a warning;
    end;
  end;
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

{*****************************************************}
procedure TALJSONNode.SaveToJson(const Stream: TStream;
                                 Var buffer: ansiString);

Const BufferSize: integer = 8192;

Var NodeStack: Tstack;
    CurrentNode: TalJSONNode;
    CurrentParentNode: TalJSONNode;
    CurrentIndentStr: AnsiString;
    IndentStr: AnsiString;
    EncodeControlCharacters: Boolean;
    SkipNodeSubTypeHelper4Int: boolean;
    AutoIndentNode: Boolean;
    BufferPos: Integer;
    LastWrittenChar: AnsiChar;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteBuffer2Stream(const buffer: ansiString; BufferLength: Integer);
  Begin
    if assigned(Stream) then begin
      If BufferLength > 0 then stream.Write(pointer(buffer)^,BufferLength);
      BufferPos := 0;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _Write2Buffer(const Source; Count: {$if CompilerVersion >= 23}{Delphi XE2}NativeInt{$ELSE}Integer{$IFEND});
  Begin
    if Count = 0 then exit;
    if Count + BufferPos > length(Buffer) then setlength(Buffer, Count + BufferPos + BufferSize);
    ALMove(Source, pbyte(Buffer)[BufferPos], Count);
    BufferPos := BufferPos + Count;
    if BufferPos >= 32768 then _WriteBuffer2Stream(Buffer,BufferPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStr2Buffer(const str:AnsiString);
  var l: integer;
  Begin
    L := Length(Str);
    if l = 0 then exit;
    LastWrittenChar := Str[L];
    _Write2Buffer(pointer(str)^,l);
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

      if NodeSubType = NstText then begin
        if EncodeControlCharacters then begin
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
      else _WriteStr2Buffer(GetNodeValueWithNodeSubTypeHelper(SkipNodeSubTypeHelper4Int));

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartObjectNode2Buffer(aObjectNode:TALJSONNode);
  var ANodeList: TALJSONNodeList;
      aEmptyNode: Boolean;
      i: integer;
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

      If aEmptyNode then _WriteStr2Buffer('}')
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
  var ANodeList: TALJSONNodeList;
      aEmptyNode: Boolean;
      i: integer;
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

      If aEmptyNode then _WriteStr2Buffer(']')
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
  NodeStack := Tstack.Create;
  Try

    {init buffer string}
    Setlength(Buffer, BufferSize); // will make buffer uniquestring
    BufferPos := 0;
    LastWrittenChar := '{';
    EncodeControlCharacters := not (poIgnoreControlCharacters in FDocument.ParseOptions);
    SkipNodeSubTypeHelper4Int := poSkipNodeSubTypeHelper4Int in FDocument.ParseOptions;
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
Var afileStream: TfileStream;
begin
  aFileStream := TfileStream.Create(String(FileName),fmCreate);
  Try
    SaveToJSONStream(aFileStream);
  finally
    aFileStream.Free;
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

Var NodeStack: Tstack;
    NodeIndexStack: TALintegerList;
    NodeStartPosStack: TALInt64List;
    CurrentNode: TalJSONNode;
    CurrentParentNode: TalJSONNode;
    CurrentNodeIndex: integer;
    CurrentNodeStartPos: System.int64;
    BufferPos: {$if CompilerVersion >= 23}{Delphi XE2}NativeInt{$ELSE}Integer{$IFEND};
    StreamPos: system.int64;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteBuffer2Stream(const buffer: ansiString; BufferLength: Integer);
  Begin
    if assigned(Stream) then begin
      If BufferLength > 0 then stream.Write(pointer(buffer)^,BufferLength);
      BufferPos := 0;
      StreamPos := stream.Position;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _Write2Buffer(const Source; Count: {$if CompilerVersion >= 23}{Delphi XE2}NativeInt{$ELSE}Integer{$IFEND});
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
      stream.Write(aInt,sizeof(aInt));
      Stream.position := StreamPos;
    end
    else ALMove(aInt, Buffer[aPos - StreamPos + 1], sizeOf(aInt));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x01 + name + \x00 + double
  Procedure _WriteFloatValue2Buffer(aTextNode:TALJSONNode);
  var aDouble: Double;
  begin
    aDouble := aTextNode.Float;
    _Write2Buffer(aDouble, sizeOf(aDouble));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x02 + name + \x00 + length (int32) + string + \x00
  Procedure _WriteTextValue2Buffer(aTextNode:TALJSONNode);
  var aInt32: system.int32;
      aText: ansiString;
  begin
    aText := aTextNode.Text;
    aInt32 := length(aText) + 1 {for the trailing #0};
    _Write2Buffer(aInt32, sizeOf(aInt32));
    _WriteStr2Buffer(aText);
    _WriteStr2Buffer(#$00);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x05 + name + \x00 + int32 + subtype + (byte*)
  Procedure _WriteBinaryValue2Buffer(aTextNode:TALJSONNode);
  var aInt32: system.int32;
      aBinary: ansiString;
      aBinarySubType: Byte;
  begin
    aBinary := aTextNode.binary;
    aBinarySubType := aTextNode.BinarySubType;
    aInt32 := length(aBinary);
    _Write2Buffer(aInt32, sizeOf(aInt32));
    _Write2Buffer(aBinarySubType, sizeOF(aBinarySubType));
    _WriteStr2Buffer(aBinary);
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
  var aInt64: system.Int64;
  begin
    aInt64 := ALDateTimeToUnixMs(aTextNode.DateTime);
    _Write2Buffer(aInt64, sizeOf(aInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x11 + name + \x00 + int64
  Procedure _WriteTimestampValue2Buffer(aTextNode:TALJSONNode);
  var aInt64: system.Int64;
  begin
    aInt64 := aTextNode.Timestamp.I64;
    _Write2Buffer(aInt64, sizeOf(aInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \xOB + name + \x00 + (byte*) + \x00 + (byte*) + \x00
  Procedure _WriteRegExValue2Buffer(aTextNode:TALJSONNode);
  var aRegExOptions: TALPerlRegExOptions;
      aRegExOptionsStr: ansiString;
  begin
    aRegExOptionsStr := '';
    aRegExOptions := aTextNode.RegExOptions;
    if preCaseLess in aRegExOptions then aRegExOptionsStr := aRegExOptionsStr + 'i';
    if preMultiLine in aRegExOptions then aRegExOptionsStr := aRegExOptionsStr +'m';
    if preExtended in aRegExOptions then aRegExOptionsStr := aRegExOptionsStr +'x';
    //'l':;
    if preSingleLine in aRegExOptions then aRegExOptionsStr := aRegExOptionsStr + 's';
    //'u':;
    _WriteStr2Buffer(aTextNode.RegEx);
    _WriteStr2Buffer(#$00);
    _WriteStr2Buffer(aRegExOptionsStr);
    _WriteStr2Buffer(#$00);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x0D + name + \x00 + length (int32) + string + \x00
  Procedure _WriteJavascriptValue2Buffer(aTextNode:TALJSONNode);
  var aInt32: system.int32;
      aJavascript: ansiString;
  begin
    aJavascript := aTextNode.Javascript;
    aInt32 := length(aJavascript) + 1 {for the trailing #0};
    _Write2Buffer(aInt32, sizeOf(aInt32));
    _WriteStr2Buffer(aJavascript);
    _WriteStr2Buffer(#$00);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x10 + name + \x00 + int32
  Procedure _WriteInt32Value2Buffer(aTextNode:TALJSONNode);
  var aInt32: system.Int32;
  begin
    aInt32 := aTextNode.int32;
    _Write2Buffer(aInt32, sizeOf(aInt32));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x12 + name + \x00 + int64
  Procedure _WriteInt64Value2Buffer(aTextNode:TALJSONNode);
  var aInt64: system.Int64;
  begin
    aInt64 := aTextNode.int64;
    _Write2Buffer(aInt64, sizeOf(aInt64));
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
  var ANodeList: TALJSONNodeList;
      aEmptyNode: Boolean;
      aPos: system.int64;
      i: integer;
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

      aPos := StreamPos + BufferPos - 4{length of the #$00#$00#$00#$00};

      aEmptyNode := True;
      aNodeList := InternalGetChildNodes;
      If assigned(aNodeList) then begin
        with aNodeList do
          If count > 0 then begin
            aEmptyNode := False;
            NodeStack.Push(aObjectNode);
            NodeIndexStack.Push(aNodeIndex);
            NodeStartPosStack.Push(aPos);
            For i := Count - 1 downto 0 do begin
              NodeStack.Push(Nodes[i]);
              NodeIndexStack.Push(i);
              NodeStartPosStack.Push(-1);
            end;
          end
      end;

      If aEmptyNode then begin
        _WriteStr2Buffer(#$00);
        _WriteInt2Pos(5{length of the object},aPos);
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
  var ANodeList: TALJSONNodeList;
      aEmptyNode: Boolean;
      aPos: system.int64;
      i: integer;
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

      aPos := StreamPos + BufferPos - 4{length of the #$00+#$00+#$00+#$00};

      aEmptyNode := True;
      aNodeList := InternalGetChildNodes;
      If assigned(aNodeList) then begin
        with aNodeList do
          If count > 0 then begin
            aEmptyNode := False;
            NodeStack.Push(aArrayNode);
            NodeIndexStack.Push(aNodeIndex);
            NodeStartPosStack.Push(aPos);
            For i := Count - 1 downto 0 do begin
              NodeStack.Push(Nodes[i]);
              NodeIndexStack.Push(i);
              NodeStartPosStack.Push(-1);
            end;
          end
      end;

      If aEmptyNode then begin
        _WriteStr2Buffer(#$00);
        _WriteInt2Pos(5{length of the object},aPos);
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
  NodeStack := Tstack.Create;
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
      CurrentNode := TAlJSONNode(NodeStack.Pop);
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
Var afileStream: TfileStream;
begin
  aFileStream := TfileStream.Create(String(FileName),fmCreate);
  Try
    SaveToBsonStream(aFileStream);
  finally
    aFileStream.Free;
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

{***********************************************************************************************}
procedure TALJSONNode.LoadFromJSONStream(Stream: TStream; Const ClearChildNodes: Boolean = True);
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
Var afileStream: TfileStream;
Begin
  aFileStream := TfileStream.Create(string(FileName), fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromJSONStream(aFileStream, ClearChildNodes);
  finally
    aFileStream.Free;
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

{***********************************************************************************************}
procedure TALJSONNode.LoadFromBSONStream(Stream: TStream; Const ClearChildNodes: Boolean = True);
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
Var afileStream: TfileStream;
Begin
  aFileStream := TfileStream.Create(string(FileName), fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromBSONStream(aFileStream, ClearChildNodes);
  finally
    aFileStream.Free;
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
      //nstRegEx: only the regex option is store in the int64
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
var aDouble: Double;
    aBool: boolean;
    aDateTime: TdateTime;
    aInt32: system.int32;
    aTimestamp: TALBSONTimestamp;
begin
  if nvInt64 in fRawNodeValueDefined then result := fRawNodeValueInt64
  else begin

    if not (nvStr in fRawNodeValueDefined) then ALJsonDocError(CALJsonOperationError,GetNodeType);

    case fNodeSubType of
      nstFloat: begin
                  IF not ALTryStrToFloat(fRawNodeValueStr, aDouble, ALDefaultFormatSettings) then ALJSONDocError('%s is not a valid Float', [fRawNodeValueStr]);
                  fRawNodeValueInt64 := Pint64(@aDouble)^;
                end;
      //nstText: can not be retrieve from int64
      //nstObject: can not be retrieve from int64
      //nstArray: can not be retrieve from int64
      //nstBinary: only the binarysubtype is store in int64
      //nstObjectID: can not be retrieve from int64
      nstBoolean: begin
                    IF not ALTryStrToBool(fRawNodeValueStr, aBool) then ALJSONDocError('%s is not a valid Boolean', [fRawNodeValueStr]);
                    fRawNodeValueInt64 := ALBoolToInt(aBool);
                  end;
      nstDateTime: begin
                     IF not ALTryStrToDateTime(fRawNodeValueStr, aDateTime, ALdefaultFormatSettings) then ALJSONDocError('%s is not a valid Datetime', [fRawNodeValueStr]);
                     fRawNodeValueInt64 := Pint64(@aDateTime)^;
                   end;
      nstNull:  begin
                  fRawNodeValueInt64 := 0;
                end;
      //nstRegEx: only the regex option is store in the int64
      //nstJavascript: can not be retrieve from int64
      nstInt32: begin
                  IF not ALTryStrToInt(fRawNodeValueStr, aInt32) then ALJSONDocError('%s is not a valid Int32', [fRawNodeValueStr]);
                  fRawNodeValueInt64 := aInt32;
                end;
      nstTimestamp: begin
                      IF not ALJSONTryStrToTimestamp(fRawNodeValueStr, aTimestamp) then ALJSONDocError('%s is not a valid Timestamp', [fRawNodeValueStr]);
                      fRawNodeValueInt64 := aTimestamp.I64;
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
  fNodeSubType := NodeSubType;
  fRawNodeValueStr := Value;
  fRawNodeValueDefined := [nvStr];
end;

{************************************************************************************************}
procedure TALJSONTextNode.SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  fNodeSubType := NodeSubType;
  fRawNodeValueInt64 := Value;
  if (NodeSubType in [nstBinary, nstRegEx]) then fRawNodeValueDefined := fRawNodeValueDefined + [nvInt64] // keep the fNodeValueStr
  else fRawNodeValueDefined := [nvInt64];
end;

{*********************************************************************************************************************************}
procedure TALJSONTextNode.SetNodeValue(const StrValue: AnsiString; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
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

    aALJsonDocument.onParseText := procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType)
                                   begin
                                     if (NodeSubType = nstBoolean)   then aLst.Add(aPath + Path + aLst.NameValueSeparator + ALBoolToStr(Args[0].VBoolean,aTrueStr,aFalseStr))
                                     else if (NodeSubType = nstnull) then aLst.Add(aPath + Path + aLst.NameValueSeparator + aNullStr)
                                     else                                 aLst.Add(aPath + Path + aLst.NameValueSeparator + ansiString(Args[0].VAnsiString));
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

    aALJsonDocument.LoadFromJSONString(AJsonStr, true{saxMode});
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
                  if (aJsonNode.ChildNodes[i].NodeSubType = nstBoolean) then   aLst.Add(aTmpPath + aLst.NameValueSeparator + ALBoolToStr(aJsonNode.ChildNodes[i].Bool,aTrueStr,aFalseStr))
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
    nstDateTime:   result := ALFormatDateTime('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', ALStrToDateTime(aValue, aFormatSettings), ALDefaultFormatSettings);
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
  vALDefaultNodeIndent := '  '; { 2 spaces }


end.
