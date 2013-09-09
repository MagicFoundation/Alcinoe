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
                birth: new Date('Dec 03, 1924'),
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
              MyJsonDoc.childnodes['_id'].text;
              MyJsonDoc.childnodes['name'].childnodes['first'].text;
              MyJsonDoc.childnodes['name'].childnodes['last'].text;
              MyJsonDoc.childnodes['birth'].text;
              for i := 0 to MyJsonDoc.childnodes['contribs'].ChildNodes.count - 1 do
                MyJsonDoc.childnodes['contribs'].childnodes[i].text;
              for i := 0 to MyJsonDoc.childnodes['awards'].ChildNodes.count - 1 do begin
                MyJsonDoc.childnodes['awards'].childnodes[i].childnodes['award'].text;
                MyJsonDoc.childnodes['awards'].childnodes[i].childnodes['year'].text;
                MyJsonDoc.childnodes['awards'].childnodes[i].childnodes['by'].text;
              end;

              ------------------------------
              To create the document nodes :

              MyJsonDoc.addchild('_id').int := 1;
              with MyJsonDoc.addchild('name', ntObject) do begin
                addchild('first').text := 'John';
                addchild('last').text := 'Backus';
              end;
              MyJsonDoc.addchild('birth').statement := 'new Date(''Dec 03, 1924'')';
              with MyJsonDoc.addchild('contribs', ntArray) do begin
                addchild.text := 'Fortran';
                addchild.text := 'ALGOL';
                addchild.text := 'Backus-Naur Form';
                addchild.text := 'FP';
              end;
              with MyJsonDoc.addchild('awards', ntArray) do begin
                with addchild(ntObject) do begin
                  addchild('award').text := 'National Medal of Science';
                  addchild('year').int := 1975;
                  addchild('by').text := 'National Science Foundation';
                end;
                with addchild(ntObject) do begin
                  addchild('award').text := 'Turing Award';
                  addchild('year').int := 1977;
                  addchild('by').text := 'ACM';
                end;
              end;
              MyJsonDoc.addchild('spouse');
              MyJsonDoc.addchild('address', ntObject);
              MyJsonDoc.addchild('phones', ntArray);

              ---------------------------------------
              To parse an JSON document in Sax Mode :

              MyJsonDoc.onParseText := procedure (Sender: TObject;
                                                  const Path: AnsiString;
                                                  const name: AnsiString;
                                                  const str: AnsiString;
                                                  const QuotedStr: Boolean)
                                       begin
                                         Writeln(Path + '=' + str);
                                       end;
              MyJsonDoc.LoadFromJSON(AJsonStr, true);


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

uses Classes,
     sysutils,
     AlStringList;

resourcestring
  cALJSONNotActive             = 'No active document';
  cAlJSONNodeNotFound          = 'Node "%s" not found';
  cALJSONInvalidNodeType       = 'Invalid node type';
  cALJSONListCapacityError     = 'Node list capacity out of bounds (%d)';
  cALJSONListCountError        = 'Node list count out of bounds (%d)';
  cALJSONListIndexError        = 'Node list index out of bounds (%d)';
  cALJSONOperationError        = 'This operation can not be performed with a node of type %s';
  cALJSONParseError            = 'JSON Parse error';

const
  cALJSONNodeMaxListSize = Maxint div 16;

type

  {class definition}
  TALJSONNode = Class;
  TALJSONNodeList= Class;
  TALJSONDocument= Class;

  {$IF CompilerVersion >= 23} {Delphi XE2}
  TAlJSONParseDocument = reference to procedure (Sender: TObject);
  TAlJSONParseTextEvent = reference to procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Str: AnsiString; const QuotedStr: Boolean);
  TAlJSONParseObjectEvent = reference to procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString);
  TAlJSONParseArrayEvent = reference to procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString);
  {$ELSE}
  TAlJSONParseDocument = procedure (Sender: TObject) of object;
  TAlJSONParseTextEvent = procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Str: AnsiString; const QuotedStr: Boolean) of object;
  TAlJSONParseObjectEvent = procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString) of object;
  TAlJSONParseArrayEvent = procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString) of object;
  {$IFEND}

  TALJSONNodeListSortCompare = function(List: TALJSONNodeList; Index1, Index2: Integer): Integer;

  TALJSONNodeType = (ntObject, //The node represents an object: { ... } or "name": { ... }
                     ntArray,  //The node represents an array: [ ... ] or "name": [ ... ]
                     ntText);  //The node represents a text content (statement, string, number, true, false, null): "..." or "name": "..."

  TALJSONDocOption = (doNodeAutoCreate, // create only ntText Node !
                      doNodeAutoIndent); // affect only the SaveToStream
  TALJSONDocOptions = set of TALJSONDocOption;

  TALJSONParseOption = (poIgnoreControlCharacters); // don't decode escaped characters (like \") and not encode them also (when save / load)
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
    function FindNode(NodeName: AnsiString): TALJSONNode; overload;
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
    function GetNodeName: AnsiString;
    procedure SetNodeName(const Value: AnsiString);
    function GetNodeType: TALJSONNodeType; virtual; abstract;
    function GetNodeTypeStr: AnsiString;
    function GetNodeValue: ansiString; virtual;
    procedure SetNodeValue(const Value: AnsiString; const NeedQuotes: Boolean); virtual;
    function GetNodeValueNeedQuotes: Boolean; virtual;
    function GetText: AnsiString;
    procedure SetText(const Value: AnsiString);
    function GetFloat: Double;
    procedure SetFloat(const Value: Double);
    function GetInt: Integer;
    procedure SetInt(const Value: Integer);
    function GetInt64: Int64;
    procedure SetInt64(const Value: Int64);
    function GetBool: Boolean;
    procedure SetBool(const Value: Boolean);
    function GetNull: Boolean;
    procedure SetNull(const Value: Boolean);
    function GetStatement: AnsiString;
    procedure SetStatement(const Value: AnsiString);
    function GetOwnerDocument: TALJSONDocument;
    procedure SetOwnerDocument(const Value: TALJSONDocument);
    function GetParentNode: TALJSONNode;
    procedure SetParentNode(const Value: TALJSONNode);
    function GetJSON: AnsiString;
    procedure SetJSON(const Value: AnsiString);
    function NestingLevel: Integer;
  public
    constructor Create(const NodeName: AnsiString); virtual;
    function AddChild(const NodeName: AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode; overload;
    function AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode; overload;
    function NextSibling: TALJSONNode;
    function PreviousSibling: TALJSONNode;
    procedure SaveToStream(const Stream: TStream);
    procedure SaveToFile(const AFileName: AnsiString);
    procedure SaveToJSON(var JSON: AnsiString);
    procedure LoadFromFile(const AFileName: AnsiString);
    procedure LoadFromStream(const Stream: TStream);
    procedure LoadFromJSON(const JSON: AnsiString);
    property ChildNodes: TALJSONNodeList read GetChildNodes write SetChildNodes;
    property HasChildNodes: Boolean read GetHasChildNodes;
    property NodeName: AnsiString read GetNodeName write SetNodeName;
    property NodeType: TALJSONNodeType read GetNodeType;
    property OwnerDocument: TALJSONDocument read GetOwnerDocument Write SetOwnerDocument;
    property ParentNode: TALJSONNode read GetParentNode;
    property Text: AnsiString read GetText write SetText;
    property int: integer read GetInt write SetInt;
    property int64: int64 read Getint64 write Setint64;
    property Float: Double read GetFloat write SetFloat;
    property Bool: Boolean read GetBool write SetBool;
    property Null: Boolean read GetNull write SetNull;
    property Statement: AnsiString read GetStatement write SetStatement;
    property JSON: AnsiString read GetJSON write SetJSON;
  end;

  //JSON object represents {} or { members }
  TALJSONObjectNode = Class(TALJSONNode)
  private
    FChildNodes: TALJSONNodeList;
  protected
    function GetNodeType: TALJSONNodeType; override;
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
    function InternalGetChildNodes: TALJSONNodeList; override;
    function GetChildNodes: TALJSONNodeList; override;
    procedure SetChildNodes(const Value: TALJSONNodeList); override;
  public
    constructor Create(const NodeName: AnsiString = ''); override;
    Destructor Destroy; override;
  end;

  {Groups statement, string, number, true, false, null}
  TALJSONTextNode = Class(TALJSONNode)
  private
    fNodeValue: AnsiString;
    fNodeValueNeedQuotes: Boolean;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeValue: ansiString; override;
    procedure SetNodeValue(const Value: AnsiString; const NeedQuotes: Boolean); override;
    function GetNodeValueNeedQuotes: Boolean; override;
  public
    constructor Create(const NodeName: AnsiString = ''); override;
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
  protected
    procedure CheckActive;
    procedure DoParseStartDocument;
    procedure DoParseEndDocument;
    procedure DoParseText(const Path: AnsiString; const name: AnsiString; const str: AnsiString; const QuotedStr: Boolean);
    procedure DoParseStartObject(const Path: AnsiString; const Name: AnsiString);
    procedure DoParseEndObject(const Path: AnsiString; const Name: AnsiString);
    procedure DoParseStartArray(const Path: AnsiString; const Name: AnsiString);
    procedure DoParseEndArray(const Path: AnsiString; const Name: AnsiString);
    Procedure InternalParseJSON(Const RawJSONStream: TStream; Const ContainerNode: TALJSONNode);
    Procedure InternalParseBSON(Const RawBSONStream: TStream; Const ContainerNode: TALJSONNode);
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
    procedure SetOptions(const Value: TALJSONDocOptions);
    procedure SetParseOptions(const Value: TALJSONParseOptions);
    procedure SetJSON(const Value: ansiString);
    procedure SetNodeIndentStr(const Value: AnsiString);
  public
    constructor Create(const aActive: Boolean = True); virtual;
    destructor Destroy; override;
    function AddChild(const NodeName: AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode;
    function CreateNode(const NodeName: AnsiString; NodeType: TALJSONNodeType): TALJSONNode;
    function IsEmptyDoc: Boolean;
    procedure LoadFromFile(const AFileName: AnsiString; const saxMode: Boolean = False);
    procedure LoadFromStream(const Stream: TStream; const saxMode: Boolean = False);
    procedure LoadFromJSON(const JSON: AnsiString; const saxMode: Boolean = False);
    procedure LoadFromBSON(const BSON: AnsiString);
    procedure SaveToFile(const AFileName: AnsiString = '');
    procedure SaveToStream(const Stream: TStream);
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
    property OnParseStartDocument: TAlJSONParseDocument read fOnParseStartDocument write fOnParseStartDocument;
    property OnParseEndDocument: TAlJSONParseDocument read fOnParseEndDocument write fOnParseEndDocument;
    property onParseText: TAlJSONParseTextEvent read fonParseText write fonParseText;
    property onParseStartObject: TAlJSONParseObjectEvent read fonParseStartObject write fonParseStartObject;
    property onParseEndObject: TAlJSONParseObjectEvent read fonParseEndObject write fonParseEndObject;
    property onParseStartArray: TAlJSONParseArrayEvent read fonParseStartArray write fonParseStartArray;
    property onParseEndArray: TAlJSONParseArrayEvent read fonParseEndArray write fonParseEndArray;
    property Tag: NativeInt read FTag write FTag;
  end;

{misc constante}
Const CALDefaultNodeIndent  = '  '; { 2 spaces }

{misc function}
{$IF CompilerVersion >= 23} {Delphi XE2}
Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
                           aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false');
{$ifend}

implementation

uses Contnrs,
     AlHTML,
     ALString;

type

  {----------------------------}
  TALBSONReader = class(TObject)
  private
    FStream:       TStream;
    FDocumentNode: TALJSONNode;
    FActiveNode:   TALJSONNode;
    function _ReadInteger: integer;
    function _ReadByte: byte;
    function _ReadName: AnsiString;
    function _ReadInt64: Int64;
    function _ReadDouble: double;
    function _ReadString(aCount: integer): AnsiString;
    function _ReadBinaryData(aCount: integer): TBytes;
  protected
  public
    procedure   ParseElement(aByte: byte);
    procedure   ParseEList;
    procedure   ParseDocument(aContainerNode: TALJSONNode);
    constructor Create(aStream: TStream; aDocumentNode: TALJSONNode); virtual;
  end;

  {----------------------------}
  TALBSONWriter = class(TObject)
  private
  protected
  public
    function ParseObject(const aContainerNode: TALJSONNode; const aArrayLength: integer = 0): AnsiString;
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
  FTag := 0;
  SetActive(aActive);
end;

{*********************************}
destructor TALJSONDocument.Destroy;
begin
  ReleaseDoc;
  inherited;
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
Procedure TALJSONDocument.InternalParseJSON(Const RawJSONStream: TStream; Const ContainerNode: TALJSONNode);

Const BufferSize: integer = 8192;
Var RawJSONString: AnsiString;
    RawJSONStringLength: Integer;
    RawJSONStringPos: Integer;
    LstParams: TALStringList;
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
    If RawJSONStringPos > 1 then begin
      Byte2Read := RawJSONStringPos - 1;
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

  {~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeNode;
  Var aName, aValue: AnsiString;
      aNode: TALJsonNode;
      aQuoteChar: ansiChar;
      aNameValueSeparator: ansiChar;
      aNodeTypeInt: integer;
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
          aNode.SetNodeValue(aName, aQuoteChar in ['"','''']);
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
      DoParseText(GetPathStr('['+alinttostr(ArrayIdx))+']', '', aName, aQuoteChar in ['"','''']);

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
    if NotSaxMode then begin

      //if workingnode = nil then it's mean we are outside the containerNode
      if not assigned(WorkingNode) then ALJSONDocError(CALJSONParseError);

      //Node withe name MUST be ONLY present inside an object node
      if (ArrayIdx >= 0)  or
         (WorkingNode.nodetype <> ntObject) then ALJSONDocError(CALJSONParseError);

      //create the node and add it to the workingnode
      aNode := CreateNode(aName, nttext);
      try
        aNode.SetNodeValue(aValue, aQuoteChar in ['"','''']);
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
    DoParseText(GetPathStr(aName), aName, aValue, aQuoteChar in ['"','''']);
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

  LstParams := TALStringList.Create;
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
    LstParams.Free;
  end;

end;

{*************************************************************}
{Last version of the spec: http://bsonspec.org/#/specification}
procedure TALJSONDocument.InternalParseBSON(const RawBSONStream: TStream; const ContainerNode: TALJSONNode);
var aBSONReader: TALBSONReader;
begin
  // start to parse BSON
  DoParseStartDocument;

  aBSONReader := TALBSONReader.Create(RawBSONStream, ContainerNode);
  try
    aBSONReader.ParseDocument(ContainerNode);
  finally
    aBSONReader.Free;
  end;

  // finish to parse BSON
  DoParseEndDocument;
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
procedure TALJSONDocument.LoadFromFile(const AFileName: AnsiString; const saxMode: Boolean = False);
var FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(string(aFileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FileStream, saxMode);
  finally
    FileStream.Free;
  end;
end;

{****************************************************}
{Loads an JSON document from a stream and activates it.
 Call LoadFromStream to load the JSON document from a stream.
 *Stream is a stream object that can be used to read the string of JSON that makes up the document.
 After loading the document from Stream, LoadFromStream sets the Active property to true.}
procedure TALJSONDocument.LoadFromStream(const Stream: TStream; const saxMode: Boolean = False);
begin
  if saxMode then SetActive(False)
  else begin
    releaseDoc;
    SetActive(True);
  end;
  InternalParseJSON(Stream, FDocumentNode);
end;

{*****************************************************************}
{Loads a string representation of an JSON document and activates it.
 Call LoadFromJSON to assign a string as the value of the JSON document. Unlike the JSON property, which lets you assign JSON on a line-by-line
 basis, LoadFromJSON treats the text of the JSON document as a whole.
 The JSON parameter is a string containing the text of an JSON document. It should represent the JSON text encoded using 8 bits char (utf-8, iso-8859-1, etc)
 After assigning the JSON property as the contents of the document, LoadFromJSON sets the Active property to true.}
procedure TALJSONDocument.LoadFromJSON(const JSON: AnsiString; const saxMode: Boolean = False);
var StringStream: TALStringStream;
begin
  StringStream := TALStringStream.Create(JSON);
  try
    LoadFromStream(StringStream, saxMode);
  finally
    StringStream.Free;
  end;
end;

{*************************************************************}
procedure TALJSONDocument.LoadFromBSON(const BSON: AnsiString);
var StringStream: TALStringStream;
begin
  StringStream := TALStringStream.Create(BSON);
  try
    releaseDoc;
    SetActive(True);
    InternalParseBSON(StringStream, FDocumentNode);
  finally
    StringStream.Free;
  end;
end;

{******************************}
{Saves the JSON document to disk.
 Call SaveToFile to save any modifications you have made to the parsed JSON document.
 AFileName is the name of the file to save. If AFileName is an empty string, TJSONDocument uses the value of the FileName property.}
procedure TALJSONDocument.SaveToFile(const AFileName: AnsiString = '');
Var afileStream: TfileStream;
begin
  aFileStream := TfileStream.Create(String(AFileName),fmCreate);
  Try
    SaveToStream(aFileStream);
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
    SaveToStream(StringStream);
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
Var aBSONWriter: TALBSONWriter;
begin
  CheckActive;

  aBSONWriter := TALBSONWriter.Create;
  try
    BSON := aBSONWriter.ParseObject(FDocumentNode);
  finally
    aBSONWriter.Free;
  end;
end;

{**********************************}
{Saves the JSON document to a stream.
 Call SaveToStream to save the contents of the JSON document to the stream specified by Stream.}
procedure TALJSONDocument.SaveToStream(const Stream: TStream);
begin
  CheckActive;
  node.SaveToStream(Stream);
end;

{*************************************}
{Returns the value of the JSON property.
 GetJSON is the read implementation of the JSON property.}
function TALJSONDocument.GetJSON: AnsiString;
begin
  SaveToJSON(Result);
end;

{**********************************}
{Sets the value of the JSON property.
 SetJSON is the write implementation of the JSON property.
 *Value contains the raw (unparsed) JSON to assign.}
procedure TALJSONDocument.SetJSON(const Value: AnsiString);
begin
 LoadFromJSON(JSON, False);
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

{*************************************************************************************************************************************}
procedure TALJSONDocument.DoParseText(const Path: AnsiString; const name: AnsiString; const str: AnsiString; const QuotedStr: Boolean);
begin
  if Assigned(fonParseText) then fonParseText(Self, Path, name, str, QuotedStr);
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

{*******************************************}
function TALJSONNode.GetNodeName: AnsiString;
begin
  result := FNodeName;
end;

{********************************************************}
procedure TALJSONNode.SetNodeName(const Value: AnsiString);
begin
  fNodeName := Value;
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
  result := ''; // hide warning
  ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr])
end;

{*************************************************************************************}
procedure TALJSONNode.SetNodeValue(const Value: AnsiString; const NeedQuotes: Boolean);
begin
  ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr])
end;

{***************************************************}
function TALJSONNode.GetNodeValueNeedQuotes: Boolean;
begin
  result := false; // hide warning
  ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr])
end;

{***********************************}
{Returns the text value of the node.}
function TALJSONNode.GetText: AnsiString;
begin
  result := GetNodeValue;
end;

{********************************}
{Sets the text value of the node.}
procedure TALJSONNode.SetText(const Value: AnsiString);
begin
  setNodeValue(Value, True{QuotedValue});
end;

{************************************}
function TALJSONNode.GetFloat: Double;
begin
  Result := ALStrToFloat(GetNodeValue, ALDefaultFormatSettings);
end;

{**************************************************}
procedure TALJSONNode.SetFloat(const Value: Double);
begin
  setNodeValue(ALFloatToStr(Value, ALDefaultFormatSettings), False{QuotedValue});
end;

{***********************************}
function TALJSONNode.GetInt: Integer;
begin
  Result := ALStrToInt(GetNodeValue);
end;

{*************************************************}
procedure TALJSONNode.SetInt(const Value: Integer);
begin
  setNodeValue(ALIntToStr(Value), False{QuotedValue});
end;

{***********************************}
function TALJSONNode.GetInt64: Int64;
begin
  Result := ALStrToInt64(GetNodeValue);
end;

{*************************************************}
procedure TALJSONNode.SetInt64(const Value: Int64);
begin
  setNodeValue(ALIntToStr(Value), False{QuotedValue});
end;

{************************************}
function TALJSONNode.GetBool: Boolean;
begin
  Result := ALStrToBool(GetNodeValue);
end;

{**************************************************}
procedure TALJSONNode.SetBool(const Value: Boolean);
begin
  if Value then setNodeValue('true', False{QuotedValue})
  else setNodeValue('false', False{QuotedValue})
end;

{************************************}
function TALJSONNode.GetNull: Boolean;
begin
  Result := (not GetNodeValueNeedQuotes) and
            (AlSameText(GetNodeValue, 'null'));
end;

{**************************************************}
procedure TALJSONNode.SetNull(const Value: Boolean);
begin
  if Value then setNodeValue('null', False{QuotedValue})
  else ALJSONDocError('Only "true" is allowed for setNull property');
end;

{********************************************}
function TALJSONNode.GetStatement: AnsiString;
begin
  Result := GetNodeValue;
end;

{**********************************************************}
procedure TALJSONNode.SetStatement(const Value: AnsiString);
begin
  setNodeValue(Value, False{QuotedValue});
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
Var aStringStream: TALStringStream;
begin
  aStringStream := TALStringStream.Create('');
  Try
    SaveToStream(aStringStream);
    result := aStringStream.DataString;
  finally
    aStringStream.Free;
  end;
end;

{************************************************}
{SetJSON reload the node with the new given value }
procedure TALJSONNode.SetJSON(const Value: AnsiString);
Var aStringStream: TALStringStream;
Begin
  aStringStream := TALStringStream.Create(Value);
  Try
    LoadFromStream(aStringStream);
  finally
    aStringStream.Free;
  end;
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
procedure TALJSONNode.SaveToStream(const Stream: TStream);

Const BufferSize: integer = 32768;
Var NodeStack: Tstack;
    CurrentNode: TalJSONNode;
    CurrentParentNode: TalJSONNode;
    CurrentIndentStr: AnsiString;
    IndentStr: AnsiString;
    EncodeControlCharacters: Boolean;
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
    if L >= BufferSize then WriteBuffer2Stream(str, l)
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
  Begin
    with aTextNode do begin

      if not (LastWrittenChar in ['{','[']) then WriteStr2Buffer(',');

      if AutoIndentNode then WriteStr2Buffer(#13#10 + CurrentIndentStr);

      if (assigned(ParentNode)) and
         (ParentNode.NodeType <> ntArray) then begin
        if EncodeControlCharacters then WriteStr2Buffer('"'+ALJavascriptEncode(NodeName)+'":')
        else WriteStr2Buffer('"'+NodeName+'":');
      end;

      if GetNodeValueNeedQuotes then begin
        if EncodeControlCharacters then WriteStr2Buffer('"'+ALJavascriptEncode(Text)+'"')
        else WriteStr2Buffer('"'+Text+'"');
      end
      else WriteStr2Buffer(Text);

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

      if (assigned(ParentNode)) and
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

      if AutoIndentNode then WriteStr2Buffer(#13#10 + CurrentIndentStr);

      if (assigned(ParentNode)) and
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
  If NodeType <> ntobject then exit;

  CurrentParentNode := nil;
  NodeStack := Tstack.Create;
  Try

    {init buffer string}
    Setlength(BufferString, bufferSize);
    BufferStringPos := 0;
    LastWrittenChar := '{';
    EncodeControlCharacters := not (poIgnoreControlCharacters in FDocument.ParseOptions);
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

{**********************************************************}
procedure TALJSONNode.LoadFromStream(const Stream: TStream);
Begin
  If NodeType <> ntObject then ALJsonDocError(CALJsonOperationError,[GetNodeTypeStr]);
  ChildNodes.Clear;
  Try
    FDocument.InternalParseJSON(Stream, self);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{************************************************************}
procedure TALJSONNode.SaveToFile(const AFileName: AnsiString);
Var afileStream: TfileStream;
begin
  aFileStream := TfileStream.Create(String(AFileName),fmCreate);
  Try
    SaveToStream(aFileStream);
  finally
    aFileStream.Free;
  end;
end;

{**************************************************************}
procedure TALJSONNode.LoadFromFile(const AFileName: AnsiString);
Var afileStream: TfileStream;
Begin
  aFileStream := TfileStream.Create(string(AFileName), fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromStream(aFileStream);
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
    SaveToStream(aStringStream);
    JSON := aStringStream.DataString;
  finally
    aStringStream.Free;
  end;
end;

{*********************************************************}
procedure TALJSONNode.LoadFromJSON(const JSON: AnsiString);
Var aStringStream: TALStringStream;
Begin
  aStringStream := TALStringStream.Create(JSON);
  Try
    LoadFromStream(aStringStream);
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
  fNodeValue := '';
  fNodeValueNeedQuotes := true;
end;

{**************************************************}
function TALJSONTextNode.GetNodeType: TALJSONNodeType;
begin
  Result := NtText;
end;

{************************************************}
function TALJSONTextNode.GetNodeValue: ansiString;
begin
  result := fNodeValue;
end;

{*****************************************************************************************}
procedure TALJSONTextNode.SetNodeValue(const Value: AnsiString; const NeedQuotes: Boolean);
begin
  fNodeValue := Value;
  fNodeValueNeedQuotes := NeedQuotes;
end;

{*******************************************************}
function TALJSONTextNode.GetNodeValueNeedQuotes: Boolean;
begin
  result := fNodeValueNeedQuotes;
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
function TALJSONNodeList.FindNode(NodeName: AnsiString): TALJSONNode;
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

{*****************************************************************************}
constructor TALBSONReader.Create(aStream: TStream; aDocumentNode: TALJSONNode);
begin
  FStream       := aStream;
  FDocumentNode := aDocumentNode;
  FActiveNode   := aDocumentNode;
end;

{*************************************}
function TALBSONReader._ReadByte: byte;
begin
  FStream.ReadBuffer(result, SizeOf(byte));
end;

{**************************************************************}
function TALBSONReader._ReadBinaryData(aCount: integer): TBytes;
begin
  SetLength(result, aCount);
  FStream.ReadBuffer(result[1], aCount);
end;

{*****************************************}
function TALBSONReader._ReadDouble: double;
begin
  FStream.ReadBuffer(result, SizeOf(double));
end;

{***************************************}
function TALBSONReader._ReadInt64: Int64;
begin
  FStream.ReadBuffer(result, SizeOf(Int64));
end;

{*******************************************}
function TALBSONReader._ReadInteger: integer;
begin
  FStream.ReadBuffer(result, SizeOf(integer));
end;

{**************************************************************}
function TALBSONReader._ReadString(aCount: integer): AnsiString;
begin
  SetLength(result, aCount);
  FStream.ReadBuffer(result[1], aCount);

  // remove ending #$00
  SetLength(result, aCount - 1);
end;

{********************}
{e_name	  ::=	cstring
 cstring	::=	(byte*) "\x00"}
function TALBSONReader._ReadName: AnsiString;
var aByte: byte;
begin
  result := '';
  aByte := _ReadByte;
  while aByte <> 0 do begin
    result := result + AnsiChar(Chr(aByte));
    aByte  := _ReadByte;
  end;
end;

{************************************************}
{element	::=	"\x01" e_name double	Floating point
            |	"\x02" e_name string	UTF-8 string
            |	"\x03" e_name document	Embedded document
            |	"\x04" e_name document	Array
            |	"\x05" e_name binary	Binary data
            |	"\x06" e_name	Undefined — Deprecated
            |	"\x07" e_name (byte*12)	ObjectId
            |	"\x08" e_name "\x00"	Boolean "false"
            |	"\x08" e_name "\x01"	Boolean "true"
            |	"\x09" e_name int64	UTC datetime
            |	"\x0A" e_name	Null value
            |	"\x0B" e_name cstring cstring	Regular expression
            |	"\x0C" e_name string (byte*12)	DBPointer — Deprecated
            |	"\x0D" e_name string	JavaScript code
            |	"\x0E" e_name string	Symbol — Deprecated
            |	"\x0F" e_name code_w_s	JavaScript code w/ scope
            |	"\x10" e_name int32	32-bit Integer
            |	"\x11" e_name int64	Timestamp
            |	"\x12" e_name int64	64-bit integer
            |	"\xFF" e_name	Min key
            |	"\x7F" e_name	Max key}
procedure TALBSONReader.ParseElement(aByte: byte);
var aName:              AnsiString;
    aStringLength:      integer;
    aString:            AnsiString;
    aDouble:            double;
    aInteger:           integer;
    aBoolean:           byte;
    aInt64:             Int64;
    aBinaryLength:      integer;
    aBinarySubtype:     byte;
    aBinaryData:        TBytes;
    aRegExpPattern:     AnsiString;
    aRegExpOptions:     AnsiString;
    aNode:              TALJSONNode;
    FCurrentActiveNode: TALJSONNode;
    aChar:              AnsiChar;
begin
  aName := _ReadName;

  {$REGION 'Floating point'}
  if aByte = 1 then begin
    aDouble := _ReadDouble;
    with FActiveNode.AddChild(aName) do begin
      Float := aDouble;
    end;
  end
  {$ENDREGION}

  {$REGION 'UTF-8 string'}
  else if aByte = 2 then begin
    aStringLength := _ReadInteger;
    aString       := _ReadString(aStringLength);
    with FActiveNode.AddChild(aName) do begin
      Text := aString;
    end;
  end
  {$ENDREGION}

  {$REGION 'Embedded document'}
  else if aByte = 3 then begin
    FCurrentActiveNode := FActiveNode;
    aNode := FActiveNode.AddChild(aName, ntObject);
    ParseDocument(aNode);
    FActiveNode := FCurrentActiveNode;
  end
  {$ENDREGION}

  {$REGION 'Array'}
  else if aByte = 4 then begin
    FCurrentActiveNode := FActiveNode;
    aNode := FActiveNode.AddChild(aName, ntArray);
    ParseDocument(aNode);
    FActiveNode := FCurrentActiveNode;
  end
  {$ENDREGION}

  {$REGION 'Binary data'}
  {"\x05" e_name binary
   binary	  ::=	int32 subtype (byte*)
   subtype	::=	"\x00"	Binary / Generic
              |	"\x01"	Function
              |	"\x02"	Binary (Old)
              |	"\x03"	UUID (Old)
              |	"\x04"	UUID
              |	"\x05"	MD5
              |	"\x80"	User defined}
  else if aByte = 5 then begin
    aBinaryLength  := _ReadInteger;
    aBinarySubtype := _ReadByte;

    // security check
    if (aBinarySubtype <> 0) and
       (aBinarySubtype <> 1) and
       (aBinarySubtype <> 2) and
       (aBinarySubtype <> 3) and
       (aBinarySubtype <> 4) and
       (aBinarySubtype <> 5) and
       (aBinarySubtype <> 128) then raise EALJSONDocError.Create('Unknown type of binary data: ' + IntToStr(aBinarySubtype));

    // TODO -oIgor: think what specific we have to do to handle binary data
    aBinaryData := _ReadBinaryData(aBinaryLength);
    with FActiveNode.AddChild(aName) do begin

      // save function as statement
      if aBinarySubtype <> 1 then Statement := AnsiString(StringOf(aBinaryData))

      // save other formats as text
      else Text := AnsiString(StringOf(aBinaryData));

    end;
  end
  {$ENDREGION}

  {$REGION 'Undefined — Deprecated'}
  // nothing to handle here, some undefined type of data
  else if aByte = 6 then Exit
  {$ENDREGION}

  {$REGION 'ObjectID'}
  {http://docs.mongodb.org/manual/reference/object-id/
   e_name (byte*12)	ObjectId}
  else if aByte = 7 then begin
    aBinaryData := _ReadBinaryData(12);
    with FActiveNode.AddChild(aName) do begin
      Text := AnsiString(StringOf(aBinaryData));
    end;
  end
  {$ENDREGION}

  {$REGION 'Boolean false and true'}
  {"\x08" e_name "\x00"}
  else if aByte = 8 then begin
    aBoolean := _ReadByte;
    with FActiveNode.AddChild(aName) do begin
      Bool := (aBoolean = 1);
    end;
  end
  {$ENDREGION}

  {$REGION 'UTC datetime'}
  {"\x09" e_name int64}
  else if aByte = 9 then begin
    aInt64 := _ReadInt64;
    with FActiveNode.AddChild(aName) do begin
      int64 := aInt64;
    end;
  end
  {$ENDREGION}

  {$REGION 'Null value'}
  {"\x0A" e_name}
  else if aByte = 10 then begin
    with FActiveNode.AddChild(aName) do begin
      Null := true;
    end;
  end
  {$ENDREGION}

  {$REGION 'Regular expression'}
  {"\x0B" e_name cstring cstring
   The first cstring is the regex pattern, the second is the regex options string. Options are
   identified by characters, which must be stored in alphabetical order. Valid options are 'i' for
   case insensitive matching, 'm' for multiline matching, 'x' for verbose mode, 'l' to make \w, \W,
   etc. locale dependent, 's' for dotall mode ('.' matches everything), and 'u' to make \w, \W,
   etc. match unicode.}
  else if aByte = 11 then begin
    aRegExpPattern := _ReadName;
    aRegExpOptions := _ReadName;

    // security check for options
    for aChar in aRegExpOptions do begin
      if (aChar <> 'i') and
         (aChar <> 'm') and
         (aChar <> 'x') and
         (aChar <> 'l') and
         (aChar <> 's') and
         (aChar <> 'u') then raise EALJSONDocError.Create('Unknown reg exp option: ' + aChar);
    end;

    // save as regular expression statement
    with FActiveNode.AddChild(aName) do begin
      Statement := '/' + aRegExpPattern + '/' + aRegExpOptions;
    end;
  end
  {$ENDREGION}

  {$REGION 'DBPointer — Deprecated'}
  {"\x0C" e_name string (byte*12)}
  else if aByte = 12 then begin
    aStringLength := _ReadInteger;
    aString       := _ReadString(aStringLength);
    aBinaryData   := _ReadBinaryData(12);

    // DBPointer is not described. It's only stated that it's deprecated.
    // So we cannot be sure how we need to build the JSON node with this
    // data. I prefer to make an exception here if we will found such data,
    // so when we will get this kind of exception, we could be able to
    // investigate it.
    raise EALJSONDocError.Create('DBPointer found. Data: ' + String(aString) + #13#10 + 'Binary data: ' + StringOf(aBinaryData));
  end
  {$ENDREGION}

  {$REGION 'JavaScript code'}
  {"\x0D" e_name string}
  else if aByte = 13 then begin
    aStringLength := _ReadInteger;
    aString       := _ReadString(aStringLength);

    // save javascript code as statement
    with FActiveNode.AddChild(aName) do begin
      Statement := aString;
    end;
  end
  {$ENDREGION}

  {$REGION 'Symbol — Deprecated'}
  {"\x0E" e_name string}
  else if aByte = 14 then begin
    aStringLength := _ReadInteger;
    aString       := _ReadString(aStringLength);

    // Normally it is just a deprecated form to describe UTF-8 string,
    // so we just save it as text
    with FActiveNode.AddChild(aName) do begin
      Text := aString;
    end;
  end
  {$ENDREGION}

  {$REGION 'JavaScript code w/ scope'}
  {"\x0F" e_name code_w_s
   code_w_s	::=	int32 string document
   Code w/ scope - The int32 is the length in bytes of the entire code_w_s value.
   The string is JavaScript code. The document is a mapping from identifiers to values,
   representing the scope in which the string should be evaluated.}
  else if aByte = 15 then begin
    _ReadInteger;
    aStringLength := _ReadInteger;
    aString       := _ReadString(aStringLength);

    // parse internal document
    FCurrentActiveNode := FActiveNode;
    aNode := FActiveNode.AddChild(aName, ntObject);
    ParseDocument(aNode);
    FActiveNode := FCurrentActiveNode;

    // NOTICE: i did not found good example how it looks like
    // if there's some javascript code with document with mapping,
    // so i will put this javascript just as one more statement
    // inside created node
    with FActiveNode.AddChild('statement') do begin
      Statement := aString;
    end;
  end
  {$ENDREGION}

  {$REGION '32-bit Integer'}
  else if aByte = 16 then begin
    aInteger := _ReadInteger;
    with FActiveNode.AddChild(aName) do begin
      int := aInteger;
    end;
  end
  {$ENDREGION}

  {$REGION 'Timestamp'}
  {"\x11" e_name int64}
  else if aByte = 17 then begin
    aInt64 := _ReadInt64;
    with FActiveNode.AddChild(aName) do begin
      int64 := aInt64;
    end;
  end
  {$ENDREGION}

  {$REGION '64-bit integer'}
  {"\x12" e_name int64}
  else if aByte = 18 then begin
    aInt64 := _ReadInt64;
    with FActiveNode.AddChild(aName) do begin
      int64 := aInt64;
    end;
  end
  {$ENDREGION}

  {$REGION 'Min key'}
  {"\xFF" e_name}
  else if aByte = 255 then begin
    with FActiveNode.AddChild(aName) do begin
      Statement := 'MinKey';  // try to type in browser console > MinKey, it will shows the BSON-function
    end;
  end
  {$ENDREGION}

  {$REGION 'Max key'}
  {"\x7F" e_name}
  else if aByte = 127 then begin
    with FActiveNode.AddChild(aName) do begin
      Statement := 'MaxKey';  // try to type in browser console > MaxKey, it will shows the BSON-function
    end;
  end;
  {$ENDREGION}

end;

{******************************}
{e_list	::=	element e_list | ""}
procedure TALBSONReader.ParseEList;
var aByte: byte;
begin
  aByte := _ReadByte;
  if aByte = 0 then Exit
  else begin
    ParseElement(aByte);
    ParseEList;
  end;
end;

{********************************}
{document	::=	int32 e_list "\x00"
 The int32 is the total number of bytes comprising the document.}
procedure TALBSONReader.ParseDocument(aContainerNode: TALJSONNode);
var aLengthOfDocument: integer;
    aInitialPosition:  integer;
    aFinalPosition:    integer;
begin
  FActiveNode       := aContainerNode;
  aInitialPosition  := FStream.Position;
  aLengthOfDocument := _ReadInteger;
  if aLengthOfDocument > 5 then ParseEList;
  aFinalPosition := FStream.Position;
  if aFinalPosition - aInitialPosition <> aLengthOfDocument then raise EALJSONDocError.Create('Incorrect length of document, ' + IntToStr(aLengthOfDocument) + ' expected');
end;

{********************************************************************}
function TALBSONWriter.ParseObject(const aContainerNode: TALJSONNode;
                                   const aArrayLength: integer = 0): AnsiString;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _clearStream(aStream: TStream);
  begin
    aStream.Position := 0;
    aStream.Size     := 0;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~}
  // "\x02" e_name string
  // e_name	 ::=	cstring
  // cstring ::=	(byte*) "\x00"
  procedure _writeUTF8String(aStream: TStream; aNode: TALJSONNode; aArrayIndex: integer = -1);
  var aName:   AnsiString;
      aText:   AnsiString;
      aLength: integer;
  begin
    if aArrayIndex > -1 then aName := #2 + ALIntToStr(aArrayIndex) + #0
    else aName   := #2 + aNode.NodeName + #0;

    aLength := Length(aNode.Text) + 1; // +1 for trailing zero according the spec
    aText   := aNode.Text + #0;

    aStream.Write(aName[1], Length(aName));
    aStream.Write(aLength,  Sizeof(integer));
    aStream.Write(aText[1], Length(aText));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  // "\x04" e_name document
  // e_name	  ::=	cstring
  // document	::=	int32 e_list "\x00"
  {The document for an array is a normal BSON document with integer values for the keys, starting with 0 and
   continuing sequentially. For example, the array ['red', 'blue'] would be encoded as the document '0':
   'red', '1': 'blue'. The keys must be in ascending numerical order.}
  procedure _writeEmbeddedArray(aStream: TStream; aNode: TALJSONNode; aArrayLength: integer = 0; aArrayIndex: integer = -1);
  var aName:     AnsiString;
      aDocument: AnsiString;
  begin
    if aArrayIndex > -1 then aName := #4 + ALIntToStr(aArrayIndex) + #0
    else aName   := #4 + aNode.NodeName + #0;
    aDocument := ParseObject(aNode, aArrayLength);

    aStream.Write(aName[1],     Length(aName));
    aStream.Write(aDocument[1], Length(aDocument));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  // "\x03" e_name document
  // e_name	  ::=	cstring
  // document	::=	int32 e_list "\x00"
  procedure _writeEmbeddedObject(aStream: TStream; aNode: TALJSONNode; aArrayLength: integer = 0; aArrayIndex: integer = -1);
  var aName:     AnsiString;
      aDocument: AnsiString;
  begin
    if aArrayIndex > -1 then aName := #3 + ALIntToStr(aArrayIndex) + #0
    else aName   := #3 + aNode.NodeName + #0;
    aDocument := ParseObject(aNode, aArrayLength);

    aStream.Write(aName[1],     Length(aName));
    aStream.Write(aDocument[1], Length(aDocument));
  end;

var aNode:         TALJSONNode;
    i:             integer;
    aStr:          AnsiString;
    aLocalStream:  TALStringStream;
    aObjectLength: integer;
    aArrayIndex:   integer;
begin
  aStr         := '';
  aLocalStream := TALStringStream.Create('');
  aArrayIndex  := -1;

  try

    // scroll all child nodes
    for i := 0 to aContainerNode.ChildNodes.Count - 1 do begin

      // get current node
      aNode := aContainerNode.ChildNodes[i];

      // reset stream
      _clearStream(aLocalStream);

      {$REGION 'Process ntText-node'}
      if aNode.NodeType = ntText then begin

        // save UTF-8 string
        if aArrayLength > 0 then begin
          Inc(aArrayIndex);
          _writeUTF8String(aLocalStream, aNode, aArrayIndex);
        end

        else _writeUTF8String(aLocalStream, aNode);

      end
      {$ENDREGION}

      {$REGION 'Process ntObject-node'}
      else if aNode.NodeType = ntArray then begin

        // save embedded array
        if aArrayLength > 0 then begin
          Inc(aArrayIndex);
          _writeEmbeddedArray(aLocalStream, aNode, aNode.ChildNodes.Count, aArrayIndex);
        end

        else _writeEmbeddedArray(aLocalStream, aNode, aNode.ChildNodes.Count);

      end
      {$ENDREGION}

      {$REGION 'Process ntObject-node'}
      else if aNode.NodeType = ntObject then begin

        // save embedded object
        if aArrayLength > 0 then begin
          Inc(aArrayIndex);
          _writeEmbeddedObject(aLocalStream, aNode, aArrayIndex);
        end
        else _writeEmbeddedObject(aLocalStream, aNode);

      end;
      {$ENDREGION}

      // save currently processed data
      aStr := aStr + aLocalStream.DataString;

    end;

    // Write the whole object with its size
    // document	 ::=	int32 e_list "\x00"
    _clearStream(aLocalStream);
    aObjectLength := Length(aStr) + 5; // 5 - it's trailing zero of document + 4 bytes to save the size of document
    aStr          := aStr + #0;
    aLocalStream.Write(aObjectLength, Sizeof(integer));
    aLocalStream.Write(aStr[1],       Length(aStr));

    result := aLocalStream.DataString;

  finally
    aLocalStream.Free;
  end;
end;

{****************************************}
  {$IF CompilerVersion >= 23} {Delphi XE2}
Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
                           aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false');

var aALJsonDocument: TALJsonDocument;
begin
  aALJsonDocument := TALJsonDocument.Create;
  try
    aALJsonDocument.onParseText := procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const str: AnsiString; const QuotedStr: Boolean)
                                   begin
                                     if (not QuotedStr) and (ALSameText(str, 'true')) then aLst.Add(Path + aLst.NameValueSeparator + aTrueStr)
                                     else if (not QuotedStr) and (ALSameText(str, 'false')) then aLst.Add(Path + aLst.NameValueSeparator + aFalseStr)
                                     else if (not QuotedStr) and (ALSameText(str, 'null')) then aLst.Add(Path + aLst.NameValueSeparator + aNullStr)
                                     else aLst.Add(Path + aLst.NameValueSeparator + str);
                                   end;
    aALJsonDocument.LoadFromJSON(AJsonStr, true{saxMode});
  finally
    aALJsonDocument.Free;
  end;
end;
{$ifend}

end.
