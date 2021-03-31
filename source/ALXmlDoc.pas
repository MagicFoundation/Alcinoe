{*******************************************************************************
TALXmlDocument is exactly like Delphi TXmlDocument
(Same functions and procedures) but 10 to 100 times more
faster (see demo) and can work even in sax mode !

Use TAlXMLDocument to represent an XML document.
TAlXMLDocument can read an existing XML document from a
file, it can be associated with an in-memory string that is
the contents of an XML document, or it can create a new,
empty XML document.

TALXMLDocument uses it's own internal parser to analyze
the XML document.
*******************************************************************************}

unit ALXmlDoc;

interface

uses
  System.Classes,
  System.sysutils,
  AlStringList;

const
  cALXmlNotActive            = 'No active document';
  cAlXmlNodeNotFound         = 'Node "%s" not found';
  cALXmlInvalidNodeType      = 'Invalid node type';
  cALXmlNotSingleTextNode    = 'Element does not contain a single text node';
  cALXmlListCapacityError    = 'Node list capacity out of bounds (%d)';
  cALXmlListCountError       = 'Node list count out of bounds (%d)';
  cALXmlListIndexError       = 'Node list index out of bounds (%d)';
  cALXmlOperationError       = 'This operation can not be performed with a node of type %s';
  cALXmlParseError           = 'Xml Parse error';
  CALXMLOnlyOneTopLevelError = 'Only one top level element is allowed in an XML document';
  CALXMLOnlyOneChildError    = 'Only one child is allowed in an attribute node';
  CALXMLParameterIsIncorrect = 'The parameter is incorrect';

const
  cAlXMLUTF8EncodingStr = 'UTF-8';
  cALXmlUTF8HeaderStr   = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'#13#10;
  CALNSDelim            = ':';
  CALXML                = 'xml';
  CALVersion            = 'version';
  CALEncoding           = 'encoding';
  CALStandalone         = 'standalone';
  CALXmlDocument        = 'DOCUMENT';

var
  vALDefaultNodeIndent: ansiString; // var instead of const to avoid new ansitring on assign

type

  {class definition}
  TALXMLNode = Class;
  TALXMLNodeList= Class;
  TALXMLDocument= Class;

  TAlXMLParseDocument = reference to procedure (Sender: TObject);
  TAlXMLParseProcessingInstructionEvent = reference to procedure (Sender: TObject; const Path, Target, Data: AnsiString);
  TAlXMLParseTextEvent = reference to procedure (Sender: TObject; const Path, Str: AnsiString);
  TAlXMLParseStartElementEvent = reference to procedure (Sender: TObject; const Path, Name: AnsiString; Attributes: TALStrings);
  TAlXMLParseEndElementEvent = reference to procedure (Sender: TObject; const Path, Name: AnsiString);
  TALXMLNodeListSortCompare = reference to function(List: TALXMLNodeList; Index1, Index2: Integer): Integer;

  TALXmlNodeType = (ntElement,          //The node represents an element. Element nodes represent simple tags that have child nodes. The child nodes of an element node can have the following node types: ntElement, ntText, ntCData, ntEntityRef, ntProcessingInstr, and ntComment. Element nodes can also have attributes (ntAttribute). Element nodes can be the child of a node of type ntDocument, ntDocFragment, ntEntityRef, and ntElement.
                    ntAttribute,        //The node represents an attribute of an element. It is not the child of another node, but its value can be accessed using the Attribute property of the element node. An attribute node can have child nodes of type ntText and ntEntityRef.
                    ntText,             //The node represents the text content of a tag. A text node cannot have any child nodes, but can appear as the child node of a node of type ntAttribute, ntDocFragment, ntElement, or ntEntityRef.
                    ntCData,            //The node represents a CDATA section in the XML source. CDATA sections identify blocks of text that would otherwise be interpreted as markup. An ntCData node can’t have any child nodes. They can appear as the child of an ntDocFragment, ntEntityRef, or ntElement node.
                    ntEntityRef,        //[not implemented yet !] The node represents a reference to an entity in the XML document. This can be any type of entity, including character entity references. The children of an entity reference node can be of the following types: ntElement, ntProcessingInstr, ntComment, ntText, ntCData, and ntEntityRef. The entity reference node can appear as the child of an ntAttribute, ntDocFragment, ntElement, or ntEntityRef node.
                    ntEntity,           //[not implemented yet !] The node represents an expanded entity. Entity nodes can have child nodes that represent the expanded entity (for example, ntText and ntEntityRef nodes). Entity nodes only appear as the child of an ntDocType node.
                    ntProcessingInstr,  //The node represents a processing instruction (PI) from the XML document. A PI node cannot have any child nodes, but can appear as the child node of a node of type ntDocument, ntDocFragment, ntElement, or ntEntityRef.
                    ntComment,          //The node represents a comment in the XML document. Comment nodes do not have child nodes. They appear as the child of an ntDocument, ntDocFragment, ntElement, or ntEntityRef node.
                    ntDocument,         //The node represents a document object, which is the root of the entire XML document. Document nodes have a single ntElement node as a child (the DocumentElement). In addition, they can have child nodes of type ntProcessingInstr, ntComment, and ntDocType. Because the document is the root of the XML document, it never appears as a child node.
                    ntDocType,          //[not implemented yet !] The node represents the document type declaration, indicated by the <!DOCTYPE > tag. The document type node can child nodes of type ntNotation and ntEntity. It always appears as the child of the document node.
                    ntDocFragment,      //[not implemented yet !] The node represents a document fragment. A document fragment node associates a node or subtree with a document without actually being contained in the document. Document fragment nodes can have child nodes of type ntElement, ntProcessingInstr, ntComment, ntText, ntCData, and ntEntityRef. It never appears as the child of another node.
                    ntNotation);        //[not implemented yet !] A node represents a notation in the document type declaration. It always appears as the child of an ntDocType node and never has any child nodes.

  TALXMLDocOption = (//doAttrNull,      //[deleted from TALXMLDocOption] When reading the value of an attribute that does not exist, the value is given as a Null Variant (as opposed to a value of an empty string).
                     //doAutoSave,      //[deleted from TALXMLDocOption] When the XML document closes, any changes are automatically saved back to the XML document file or to the XML property.
                     doNodeAutoCreate,  //If the application tries to read a node by name, using the Nodes property of an IXMLNodeList interface, and that node does not exist, then the application creates a new node using the specified name.
                     doNodeAutoIndent); //When formatting the XML text from the parsed set of nodes, child nodes are automatically indented from their parent nodes.
  TALXMLDocOptions = set of TALXMLDocOption;

  TALXMLParseOption = (//poResolveExternals,   //[not implemented yet !] External definitions (resolvable namespaces, DTD external subsets, and external entity references) are resolved at parse time
                       //poValidateOnParse     //[not implemented yet !] The DOM parser validates the XML document against its schema information as well as verifying that it is well-formed XML.
                       //poAsyncLoad           //[deleted from TALXMLDocOption] The DOM parser works asynchronously. This can speed up performance, especially when parsing a large XML document, but can also lead to exceptions if the application tries to access nodes in the document before it is completely parsed.
                       poPreserveWhiteSpace,   //White space in the text of the XML document is not stripped off.
                       poIgnoreXMLReferences); //[added from TParseOption] don't decode xml entities (like &amp;) and not encode them also (when save / load)
  TALXMLParseOptions = set of TALXMLParseOption;

  TALXMLPrologItem = (xpVersion,
                      xpEncoding,
                      xpStandalone);

  TALXMLPointerList = array of TALXMLNode;

  {Exception}
  EALXMLDocError = class(Exception)
  end;

  {TALXMLNodeList}
  {TALXMLNodeList is used to represent a set of related nodes (TALXMLNode object) in an XML document. For example, TALXMLNodeList is used to
   represent all of the children of a node, or all of the attributes of a node. TALXMLNodeList can be used to add or delete nodes from the
   List, or to access specific nodes.}
  TALXMLNodeList = class(Tobject)
  Private
    //[Deleted from TXMLNodeList] FDefaultNamespaceURI: DOMString;
    //[Deleted from TXMLNodeList] FNotificationProc: TNodeListNotification;
    //[Deleted from TXMLNodeList] FUpdateCount: Integer;
    FCapacity: Integer; // [added from TXMLNodeList]
    FCount: integer; // [added from TXMLNodeList]
    FList: TALXMLPointerList; //[Replace from TXMLNodeList] FList: IInterfaceList;
    [weak] FOwner: TALXMLNode;
    procedure QuickSort(L, R: Integer; ACompare: TALXMLNodeListSortCompare); // [added from TXMLNodeList]
  protected
    //[Deleted from TXMLNodeList] function DoNotify(Operation: TNodeListOperation; const Node: IXMLNode; const IndexOrName: OleVariant; BeforeOperation: Boolean): IXMLNode;
    //[Deleted from TXMLNodeList] function InternalInsert(Index: Integer; const Node: TALXMLNode): Integer;
    //[Deleted from TXMLNodeList] property DefaultNamespaceURI: AnsiString read GetDefaultNamespaceURI;
    //[Deleted from TXMLNodeList] property List: IInterfaceList read FList;
    //[Deleted from TXMLNodeList] property NotificationProc: TNodeListNotification read FNotificationProc;
    procedure Grow; // [added from TXMLNodeList]
    procedure SetCapacity(NewCapacity: Integer); // [added from TXMLNodeList]
    procedure SetCount(NewCount: Integer); // [added from TXMLNodeList]
    property Owner: TALXMLNode read FOwner;
    function Get(Index: Integer): TALXMLNode;
    function GetNodeByIndex(Const Index: Integer): TALXMLNode; // [added from TXMLNodeList]
    function GetNodeByName(Const Name: AnsiString): TALXMLNode; // [added from TXMLNodeList]
    Function InternalInsert(Index: Integer; const Node: TALXMLNode): integer;
  public
    //[Deleted from TXMLNodeList] function Delete(const Name, NamespaceURI: AnsiString): Integer; overload;
    //[Deleted from TXMLNodeList] function FindNode(NodeName, NamespaceURI: AnsiString): TALXMLNode; overload;
    //[Deleted from TXMLNodeList] function FindNode(ChildNodeType: TGuid): IXMLNode; overload;
    //[Deleted from TXMLNodeList] function GetUpdateCount: Integer;
    //[Deleted from TXMLNodeList] function IndexOf(const Name, NamespaceURI: AnsiString): Integer; overload;
    //[Deleted from TXMLNodeList] procedure BeginUpdate;
    //[Deleted from TXMLNodeList] procedure EndUpdate;
    //[Deleted from TXMLNodeList] property UpdateCount: Integer read GetUpdateCount;
    constructor Create(Owner: TALXMLNode); //[Replace from TXMLNodeList] constructor Create(Owner: TXMLNode; const DefaultNamespaceURI: DOMString; NotificationProc: TNodeListNotification);
    destructor Destroy; override;
    procedure CustomSort(Compare: TALXMLNodeListSortCompare); // [added from TXMLNodeList]
    function Add(const Node: TALXMLNode): Integer;
    function Delete(const Index: Integer): Integer; overload;
    function Delete(const Name: AnsiString): Integer; overload;
    function Extract(const index: integer): TALXMLNode; overload; // [added from TXMLNodeList]
    function Extract(const Node: TALXMLNode): TALXMLNode; overload; // [added from TXMLNodeList]
    procedure Exchange(Index1, Index2: Integer); // [added from TXMLNodeList]
    function FindNode(const NodeName: AnsiString): TALXMLNode; overload;
    function FindNode(const NodeName: AnsiString; NodeAttributes: Array of ansiString): TALXMLNode; overload; // [added from TXMLNodeList]
    function FindSibling(const Node: TALXMLNode; Delta: Integer): TALXMLNode;
    function First: TALXMLNode;
    function IndexOf(const Name: AnsiString): Integer; overload;
    function IndexOf(const Node: TALXMLNode): Integer; overload;
    function Last: TALXMLNode;
    function Remove(const Node: TALXMLNode): Integer;
    function ReplaceNode(const OldNode, NewNode: TALXMLNode): TALXMLNode;
    procedure Clear;
    procedure Insert(Index: Integer; const Node: TALXMLNode);
    property Count: Integer read fCount;
    property Nodes[const Name: AnsiString]: TALXMLNode read GetNodeByName; default;
    property Nodes[const Index: integer]: TALXMLNode read GetNodeByIndex; default;
  end;

  {TALXMLNode}
  {TALXMLNode represents a node in an XML document.
   For example, if the XML document includes the following:
   <Address country="US">
     <Name>
       <First> John </First>
       <MI> Q. </MI>
       <Last> Public </Last>
     </Name>
     <Street> 123 Easy Street </Street>
     <City> Anytown </City>
     <State> CA </State>
   </Address>
   then the XML document generates TXMLNode descendants for the elements Address and Name. Child nodes and node attributes
   appear as properties of the TXMLNode descendant.}
  TALXMLNode = class(TObject)
  private
    //[Deleted from TXMLNode] FAttributeNodes: IXMLNodeList;
    //[Deleted from TXMLNode] FChildNodes: IXMLNodeList;
    //[Deleted from TXMLNode] FChildNodeClasses: TNodeClassArray;
    //[Deleted from TXMLNode] FCollection: TXMLNodeCollection;
    //[Deleted from TXMLNode] FDOMNode: IDOMNode;
    //[Deleted from TXMLNode] FHostNode: TXMLNode;
    //[Deleted from TXMLNode] FParentNode: TXMLNode;
    //[Deleted from TXMLNode] FHostedNodes: TXMLNodeArray;
    //[Deleted from TXMLNode] FIsDocElement: Boolean;
    //[Deleted from TXMLNode] FReadOnly: Boolean;
    //[Deleted from TXMLNode] FOnHostChildNotify: TNodeListNotification;
    //[Deleted from TXMLNode] FOnHostAttrNotify: TNodeListNotification;
    [weak] FDocument: TALXMLDocument;
  protected
    //[Deleted from TXMLNode] function _AddRef: Integer; stdcall;
    //[Deleted from TXMLNode] function _Release: Integer; stdcall;
    //[Deleted from TXMLNode] function GetHostedNodes: TALXMLNodeArray;
    //[Deleted from TXMLNode] function GetPrefixedName(const Name, NamespaceURI: AnsiString): AnsiString;
    //[Deleted from TXMLNode] function GetChildNodeClasses: TNodeClassArray;
    //[Deleted from TXMLNode] function GetCollection: TALXMLNodeCollection;
    //[Deleted from TXMLNode] function GetDOMNode: IDOMNode;
    //[Deleted from TXMLNode] function GetHostNode: TALXMLNode;
    //[Deleted from TXMLNode] function GetNamespaceURI: AnsiString;
    //[Deleted from TXMLNode] function GetNodeObject: TALXMLNode;
    //[Deleted from TXMLNode] function GetReadOnly: Boolean;
    //[Deleted from TXMLNode] function HasChildNode(const ChildTag, NamespaceURI: AnsiString): Boolean; overload;
    //[Deleted from TXMLNode] function HasChildNode(const ChildTag: AnsiString): Boolean; overload;
    //[Deleted from TXMLNode] procedure AddHostedNode(Node: TALXMLNode);
    //[Deleted from TXMLNode] procedure AttributeListNotify(Operation: TNodeListOperation; var Node: TALXMLNode; const IndexOrName: OleVariant; BeforeOperation: Boolean);
    //[Deleted from TXMLNode] procedure CheckNotHosted;
    //[Deleted from TXMLNode] procedure CheckReadOnly;
    //[Deleted from TXMLNode] procedure ChildListNotify(Operation: TNodeListOperation; var Node: TALXMLNode; const IndexOrName: OleVariant; BeforeOperation: Boolean); virtual;
    //[Deleted from TXMLNode] procedure DoNodeChange(ChangeType: TALNodeChange; BeforeOperation: Boolean);
    //[Deleted from TXMLNode] procedure RemoveHostedNode(Node: TALXMLNode);
    //[Deleted from TXMLNode] procedure SetCollection(const Value: TALXMLNodeCollection);
    //[Deleted from TXMLNode] procedure SetReadOnly(const Value: Boolean);
    //[Deleted from TXMLNode] property HostedNodes: TALXMLNodeArray read GetHostedNodes;
    //[Deleted from TXMLNode] property OnHostAttrNotify: TNodeListNotification read FOnHostAttrNotify write FOnHostAttrNotify;
    //[Deleted from TXMLNode] property OnHostChildNotify: TNodeListNotification read FOnHostChildNotify write FOnHostChildNotify;
    //[Deleted from TXMLNode] function CreateAttributeNode(const ADOMNode: IDOMNode): TALXMLNode;
    //[Deleted from TXMLNode] function CreateChildNode(const ADOMNode: IDOMNode): TALXMLNode;
    //[Deleted from TXMLNode] function CreateCollection(const CollectionClass: TALXMLNodeCollectionClass; const ItemIterface: TGuid; const ItemTag: AnsiString; ItemNS: AnsiString = ''): TALXMLNodeCollection;
    //[Deleted from TXMLNode] function DOMElement: IDOMElement;
    //[Deleted from TXMLNode] function FindHostedNode(const NodeClass: TALXMLNodeClass): TALXMLNode;
    //[Deleted from TXMLNode] procedure RegisterChildNode(const TagName: AnsiString; ChildNodeClass: TALXMLNodeClass; NamespaceURI: AnsiString = '');
    //[Deleted from TXMLNode] procedure RegisterChildNodes(const TagNames: array of AnsiString; const NodeClasses: array of TALXMLNodeClass);
    //[Deleted from TXMLNode] property ChildNodeClasses: TNodeClassArray read GetChildNodeClasses;
    //[Deleted from TXMLNode] property HostNode: TALXMLNode read GetHostNode;
    //[Deleted from TXMLNode] function GetChildValue(const IndexOrName: OleVariant): OleVariant;
    //[Deleted from TXMLNode] procedure SetChildValue(const IndexOrName, Value: OleVariant);
    //[Deleted from TXMLNode] function InternalAddChild(NodeClass: TALXMLNodeClass; const NodeName, NamespaceURI: AnsiString; Index: Integer): TALXMLNode;
    procedure DoBeforeChildNodesInsert(Index: Integer; const Node: TALXMLNode); virtual; // [added from TXMLNode]
    function CreateAttributeList: TALXMLNodeList;
    function CreateChildList: TALXMLNodeList;
    function InternalGetAttributeNodes: TALXMLNodeList; virtual; // [added from TXMLNode]
    function InternalGetChildNodes: TALXMLNodeList; virtual; // [added from TXMLNode]
    function GetInternalChildValue: AnsiString; virtual; // [added from TXMLNode]
    function GetInternalValue: AnsiString; virtual; // [added from TXMLNode]
    function GetAttribute(const AttrName: AnsiString): AnsiString;
    function GetAttributeNodes: TALXMLNodeList; virtual;
    function GetChildNodes: TALXMLNodeList; virtual;
    function GetHasChildNodes: Boolean;
    function GetIsTextElement: Boolean;
    function GetLocalName: AnsiString;
    function GetNodeName: AnsiString;
    function GetNodeType: TALXMLNodeType; virtual; abstract;
    function GetNodeTypeStr: AnsiString;
    function GetNodeValue: AnsiString;
    function GetOwnerDocument: TALXMLDocument;
    function GetParentNode: TALXMLNode; virtual;
    function GetPrefix: AnsiString;
    function GetText: AnsiString;
    function GetXML: AnsiString;
    procedure SetXML(const Value: AnsiString);  // [added from TXMLNode]
    function NestingLevel: Integer;
    procedure CheckTextNode;
    procedure SetAttributeNodes(const Value: TALXMLNodeList); virtual;
    procedure SetChildNodes(const Value: TALXMLNodeList); virtual;
    procedure SetInternalChildValue(const Value: AnsiString); virtual; // [added from TXMLNode]
    procedure SetInternalValue(const Value: AnsiString); virtual; // [added from TXMLNode]
    procedure SetNodeName(const Value: AnsiString); // [added from TXMLNode]
    procedure SetOwnerDocument(const Value: TALXMLDocument); // [added from TXMLNode]
    procedure SetParentNode(const Value: TALXMLNode); virtual;
    procedure SetNodeValue(const Value: AnsiString);
    procedure SetText(const Value: AnsiString);
    procedure SetAttribute(const AttrName: AnsiString; const Value: AnsiString);
    property InternalChildValue: AnsiString read GetInternalChildValue write SetInternalChildValue; // [added from TXMLNode]
    property InternalValue: AnsiString read GetInternalValue write SetInternalValue; // [added from TXMLNode]
  public
    //[Deleted from TXMLNode] constructor CreateHosted(HostNode: TALXMLNode);
    //[Deleted from TXMLNode] destructor Destroy; override;
    //[Deleted from TXMLNode] function AddChild(const TagName, NamespaceURI: AnsiString; GenPrefix: Boolean = False; Index: Integer = -1): TALXMLNode; overload;
    //[Deleted from TXMLNode] function AddChild(const TagName, NamespaceURI: AnsiString; NodeClass: TALXMLNodeClass; Index: Integer = -1): TALXMLNode; overload;
    //[Deleted from TXMLNode] function FindNamespaceDecl(const NamespaceURI: AnsiString): TALXMLNode;
    //[Deleted from TXMLNode] function FindNamespaceURI(const TagOrPrefix: AnsiString): AnsiString;
    //[Deleted from TXMLNode] function GetAttributeNS(const AttrName, NamespaceURI: AnsiString): OleVariant;
    //[Deleted from TXMLNode] function HasAttribute(const Name, NamespaceURI: AnsiString): Boolean; overload;
    //[Deleted from TXMLNode] procedure DeclareNamespace(const Prefix, URI: AnsiString);
    //[Deleted from TXMLNode] procedure Normalize;
    //[Deleted from TXMLNode] procedure Resync;
    //[Deleted from TXMLNode] procedure SetAttributeNS(const AttrName, NamespaceURI: AnsiString; const Value: OleVariant);
    //[Deleted from TXMLNode] procedure TransformNode(const stylesheet: TALXMLNode; const output: TALXMLDocument); overload;
    //[Deleted from TXMLNode] procedure TransformNode(const stylesheet: TALXMLNode; var output: AnsiString); overload;
    //[Deleted from TXMLNode] property Collection: TALXMLNodeCollection read FCollection write FCollection;
    //[Deleted from TXMLNode] property DOMNode: IDOMNode read GetDOMNode;
    //[Deleted from TXMLNode] property NamespaceURI: AnsiString read GetNameSpaceURI;
    //[Deleted from TXMLNode] property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    //[Deleted from TXMLNode] property ChildValues[const IndexOrName: OleVariant]: OleVariant read GetChildValue write SetChildValue; default;
    constructor Create(const NameOrData: AnsiString; const AddlData: AnsiString = ''); virtual; //[Replace from TXMLNode] constructor Create(const ADOMNode: IDOMNode; const AParentNode: TALXMLNode; const OwnerDoc: TXMLDocument);
    function CloneNode(Deep: Boolean): TALXMLNode;
    function AddChild(const TagName: AnsiString; Index: Integer = -1): TALXMLNode; overload;
    function HasAttribute(const Name: AnsiString): Boolean; overload;
    function NextSibling: TALXMLNode;
    function PreviousSibling: TALXMLNode;
    property AttributeNodes: TALXMLNodeList read GetAttributeNodes;
    property Attributes[const AttrName: AnsiString]: AnsiString read GetAttribute write SetAttribute;
    property ChildNodes: TALXMLNodeList read GetChildNodes write SetChildNodes;
    property HasChildNodes: Boolean read GetHasChildNodes;
    property IsTextElement: Boolean read GetIsTextElement;
    property LocalName: AnsiString read GetLocalName;
    property NodeName: AnsiString read GetNodeName write SetNodeName; //[Replace from TXMLNode] property NodeName: AnsiString;
    property NodeType: TALXMLNodeType read GetNodeType;
    property NodeValue: AnsiString read GetNodeValue write SetNodeValue;
    property OwnerDocument: TALXMLDocument read GetOwnerDocument Write SetOwnerDocument;
    property ParentNode: TALXMLNode read GetParentNode;
    property Prefix: AnsiString read GetPrefix;
    property Text: AnsiString read GetText write SetText;
    property XML: AnsiString read GetXML write SetXML;
    procedure SaveToStream(const Stream: TStream; Const SaveOnlyChildNodes: Boolean=False); // [added from TXMLNode]
    procedure SaveToFile(const AFileName: AnsiString; Const SaveOnlyChildNodes: Boolean=False); // [added from TXMLNode]
    procedure SaveToXML(var Xml: AnsiString; Const SaveOnlyChildNodes: Boolean=False); // [added from TXMLNode]
    procedure LoadFromFile(const AFileName: AnsiString; Const FileContainOnlyChildNodes: Boolean=False; Const ClearChildNodes: Boolean = True); // [added from TXMLNode]
    procedure LoadFromStream(const Stream: TStream; Const StreamContainOnlyChildNodes: Boolean=False; Const ClearChildNodes: Boolean = True); // [added from TXMLNode]
    procedure LoadFromXML(const Xml: AnsiString; Const XmlContainOnlyChildNodes: Boolean=False; Const ClearChildNodes: Boolean = True); // [added from TXMLNode]
  end;

  {The node represents an element. Element nodes represent simple tags that have child nodes.
   The child nodes of an element node can have the following node types: ntElement, ntText,
   ntCData, ntEntityRef, ntProcessingInstr, and ntComment. Element nodes can also have attributes
   (ntAttribute). Element nodes can be the child of a node of type ntDocument, ntDocFragment,
   ntEntityRef, and ntElement.}
  TALXmlElementNode = Class(TALXMLNode)
  private
    FAttributeNodes: TALXMLNodeList;
    FChildNodes: TALXMLNodeList;
    [weak] FParentNode: TALXMLNode;
    FInternalValue: AnsiString;
  protected
    procedure DoBeforeChildNodesInsert(Index: Integer; const Node: TALXMLNode); override; // [added from TXMLNode]
    function GetNodeType: TALXMLNodeType; override;
    function GetAttributeNodes: TALXMLNodeList; override;
    function GetChildNodes: TALXMLNodeList; override;
    function GetParentNode: TALXMLNode; override;
    function GetInternalValue: AnsiString; override;
    procedure SetAttributeNodes(const Value: TALXMLNodeList); override;
    procedure SetChildNodes(const Value: TALXMLNodeList); override;
    procedure SetParentNode(const Value: TALXMLNode); override;
    procedure SetInternalValue(const Value: AnsiString); override;
    function InternalGetAttributeNodes: TALXMLNodeList; override;
    function InternalGetChildNodes: TALXMLNodeList; override;
  public
    constructor Create(const NameOrData: AnsiString); reintroduce;
    Destructor Destroy; override;
  end;

  {The node represents an attribute of an element. It is not the child of another node, but
   its value can be accessed using the Attribute property of the element node. An attribute
   node can have child nodes of type ntText and ntEntityRef.}
  TALXmlAttributeNode = Class(TALXMLNode)
  private
    FChildNodes: TALXMLNodeList;
    FInternalValue: AnsiString;
  protected
    procedure DoBeforeChildNodesInsert(Index: Integer; const Node: TALXMLNode); override; // [added from TXMLNode]
    function GetNodeType: TALXMLNodeType; override;
    function GetChildNodes: TALXMLNodeList; override;
    function GetInternalValue: AnsiString; override;
    procedure SetChildNodes(const Value: TALXMLNodeList); override;
    procedure SetInternalValue(const Value: AnsiString); override;
    function InternalGetChildNodes: TALXMLNodeList; override;
  public
    constructor Create(const NameOrData: AnsiString); reintroduce;
    Destructor Destroy; override;
  end;

  {The node represents the text content of a tag. A text node cannot have any child nodes,
   but can appear as the child node of a node of type ntAttribute, ntDocFragment, ntElement,
   or ntEntityRef.}
  TALXmlTextNode = Class(TALXMLNode)
  private
    [weak] FParentNode: TALXMLNode;
    FInternalValue: AnsiString;
  protected
    function GetNodeType: TALXMLNodeType; override;
    function GetParentNode: TALXMLNode; override;
    function GetInternalValue: AnsiString; override;
    procedure SetParentNode(const Value: TALXMLNode); override;
    procedure SetInternalValue(const Value: AnsiString); override;
  public
    constructor Create(const NameOrData: AnsiString); reintroduce;
    Destructor Destroy; override;
  end;

  {The node represents a document object, which is the root of the entire XML document.
   Document nodes have a single ntElement node as a child (the DocumentElement). In addition,
   they can have child nodes of type ntProcessingInstr, ntComment, and ntDocType. Because the
   document is the root of the XML document, it never appears as a child node.}
  TALXmlDocumentNode = Class(TALXmlNode)
  private
    FChildNodes: TALXMLNodeList;
  protected
    procedure DoBeforeChildNodesInsert(Index: Integer; const Node: TALXMLNode); override;
    function GetNodeType: TALXMLNodeType; override;
    function GetChildNodes: TALXMLNodeList; override;
    procedure SetChildNodes(const Value: TALXMLNodeList); override;
    function InternalGetChildNodes: TALXMLNodeList; override;
  public
    constructor Create(const OwnerDoc: TALXMLDocument); reintroduce;
    Destructor Destroy; override;
  end;

  {The node represents a comment in the XML document. Comment nodes do not have child nodes.
   They appear as the child of an ntDocument, ntDocFragment, ntElement, or ntEntityRef node.}
  TALXmlCommentNode = Class(TALXMLNode)
  private
    [weak] FParentNode: TALXMLNode;
    FInternalValue: AnsiString;
  protected
    function GetNodeType: TALXMLNodeType; override;
    function GetParentNode: TALXMLNode; override;
    function GetInternalValue: AnsiString; override;
    procedure SetParentNode(const Value: TALXMLNode); override;
    procedure SetInternalValue(const Value: AnsiString); override;
  public
    constructor Create(const NameOrData: AnsiString); reintroduce;
    Destructor Destroy; override;
  end;

  {The node represents a processing instruction (PI) from the XML document. A PI node cannot
   have any child nodes, but can appear as the child node of a node of type ntDocument,
   ntDocFragment, ntElement, or ntEntityRef.}
  TALXmlProcessingInstrNode = Class(TALXMLNode)
  private
    [weak] FParentNode: TALXMLNode;
    FInternalValue: AnsiString;
    FInternalChildValue: AnsiString;
  protected
    function GetNodeType: TALXMLNodeType; override;
    function GetParentNode: TALXMLNode; override;
    Function GetInternalChildValue: AnsiString; override;
    function GetInternalValue: AnsiString; override;
    procedure SetParentNode(const Value: TALXMLNode); override;
    procedure SetInternalChildValue(const Value: AnsiString); override;
    procedure SetInternalValue(const Value: AnsiString); override;
  public
    constructor Create(const NameOrData: AnsiString; const AddlData: AnsiString = ''); override;
    Destructor Destroy; override;
  end;

  {The node represents a CDATA section in the XML source. CDATA sections identify blocks of
   text that would otherwise be interpreted as markup. An ntCData node can’t have any child
   nodes. They can appear as the child of an ntDocFragment, ntEntityRef, or ntElement node.}
  TALXmlCDataNode = Class(TALXMLNode)
  private
    [weak] FParentNode: TALXMLNode;
    FInternalValue: AnsiString;
  protected
    function GetNodeType: TALXMLNodeType; override;
    function GetParentNode: TALXMLNode; override;
    function GetInternalValue: AnsiString; override;
    procedure SetParentNode(const Value: TALXMLNode); override;
    procedure SetInternalValue(const Value: AnsiString); override;
  public
    constructor Create(const NameOrData: AnsiString); reintroduce;
    Destructor Destroy; override;
  end;

  //[not implemented yet !]
  {The node represents a reference to an entity in the XML document. This can be any type of
   entity, including character entity references. The children of an entity reference node can
   be of the following types: ntElement, ntProcessingInstr, ntComment, ntText, ntCData, and
   ntEntityRef. The entity reference node can appear as the child of an ntAttribute, ntDocFragment,
   ntElement, or ntEntityRef node.}
  //TALXmlEntityRefNode = Class(TALXMLNode)
  //private
  //protected
  //public
  //end;

  //[not implemented yet !]
  {The node represents an expanded entity. Entity nodes can have child nodes that represent the
   expanded entity (for example, ntText and ntEntityRef nodes). Entity nodes only appear as the
   child of an ntDocType node.}
  //TALXmlEntityNode = Class(TALXMLNode)
  //private
  //protected
  //public
  //end;

  //[not implemented yet !]
  {The node represents the document type declaration, indicated by the <!DOCTYPE > tag. The document
   type node can child nodes of type ntNotation and ntEntity. It always appears as the child of the
   document node.}
  //TALXmlDocTypeNode = Class(TALXMLNode)
  //private
  //protected
  //public
  //end;

  //[not implemented yet !]
  {The node represents a document fragment. A document fragment node associates a node or subtree
   with a document without actually being contained in the document. Document fragment nodes can
   have child nodes of type ntElement, ntProcessingInstr, ntComment, ntText, ntCData, and ntEntityRef.
   It never appears as the child of another node.}
  //TALXmlDocFragmentNode = Class(TALXMLNode)
  //private
  //protected
  //public
  //end;

  //[not implemented yet !]
  {A node represents a notation in the document type declaration. It always appears as the child of an
   ntDocType node and never has any child nodes.}
  //TALXmlNotationNode = Class(TALXMLNode)
  //private
  //protected
  //public
  //end;

  {TALXMLDocument}
  TALXMLDocument = class(TObject)
  private
    //[Deleted from TXMLDocument] FXMLData: AnsiString;
    //[Deleted from TXMLDocument] FDOMVendor: TDOMVendor;
    //[Deleted from TXMLDocument] FRefCount: Integer;
    //[Deleted from TXMLDocument] FDocBindingInfo: TNodeClassArray;
    //[Deleted from TXMLDocument] FAfterNodeChange: TNodeChangeEvent;
    //[Deleted from TXMLDocument] FBeforeNodeChange: TNodeChangeEvent;
    //[Deleted from TXMLDocument] FDOMDocument: IDOMDocument;
    //[Deleted from TXMLDocument] FDOMImplementation: IDOMImplementation;
    //[Deleted from TXMLDocument] FDOMParseOptions: IDOMParseOptions;
    //[Deleted from TXMLDocument] FDOMPersist: IDOMPersist;
    //[Deleted from TXMLDocument] FModified: Integer;
    //[Deleted from TXMLDocument] FNSPrefixBase: AnsiString;
    //[Deleted from TXMLDocument] FOnAsyncLoad: TAsyncEventHandler;
    //[Deleted from TXMLDocument] FOwnerIsComponent: Boolean;
    //[Deleted from TXMLDocument] FPrefixID: Integer;
    //[Deleted from TXMLDocument] FXMLStrRead: Integer;
    //[Deleted from TXMLDocument] function GetDOMParseOptions: IDOMParseOptions;
    //[Deleted from TXMLDocument] procedure SetDOMImplementation(const Value: IDOMImplementation);
    //[Deleted from TXMLDocument] procedure SetDOMVendor(const Value: TDOMVendor);
    //[Deleted from TXMLDocument] function IsXMLStored: Boolean;
    //[Deleted from TXMLDocument] function NodeIndentStored: Boolean;
    //[Deleted from TXMLDocument] FXMLStrings: TALStringList;
    //[Deleted from TXMLDocument] FFileName: AnsiString;
    //[Deleted from TXMLDocument] FDocSource: TalXMLDocumentSource;
    //[Deleted from TXMLDocument] FSrcStream: TStream;
    //[Deleted from TXMLDocument] FAfterClose: TNotifyEvent;
    //[Deleted from TXMLDocument] FAfterOpen: TNotifyEvent;
    //[Deleted from TXMLDocument] FBeforeClose: TNotifyEvent;
    //[Deleted from TXMLDocument] FBeforeOpen: TNotifyEvent;
    FTag: NativeInt;
    FDocumentNode: TALXMLNode;
    FNodeIndentStr: AnsiString;
    FOptions: TALXMLDocOptions;
    FParseOptions: TALXMLParseOptions;
    fPathSeparator: ansiChar;
    FOnParseProcessingInstruction: TAlXMLParseProcessingInstructionEvent; // [added from TXMLDocument]
    FOnParseStartDocument: TAlXMLParseDocument; // [added from TXMLDocument]
    FOnParseEndDocument: TAlXMLParseDocument; // [added from TXMLDocument]
    FOnParseStartElement: TAlXMLParseStartElementEvent; // [added from TXMLDocument]
    FOnParseEndElement: TAlXMLParseEndElementEVent; // [added from TXMLDocument]
    FonParseText: TAlXMLParseTextEvent; // [added from TXMLDocument]
    FonParseComment: TAlXMLParseTextEvent; // [added from TXMLDocument]
    FonParseCData: TAlXMLParseTextEvent; // [added from TXMLDocument]
  protected
    //[Deleted from TXMLDocument] function _AddRef: Integer; stdcall;
    //[Deleted from TXMLDocument] function _Release: Integer; stdcall;
    //[Deleted from TXMLDocument] function GetChildNodeClass(const Node: IDOMNode): TXMLNodeClass; virtual;
    //[Deleted from TXMLDocument] function GetDocumentObject: TALXMLDocument;
    //[Deleted from TXMLDocument] function GetDOMPersist: IDOMPersist;
    //[Deleted from TXMLDocument] procedure AssignParseOptions;
    //[Deleted from TXMLDocument] procedure CheckDOM; dynamic;
    //[Deleted from TXMLDocument] procedure DefineProperties(Filer: TFiler); override;
    //[Deleted from TXMLDocument] procedure DoNodeChange(const Node: TALXMLNode; ChangeType: TNodeChange; BeforeOperation: Boolean);
    //[Deleted from TXMLDocument] procedure ReadDOMVendor(Reader: TReader);
    //[Deleted from TXMLDocument] procedure SaveToUTF8String(var XML: string);
    //[Deleted from TXMLDocument] procedure SetModified(const Value: Boolean);
    //[Deleted from TXMLDocument] procedure WriteDOMVendor(Writer: TWriter);
    //[Deleted from TXMLDocument] function GetSchemaRef: AnsiString;
    //[Deleted from TXMLDocument] function GetAsyncLoadState: Integer;
    //[Deleted from TXMLDocument] function GetDOMDocument: IDOMDocument;
    //[Deleted from TXMLDocument] function GetModified: Boolean;
    //[Deleted from TXMLDocument] procedure SetDOMDocument(const Value: IDOMDocument);
    //[Deleted from TXMLDocument] procedure SetOnAsyncLoad(const Value: TAsyncEventHandler);
    //[Deleted from TXMLDocument] property DocBindingInfo: TNodeClassArray read FDocBindingInfo;
    //[Deleted from TXMLDocument] property DOMParseOptions: IDOMParseOptions read GetDOMParseOptions;
    //[Deleted from TXMLDocument] property DOMPersist: IDOMPersist read GetDOMPersist;
    //[Deleted from TXMLDocument] property PrefixID: Integer read FPrefixID write FPrefixID;
    //[Deleted from TXMLDocument] property DocumentObject: TXMLDocument read GetDocumentObject;
    //[Deleted from TXMLDocument] procedure CheckAutoSave;
    //[Deleted from TXMLDocument] procedure SaveToXMLStrings;
    //[Deleted from TXMLDocument] procedure SetXMLStrings(const Value: AnsiString);
    //[Deleted from TXMLDocument] procedure XMLStringsChanging(Sender: TObject);
    //[Deleted from TXMLDocument] function GetFileName: AnsiString;
    //[Deleted from TXMLDocument] procedure SetFileName(const Value: AnsiString);
    //[Deleted from TXMLDocument] property DocSource: TALXMLDocumentSource read FDocSource write FDocSource;
    //[Deleted from TXMLDocument] procedure DoAfterClose;
    //[Deleted from TXMLDocument] procedure DoAfterOpen;
    //[Deleted from TXMLDocument] procedure DoBeforeClose;
    //[Deleted from TXMLDocument] procedure DoBeforeOpen;
    function GetPrologNode: TALXMLNode;
    function GetPrologValue(PrologItem: TALXMLPrologItem; const Default: AnsiString = ''): AnsiString;
    function InternalSetPrologValue(const PrologNode: TALXMLNode; const Value: AnsiString; PrologItem: TALXMLPrologItem): AnsiString; //[Replace from TXMLNodeList] function InternalSetPrologValue(const PrologNode: IXMLNode; const Value: Variant; PrologItem: TXMLPrologItem): string;
    procedure CheckActive;
    procedure DoParseProcessingInstruction(const Path, Target, Data: AnsiString); // [added from TXMLDocument]
    procedure DoParseStartDocument; // [added from TXMLDocument]
    procedure DoParseEndDocument; // [added from TXMLDocument]
    procedure DoParseStartElement(const Path, Name: AnsiString; Attributes: TALStrings); // [added from TXMLDocument]
    procedure DoParseEndElement(const Path, Name: AnsiString); // [added from TXMLDocument]
    procedure DoParseText(const Path, Str: AnsiString); // [added from TXMLDocument]
    procedure DoParseComment(const Path, Str: AnsiString); // [added from TXMLDocument]
    procedure DoParseCData(const Path, Str: AnsiString); // [added from TXMLDocument]
    Procedure ParseXmlStream(RawXmlStream: TStream; ContainerNode: TALXmlNode; StreamContainOnlyChildNodes: Boolean=False); // [added from TXMLDocument]
    procedure ReleaseDoc;
    procedure SetPrologValue(const Value: AnsiString; PrologItem: TALXMLPrologItem); //[Replace from TXMLNodeList] procedure SetPrologValue(const Value: Variant; PrologItem: TXMLPrologItem);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetChildNodes: TALXMLNodeList;
    function GetDocumentElement: TALXMLNode;
    function GetDocumentNode: TALXMLNode;
    function GetEncoding: AnsiString;
    function GetNodeIndentStr: AnsiString;
    function GetOptions: TALXMLDocOptions;
    function GetParseOptions: TALXMLParseOptions;
    function GetPathSeparator: AnsiChar;
    procedure SetPathSeparator(const Value: ansiChar);
    function GetStandAlone: AnsiString;
    function GetVersion: AnsiString;
    function GetXML: AnsiString; //[Replace from TXMLDocument] function GetXML: TALStrings;
    procedure SetDocumentElement(const Value: TALXMLNode);
    procedure SetOptions(const Value: TALXMLDocOptions);
    procedure SetParseOptions(const Value: TALXMLParseOptions);
    procedure SetStandAlone(const Value: AnsiString);
    procedure SetVersion(const Value: AnsiString);
    procedure SetXML(const Value: ansiString); //[Replace from TXMLDocument] procedure SetXML(const Value: TALStrings);
    procedure SetEncoding(const Value: AnsiString);
    procedure SetNodeIndentStr(const Value: AnsiString);
  public
    //[Deleted from TXMLDocument] class function NewInstance: TObject; override;
    //[Deleted from TXMLDocument] function AddChild(const TagName, NamespaceURI: AnsiString): TALXMLNode; overload;
    //[Deleted from TXMLDocument] function GetDocBinding(const TagName: AnsiString; DocNodeClass: TClass; NamespaceURI: AnsiString = ''): TALXMLNode;
    //[Deleted from TXMLDocument] procedure LoadFromXML(const XML: WideString); overload;
    //[Deleted from TXMLDocument] procedure RegisterDocBinding(const TagName: AnsiString; DocNodeClass: TClass; NamespaceURI: AnsiString = '');
    //[Deleted from TXMLDocument] procedure Resync;
    //[Deleted from TXMLDocument] procedure SaveToXML(var XML: WideString); overload;
    //[Deleted from TXMLDocument] property AsyncLoadState: Integer read GetAsyncLoadState;
    //[Deleted from TXMLDocument] property DOMDocument: IDOMDocument read GetDOMDocument write SetDOMDocument;
    //[Deleted from TXMLDocument] property DOMImplementation: IDOMImplementation read FDOMImplementation write SetDOMImplementation;
    //[Deleted from TXMLDocument] function GeneratePrefix(const Node: TALXMLNode): AnsiString;
    //[Deleted from TXMLDocument] property Modified: Boolean read GetModified;
    //[Deleted from TXMLDocument] property NSPrefixBase: AnsiString read FNSPrefixBase write FNSPrefixBase;
    //[Deleted from TXMLDocument] property SchemaRef: AnsiString read GetSchemaRef;
    //[Deleted from TXMLDocument] procedure Refresh;
    //[Deleted from TXMLDocument] property XML: TALStrings read GetXML write SetXML;
    //[Deleted from TXMLDocument] property FileName: AnsiString read GetFileName write SetFileName;
    //[Deleted from TXMLDocument] procedure parseXML;
    //[Deleted from TXMLDocument] property BeforeOpen: TNotifyEvent read FBeforeOpen write FBeforeOpen;
    //[Deleted from TXMLDocument] property AfterOpen: TNotifyEvent read FAfterOpen write FAfterOpen;
    //[Deleted from TXMLDocument] property BeforeClose: TNotifyEvent read FBeforeClose write FBeforeClose;
    //[Deleted from TXMLDocument] property AfterClose: TNotifyEvent read FAfterClose write FAfterClose;
    //[Deleted from TXMLDocument] property DOMVendor: TDOMVendor read FDOMVendor write SetDOMVendor;
    //[Deleted from TXMLDocument] property BeforeNodeChange: TNodeChangeEvent read FBeforeNodeChange write FBeforeNodeChange;
    //[Deleted from TXMLDocument] property AfterNodeChange: TNodeChangeEvent read FAfterNodeChange write FAfterNodeChange;
    //[Deleted from TXMLDocument] property OnAsyncLoad: TAsyncEventHandler read FOnAsyncLoad write SetOnAsyncLoad;
    constructor Create(const aActive: Boolean = True); overload; virtual; //[Replace from TXMLDocument]  constructor Create
    constructor Create(const Rootname:AnsiString; const EncodingStr: AnsiString = cAlXMLUTF8EncodingStr); overload; virtual; // [added from TXMLDocument]
    destructor Destroy; override;
    procedure MultiThreadPrepare; // [added from TXMLDocument]
    procedure Clear(const Rootname:AnsiString; const EncodingStr: AnsiString = cAlXMLUTF8EncodingStr); // [added from TXMLDocument]
    function AddChild(const TagName: AnsiString): TALXMLNode;
    function CreateElement(const TagOrData: AnsiString): TALXMLNode;  //[Replace from TXMLDocument] function CreateElement(const TagOrData, NamespaceURI: AnsiString): TALXMLNode;
    function CreateNode(const NameOrData: AnsiString; NodeType: TALXMLNodeType = ntElement; const AddlData: AnsiString = ''): TALXMLNode;
    function IsEmptyDoc: Boolean;
    procedure LoadFromFile(const AFileName: AnsiString; const saxMode: Boolean = False); //[Replace from TXMLDocument]  procedure LoadFromFile(const AFileName: AnsiString = '');
    procedure LoadFromStream(const Stream: TStream; const saxMode: Boolean = False); //[Replace from TXMLDocument] procedure LoadFromStream(const Stream: TStream; EncodingType: TALXMLEncodingType = xetUnknown);
    procedure LoadFromXML(const XML: AnsiString; const saxMode: Boolean = False); //[Replace from TXMLDocument]  procedure LoadFromXML(const XML: AnsiString); overload;
    procedure SaveToFile(const AFileName: AnsiString);
    procedure SaveToStream(const Stream: TStream);
    procedure SaveToXML(var XML: AnsiString);
    property ChildNodes: TALXMLNodeList read GetChildNodes;
    property DocumentElement: TALXMLNode read GetDocumentElement write SetDocumentElement;
    property Encoding: AnsiString read GetEncoding write SetEncoding;
    property Node: TALXMLNode read GetDocumentNode;
    property StandAlone: AnsiString read GetStandAlone write SetStandAlone;
    property Version: AnsiString read GetVersion write SetVersion;
    property Active: Boolean read GetActive write SetActive;
    property NodeIndentStr: AnsiString read GetNodeIndentStr write SetNodeIndentStr;
    property Options: TALXMLDocOptions read GetOptions write SetOptions;
    property ParseOptions: TALXMLParseOptions read GetParseOptions write SetParseOptions;
    property PathSeparator: ansiChar read GetPathSeparator write SetPathSeparator;
    property XML: AnsiString read GetXML write SetXML;
    property OnParseProcessingInstruction: TAlXMLParseProcessingInstructionEvent read FOnParseProcessingInstruction write FOnParseProcessingInstruction; // [added from TXMLDocument]
    property OnParseStartDocument: TAlXMLParseDocument read FOnParseStartDocument write FOnParseStartDocument; // [added from TXMLDocument]
    property OnParseEndDocument: TAlXMLParseDocument read FOnParseEndDocument write FOnParseEndDocument; // [added from TXMLDocument]
    property OnParseStartElement: TAlXMLParseStartElementEvent read FOnParseStartElement write FOnParseStartElement; // [added from TXMLDocument]
    property OnParseEndElement: TAlXMLParseEndElementEVent read FOnParseEndElement Write FOnParseEndElement; // [added from TXMLDocument]
    property OnParseText: TAlXMLParseTextEvent read FonParseText Write FonParseText; // [added from TXMLDocument]
    property OnParseComment: TAlXMLParseTextEvent read FonParseComment Write FonParseComment; // [added from TXMLDocument]
    property OnParseCData: TAlXMLParseTextEvent read FonParseCData Write FonParseCData; // [added from TXMLDocument]
    property Tag: NativeInt read FTag write FTag;
  end;

{misc function}
Function  ALFindXmlNodeByChildNodeValue(xmlrec:TalxmlNode;
                                        Const ChildNodeName, ChildNodeValue : AnsiString;
                                        Const Recurse: Boolean = False): TalxmlNode;
Function  ALFindXmlNodeByNameAndChildNodeValue(xmlrec:TalxmlNode;
                                               Const NodeName: ansiString;
                                               Const ChildNodeName, ChildNodeValue: AnsiString;
                                               Const Recurse: Boolean = False): TalxmlNode;
Function  ALFindXmlNodeByAttribute(xmlrec:TalxmlNode;
                                   Const AttributeName, AttributeValue : AnsiString;
                                   Const Recurse: Boolean = False): TalxmlNode;
Function  ALFindXmlNodeByNameAndAttribute(xmlrec:TalxmlNode;
                                          Const NodeName: ansiString;
                                          Const AttributeName, AttributeValue: AnsiString;
                                          Const Recurse: Boolean = False): TalxmlNode;
function  ALExtractAttrValue(const AttrName, AttrLine: AnsiString; const Default: AnsiString = ''): AnsiString;

implementation

uses
  System.Math,
  System.Contnrs,
  AlHTML,
  ALHttpClient,
  ALCommon,
  ALString;

{**********************************}
{Raises an EALXMLDocError exception.
 Call ALXMLDocError to raise an EALXMLDocError exception. Using ALXMLDocError rather than explicitly creating
 the EALXMLDocError instance can result in more space-efficient code.
 *Msg is the error message associated with the EALXMLDocError instance.}
procedure ALXMLDocError(const Msg: String); overload;
begin
  raise EALXMLDocError.Create(Msg);
end;

{**********************************}
{Raises an EALXMLDocError exception.
 Call ALXMLDocError to raise an EALXMLDocError exception. Using ALXMLDocError rather than explicitly creating
 the EALXMLDocError instance can result in more space-efficient code.
 *Msg is the error message associated with the EALXMLDocError instance.
 *Args supplies arguments for any format specifiers embedded in Msg.
 Args_Size is the index of the last entry in the Args array. That is one less than the total number of arguments.}
procedure ALXMLDocError(const Msg: String; const Args: array of const); overload;
begin
  raise EALXMLDocError.CreateFmt(Msg, Args);
end;

{****************************************************************************}
{Indicates whether a specified node matches a given tag name. Call NodeMatches
 to determine whether the node with the object specified by Node refers to the same node as is described by the TagName
 NodeMatches returns true if The value of the local name or tag name of Node is the same as the TagName parameter.}
function ALNodeMatches(const Node: TALXMlNode; const TagName: AnsiString): Boolean;
begin
  Result := (Node.NodeName = TagName) or (Node.LocalName = TagName);
end;

{******************************************************}
{Returns the namespace prefix of an XML node’s tag name.
 Call ExtractPrefix to determine the namespace prefix, if any, of a full tag name for an XML node.
 *AName is the tag name of the XML node.
 ExtractPrefix returns the namespace prefix, not including the colon (:) that separates the namespace from the local name of
 the XML node. If the tag name does not include a namespace prefix, ExtractPrefix returns an empty string.}
function ALExtractPrefix(const AName: AnsiString): AnsiString;
var LSepPos: Integer;
begin
  LSepPos := ALPos(CALNSDelim,Aname);
  if LSepPos > 0 then begin
    setlength(Result,LSepPos - 1);
    ALMove(pointer(aName)^, pointer(Result)^, LSepPos - 1);
  end
  else result := '';
end;

{*************************************************************************}
{Strips the namespace prefix, if present, from the tag name of an XML node.
 Call ExtractLocalName to convert a full tag name for an XML node into the corresponding local name.
 *AName is the tag name to be converted.
 ExtractLocalName returns the value of AName with any namespace prefix stripped away.}
function ALExtractLocalName(const AName: AnsiString): AnsiString;
var LSepPos: Integer;
    LLength: integer;
begin
  LSepPos := ALPos(CALNSDelim,Aname);
  if LSepPos > 0 then begin
    LLength := Length(Aname) - LSepPos;
    setlength(Result,LLength);
    ALMove(pbyte(aName)[LSepPos], pointer(Result)^, LLength);
  end
  else begin
    LLength := Length(Aname);
    setlength(Result,LLength);
    ALMove(pointer(aName)^, pointer(Result)^, LLength);
  end;
end;

{*******************************************************************************************}
{Call CreateNode to create a new generic XML node. The resulting node does not have a parent,
 but can be added to the ChildNodes or AttributeNodes list of any node in the document.
 NameOrData provides the tag name or value of the newly created node. Its interpretation depends on
 the type of node created, as indicated in the following table:

 NodeType           NameOrData
 ntElement	        The tag name. (ex: aName or xi:aName)
 ntAttribute	      The attribute name. (ex: aName or xi:aName)
 ntText	            The value of the node.
 ntCData	          The value of the CDATA section.
 ntEntityRef	      The name of the referenced entity.
 ntProcessingInstr	The target of the processing instruction.
 ntComment	        The value (text) of the comment.
 ntDocFragment	    Not used.

 NodeType indicates the type of node to create. It can only be one of the types listed in the
 previous table. The meaning of AddlData depends on the node type, as indicated in
 the following table:

 NodeType	          AddlData
 ntProcessingInstr	The content of the processing instruction, except for the target.

 CreateNode returns the interface for the new node.}
function ALCreateXmlNode(const NameOrData: AnsiString;
                         NodeType: TALXMLNodeType = ntElement;
                         const AddlData: AnsiString = ''): TALXMLNode;
begin
  case NodeType of
    ntElement:         Result := TALXmlElementNode.Create(NameOrData);
    ntAttribute:       Result := TALXmlattributeNode.Create(NameOrData);
    ntText:            Result := TALXmlTextNode.Create(NameOrData);
    ntCData:           Result := TALXmlCDataNode.Create(NameOrData);
    ntEntityRef:       Result := nil;//todo
    ntProcessingInstr: Result := TALXmlProcessingInstrNode.Create(NameOrData, addlData);
    ntComment:         Result := TALXmlCommentNode.Create(NameOrData);
    ntDocFragment:     Result := nil;//todo
    else begin
      Result := nil; //for hide warning
      AlXMLDocError(cAlXmlInvalidNodeType);
    end;
  end;
end;

{************************************************************************************************************}
function ALExtractAttrValue(const AttrName, AttrLine: AnsiString; const Default: AnsiString = ''): AnsiString;
var LineLen, ItemPos, ItemEnd: Integer;
begin
  ItemPos := ALPos(AttrName, AttrLine);
  LineLen := Length(AttrLine);
  if ItemPos > 0 then begin
    Inc(ItemPos, Length(AttrName));
    while (ItemPos < LineLen) and not (AttrLine[ItemPos] in ['''','"']) do Inc(ItemPos);
    if ItemPos < LineLen then begin
      ItemEnd := ItemPos + 1;
      while (ItemEnd < LineLen) and not (AttrLine[ItemEnd] in ['''','"']) do Inc(ItemEnd);
      Result := ALCopyStr(AttrLine, ItemPos+1, ItemEnd-ItemPos-1);
    end;
  end
  else Result := Default;
end;

{*************************************************************************************}
procedure ALAppendItem(var AttrStr: AnsiString; const AttrName, AttrValue: AnsiString);
begin
  if AttrValue <> '' then begin
    if AttrStr <> '' then AttrStr := AttrStr + ' ' + AttrName + '="' + AttrValue + '"'
    else AttrStr := AttrStr + AttrName + '="' + AttrValue + '"';
  end;
end;

{************************************}
{Instantiates a TALXMLDocument object.
 Call Create to instantiate a TALXMLDocument component at runtime.}
constructor TALXMLDocument.create(const aActive: Boolean = True);
begin
  inherited create;
  FDocumentNode:= nil;
  FParseOptions:= [];
  fPathSeparator := '.';
  FOnParseProcessingInstruction:= nil;
  FOnParseStartDocument:= nil;
  FOnParseEndDocument:= nil;
  FOnParseStartElement:= nil;
  FOnParseEndElement:= nil;
  FonParseText:= nil;
  FonParseComment:= nil;
  FonParseCData:= nil;
  FOptions := [];
  NodeIndentStr := vALDefaultNodeIndent;
  FTag := 0;
  SetActive(aActive);
end;

{******************************************************************************************************************}
constructor TALXMLDocument.Create(const Rootname:AnsiString; const EncodingStr: AnsiString = cAlXMLUTF8EncodingStr);
begin
  create(true);
  version := '1.0';
  standalone := 'yes';
  Encoding := EncodingStr;
  AddChild(Rootname);
end;

{************************************}
{Disposes of a TALXMLDocument object.}
destructor TALXMLDocument.Destroy;
begin
  ReleaseDoc;
  inherited;
end;

{****************************************************************************}
//will create all the nodelist to be sure that multiple thread can safely read
//at the same time the Xmldocument
procedure TALXMLDocument.MultiThreadPrepare;

  procedure _doMultiThreadPrepare(aNode: TalXmlNode);
  var I: integer;
  begin
    If assigned(ANode.ChildNodes) then  // aNode.ChildNodes will create the nodelist
      For I := 0 to aNode.ChildNodes.Count - 1 do
        _doMultiThreadPrepare(aNode.ChildNodes[I]);
    If assigned(ANode.attributeNodes) then  // aNode.attributeNodes will create the nodelist
      For I := 0 to aNode.attributeNodes.Count - 1 do
        _doMultiThreadPrepare(aNode.attributeNodes[I]);
  end;

begin
  _doMultiThreadPrepare(DocumentElement);
end;

{***************************************************************************************************************}
procedure TALXMLDocument.Clear(const Rootname:AnsiString; const EncodingStr: AnsiString = cAlXMLUTF8EncodingStr);
begin
  releaseDoc;
  Active := true;
  version := '1.0';
  standalone := 'yes';
  Encoding := EncodingStr;
  AddChild(Rootname);
end;

{****************************************}
{Returns the value of the Active property.
 GetActive is the read implementation of the Active property.}
function TALXMLDocument.GetActive: Boolean;
begin
  Result := Assigned(FDocumentNode);
end;

{*************************************}
{Sets the value of the Active property.
 SetActive is the write implementation of the Active property.
 *Value is the new value to set.}
procedure TALXMLDocument.SetActive(const Value: Boolean);
begin
  if Value <> GetActive then begin
    if Value then FDocumentNode := TALXmlDocumentNode.Create(self)
    else ReleaseDoc;
  end;
end;

{****************************************************************************************************************************}
{StreamContainOnlyChildNodes mean that the stream contain ONLY the child node of ContainerNode, so it's not a valid xml stream
 like <root>...</root> but more like <rec>...</rec><rec>...</rec><rec>...</rec>}
Procedure TALXMLDocument.ParseXmlStream(RawXmlStream: TStream;
                                        ContainerNode: TALXmlNode;
                                        StreamContainOnlyChildNodes: Boolean=False);

Const BufferSize: integer = 8192;

Var buffer: AnsiString;
    bufferLength: Integer;
    bufferPos: Integer;
    PreserveWhiteSpace: Boolean;
    LstParams: TALStringList;
    NotSaxMode: Boolean;
    WorkingNode: TALXmlNode;
    DecodeXmlReferences: Boolean;
    UseContainerNodeInsteadOfAddingChildNode: Boolean;
    Paths: TALStringList;
    CodePage: Word;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function Expandbuffer: boolean;
  Var ByteReaded, Byte2Read: Integer;
  Begin
    If (bufferLength > 0) and (bufferPos > 1) then begin
      if (bufferPos > bufferLength) then RawXmlStream.Position := RawXmlStream.Position - bufferLength + bufferPos - 1;
      Byte2Read := min(bufferPos - 1, bufferLength);
      if bufferPos <= length(buffer) then ALMove(Pbyte(Buffer)[BufferPos - 1],
                                                 pointer(Buffer)^,
                                                 bufferLength-bufferPos+1); // no uniqueString will be call in this variant
      bufferPos := 1;
    end
    else begin
      Byte2Read := BufferSize;
      bufferLength := bufferLength + BufferSize;
      SetLength(buffer, bufferLength);
    end;

    //range check error is we not do so
    if RawXmlStream.Position < RawXmlStream.Size then ByteReaded := RawXmlStream.Read(Pbyte(Buffer)[BufferLength - Byte2Read{+ 1 - 1}],Byte2Read)
    else ByteReaded := 0;

    If ByteReaded <> Byte2Read then begin
      bufferLength := bufferLength - Byte2Read + ByteReaded;
      SetLength(buffer, bufferLength);
      Result := ByteReaded > 0;
    end
    else result := True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function PosInXmlString(const Substr: AnsiString; Offset: integer = 1): integer;
  Begin
    Result := ALPosEx(Substr,buffer,OffSet);
    While (Result <= 0) do begin
      Offset := bufferlength - bufferPos + 3 - length(Substr);
      If not Expandbuffer then break;
      Result := ALPosEx(Substr,buffer,offset);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function CharPosInString(const Substr: array of AnsiChar; const str: AnsiString; Offset: integer = 1): integer;
  Var I: integer;
      LowsubStr, highSubStr: integer;
      lnStr: integer;
      c: AnsiChar;
  Begin
    Result := 0;
    LowsubStr := low(SubStr);
    highSubStr := high(substr);
    lnStr := length(Str);
    while offset <= lnStr do begin
      c := str[offset];
      for I := LowsubStr to highSubStr do
        if c = Substr[I] then begin
          result := Offset;
          exit;
        end;
      inc(offset);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure CheckAttributes(TagParams: TALStrings);
  Var I: integer;
      S1, S2, S3: AnsiString;
      L1: integer;
      P1: integer;
  Begin
    I := 0;
    While I <= TagParams.Count - 3 do begin
      S1 := TagParams[I];
      S2 := TagParams[I+1];
      S3 := TagParams[I+2];
      L1 := length(S1);
      P1 := AlPos('=',S1);
      IF (P1 <= 0) and
         (S2 = '=') then begin {aname = "obie2.html"}
        TagParams[I] := S1 + S2 + S3;
        tagParams.Delete(I+2);
        tagParams.Delete(I+1);
      end
      else if (L1 > 0) and
              (P1 = L1) then begin {aname= "obie2.html"}
        TagParams[I] := S1 + S2;
        tagParams.Delete(I+1);
      end
      else if (L1 > 0) and
              (P1 <= 0) and
              (AlPos('=',S2) = 1)  then begin {aname ="obie2.html"}
        TagParams[I] := S1 + S2;
        tagParams.Delete(I+1);
      end;
      inc(I);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetPathStr(Const ExtraItems: ansiString = ''): ansiString;
  var I, L, Size, Count: Integer;
      P: PAnsiChar;
      LB: AnsiChar;
      S: AnsiString;
  begin
    Count := Paths.Count;
    LB := PathSeparator;
    Size := length(ExtraItems);
    if size <> 0 then Inc(Size, 1{length(LB)});
    for I := 0 to Count - 1 do Inc(Size, Length(Paths[I]) + 1{length(LB)});
    if size <> 0 then dec(Size, 1{length(LB)});
    SetLength(Result, Size);
    P := Pointer(Result);
    for I := 0 to Count - 1 do begin
      S := Paths[I];
      L := Length(S);
      if L <> 0 then begin
        ALMove(Pointer(S)^, P^, L);
        Inc(P, L);
      end;
      L := 1{length(LB)};
      if (L <> 0) and ((i <> Count - 1) or (ExtraItems <> '')) then begin
        ALMove(LB, P^, L);
        Inc(P, L);
      end;
    end;
    if ExtraItems <> '' then begin
      L := length(ExtraItems);
      ALMove(Pointer(ExtraItems)^, P^, L);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~}
  procedure AnalyzePI;
  Var P1, P2: Integer;
      LName, LContent: AnsiString;
  Begin
    { <?name?>...
      <?name content?>... }
    P2 := PosInXmlString('?>', bufferPos + 2);
    If P2 > 0 then begin

      P1 := CharPosInString([' ',#9,#13,#10],buffer, bufferPos+2);
      If (P1 <= 0) or (P1 > P2) then P1 := P2;
      LName := ALCopyStr(buffer,bufferPos + 2,P1-bufferPos - 2);
      LContent := ALCopyStr(buffer,P1+1,P2-P1-1);
      If NotSaxMode then begin
        if not assigned(WorkingNode) then ALXmlDocError(CALXmlParseError);
        If UseContainerNodeInsteadOfAddingChildNode then ALXmlDocError(CALXmlParseError); // because if not we need to delete and recreate the container node and this can cause trouble in the calling function
        WorkingNode.ChildNodes.Add(CreateNode(LName, ntProcessingInstr, LContent));
      end;

      //calculate the encoding
      //note: The XML Declaration at the beginning of an XML document (shown below) is not a processing instruction, however its similar
      //      syntax has often resulted in it being referred to as a processing instruction
      if ((not notSaxMode) or
          (ContainerNode.NodeType=ntDocument)) and
         (bufferPos in [1,4{UTF8 BOOM}]) then CodePage := ALGetCodePageFromCharSetName(ALExtractAttrValue(CALEncoding, LContent, ''));

      DoParseProcessingInstruction(GetPathStr(LName), LName, LContent);

      bufferPos := P2 + 2;

    end
    else ALXmlDocError(CALXmlParseError);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeDOCTYPE;
  Var P1: Integer;
  Begin
    { <!DOCTYPE ....> }
    P1 := PosInXmlString('>', bufferPos + 9);
    If P1 > 0 then begin
      //not yet implemented - simply skip the tag
      bufferPos := P1 + 1;
    end
    else ALXmlDocError(CALXmlParseError);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeComment;
  Var P1: Integer;
      LContent: AnsiString;
  Begin
    { <!-- name -->... }
    P1 := PosInXmlString('-->',bufferPos + 4);
    If P1 > 0 then begin
      LContent := ALCopyStr(buffer,bufferPos + 4,P1-bufferPos - 4);
      if NotSaxMode then begin
        if not assigned(WorkingNode) then ALXmlDocError(CALXmlParseError);
        If UseContainerNodeInsteadOfAddingChildNode then ALXmlDocError(CALXmlParseError); // because if not we need to delete and recreate the container node and this can cause trouble in the calling function
        WorkingNode.ChildNodes.Add(CreateNode(LContent, ntcomment, ''));
      end;
      DoParseComment(GetPathStr, LContent);
      bufferPos := P1 + 3;
    end
    else ALXmlDocError(CALXmlParseError);
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeCDATA;
  Var P1: Integer;
      LContent: AnsiString;
  Begin
    { <![CDATA[...]]> }
    P1 := PosInXmlString(']]>',bufferPos + 9);
    If P1 > 0 then begin
      LContent := ALCopyStr(buffer,bufferPos + 9,P1-bufferPos - 9);
      if NotSaxMode then begin
        if not assigned(WorkingNode) then ALXmlDocError(CALXmlParseError);
        If UseContainerNodeInsteadOfAddingChildNode then ALXmlDocError(CALXmlParseError); // because if not we need to delete and recreate the container node and this can cause trouble in the calling function
        WorkingNode.ChildNodes.Add(CreateNode(LContent, ntcdata, ''));
      end;
      DoParseCdata(GetPathStr, LContent);
      bufferPos := P1 + 3;
    end
    else ALXmlDocError(CALXmlParseError);
  end;

  {~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeTag;
  Var LTagEnclosed : Boolean;
      LName, LContent: AnsiString;
      LContentLn: Integer;
      LNode: TALXmlNode;
      P1, P2: Integer;
      I:Integer;
  Begin
    //<name>...
    //<name attrname="attrvalue">...
    //<name/>...
    //<name attrname="attrvalue"/>...
    //</name>...
    P1 := PosInXmlString('>',bufferPos + 1);
    If P1 > 0 then begin

      //end tag
      If buffer[bufferPos + 1] = '/' then begin
        LName := ALtrimRight(ALCopyStr(buffer,bufferPos + 2,P1-bufferPos - 2));
        if (Paths.Count = 0) or
           (Paths[paths.Count - 1] <> LName) then ALXmlDocError(CALXmlParseError);
        if NotSaxMode then begin
          LNode := TALXmlNode(Paths.Objects[paths.Count - 1]);
          if (LNode <> WorkingNode) then ALXmlDocError(CALXmlParseError);
          If WorkingNode<>ContainerNode then WorkingNode := WorkingNode.ParentNode
          Else WorkingNode := nil;
        end;
        DoParseEndElement(GetPathStr, LName);
        paths.Delete(paths.Count - 1);
        bufferPos := P1 + 1;
        exit;
      end;

      //tag is enclosed?
      If (buffer[P1-1] = '/') then Begin
        LTagEnclosed := True;
        dec(P1);
      end
      else LTagEnclosed := False;

      P2 := CharPosInString([' ',#9,#13,#10],buffer,bufferPos + 1);
      If (P2 <= 0) or (P2 > P1) then P2 := P1;

      LName := ALCopyStr(buffer,bufferPos + 1,P2-bufferPos - 1);
      if (LName = '') or (LName[1] = #0) then ALXmlDocError(CALXmlParseError); // mean it's an UTF-16 encoding not yet supported
      LContent := ALCopyStr(buffer,P2+1,P1-P2-1);

      if NotSaxMode then begin
        if not assigned(WorkingNode) then ALXmlDocError(CALXmlParseError);
        If UseContainerNodeInsteadOfAddingChildNode then begin
          LNode := ContainerNode;
          LNode.NodeName := LName;
          UseContainerNodeInsteadOfAddingChildNode := False;
          if LTagEnclosed then WorkingNode := nil;
        end
        else begin
          LNode := CreateNode(LName, ntelement, '');
          try
            WorkingNode.ChildNodes.Add(LNode);
          Except
            LNode.Free;
            raise;
          end;
        end
      end
      else LNode := nil; //for hide warning

      LstParams.Clear;
      If (LContent <> '') then begin
        ALExtractHeaderFields([' ', #9, #13, #10],    //Separators
                              [' ', #9, #13, #10],    //WhiteSpace
                              ['"', ''''],            //Quotes
                              PAnsiChar(LContent),    //Content
                              lstParams,              //Strings
                              False,                  //Decode
                              False);                 //StripQuotes
        CheckAttributes(LstParams);
        For I := 0 to LstParams.Count - 1 do begin
          LContent := LstParams.ValueFromIndex[I];
          LContentLn := length(LContent);
          if (LContentLn < 2) or
             (LContent[1] <> LContent[LContentLn]) or
             (not (LContent[1] in ['''','"'])) then ALXmlDocError(CALXmlParseError)
          else begin
            if DecodeXmlReferences then begin
              if CodePage = CP_UTF8 then LstParams[I] := LstParams.Names[I] + '=' + ALXMLTextElementDecode(alCopyStr(LContent,2,LContentLn-2))
              else LstParams[I] := LstParams.Names[I] + '=' + ALUTF8decode(ALXMLTextElementDecode(ALUTF8Encode(alCopyStr(LContent,2,LContentLn-2), CodePage)), Codepage);
            end
            else LstParams[I] := LstParams.Names[I] + '=' + alCopyStr(LContent,2,LContentLn-2)
          end;
        end;

        if NotSaxMode then
          For I := 0 to LstParams.Count - 1 do
            LNode.Attributes[LstParams.Names[I]] := LstParams.ValueFromIndex[I];
      end;

      DoParseStartElement(GetPathStr(LName), LName, LstParams);

      if not LTagEnclosed then begin
        if NotSaxMode then begin
          WorkingNode := LNode;
          Paths.AddObject(LName, WorkingNode);
        end
        else Paths.Add(LName);
        bufferPos := P1+1;
      end
      else begin
        DoParseEndElement(GetPathStr(LName), LName);
        bufferPos := P1+2;
      end;

    end
    else ALXmlDocError(CALXmlParseError);
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  Procedure AnalyzeText;
  Var P1: Integer;
      Str1: AnsiString;
  Begin
    P1 := PosInXmlString('<',bufferPos);
    If (P1<=0) then P1 := bufferLength + 1;

    Str1 := ALCopyStr(buffer,bufferPos,P1-bufferPos);
    If (PreserveWhiteSpace) or (ALTrim(Str1) <> '') then Begin

      if DecodeXmlReferences then begin
        if CodePage = CP_UTF8 then str1 := ALXMLTextElementDecode(Str1)
        else str1 := ALUTF8decode(ALXMLTextElementDecode(ALUTF8Encode(Str1, CodePage)), Codepage);
      end;

      if (notSaxMode) then begin
        if not assigned(WorkingNode) then ALXmlDocError(CALXmlParseError);
        if (WorkingNode.NodeType <> ntdocument) then begin  // ntdocument can not have any text node, so simply skip whiteSpace comment (else error)
          If UseContainerNodeInsteadOfAddingChildNode then ALXmlDocError(CALXmlParseError); // because if not we need to delete and recreate the container node and this can cause trouble in the calling function
          WorkingNode.ChildNodes.Add(CreateNode(Str1, ntText, ''));
        end
        else if (ALTrim(Str1) <> '') then ALXmlDocError(CALXmlParseError);
      end;

      DoParseText(GetPathStr, Str1);

    end;

    bufferPos := P1;
  end;

Begin

  //
  // NOTE: the childNodes and the AttributesNodes of the ContainerNode
  // must have been cleared by the calling function!
  //
  // NOTE: ContainerNode must have fDocument assigned
  //
  // NOTE: ContainerNode must be TDocument or TElement or nil (sax mode)
  //

  LstParams := TALStringList.Create;
  Paths := TALStringList.Create;
  Try

    DoParseStartDocument;

    PreserveWhiteSpace := (poPreserveWhiteSpace in ParseOptions) and (not (doNodeAutoIndent in Options));
    WorkingNode := ContainerNode;
    NotSaxMode := assigned(ContainerNode);
    UseContainerNodeInsteadOfAddingChildNode := NotSaxMode and (ContainerNode.NodeType <> ntdocument) and (not StreamContainOnlyChildNodes);
    DecodeXmlReferences := not (poIgnoreXmlReferences in ParseOptions);
    if NotSaxMode then CodePage := ALGetCodePageFromCharSetName(Encoding)
    else CodePage := 0;
    buffer := '';
    bufferLength := 0;
    bufferPos := 1;
    Expandbuffer;
    if AlUTF8DetectBOM(PansiChar(buffer),length(buffer)) then bufferPos := 4;

    While bufferPos <= bufferLength do begin

      If buffer[bufferPos] = '<' then begin
             If (bufferPos < bufferLength) and
                (buffer[bufferPos + 1] = '?') then AnalyzePI
        else if (bufferPos + 2 < bufferLength) and
                (buffer[bufferPos + 1] = '!') and
                (buffer[bufferPos + 2] = '-') and
                (buffer[bufferPos + 3] = '-') then AnalyzeComment
        else if (bufferPos + 7 < bufferLength) and
                (buffer[bufferPos + 1] = '!') and
                (buffer[bufferPos + 2] = '[') and
                (buffer[bufferPos + 3] = 'C') and
                (buffer[bufferPos + 4] = 'D') and
                (buffer[bufferPos + 5] = 'A') and
                (buffer[bufferPos + 6] = 'T') and
                (buffer[bufferPos + 7] = 'A') and
                (buffer[bufferPos + 8] = '[') then AnalyzeCData
        else if (bufferPos + 7 < bufferLength) and
                (buffer[bufferPos + 1] = '!') and
                (buffer[bufferPos + 2] = 'D') and
                (buffer[bufferPos + 3] = 'O') and
                (buffer[bufferPos + 4] = 'C') and
                (buffer[bufferPos + 5] = 'T') and
                (buffer[bufferPos + 6] = 'Y') and
                (buffer[bufferPos + 7] = 'P') and
                (buffer[bufferPos + 8] = 'E') then AnalyzeDOCTYPE
        else AnalyzeTag
      end
      else AnalyzeText;

      if bufferPos + 7 >= bufferLength then Expandbuffer;

    end;

    //some tags are not closed
    if Paths.Count > 0 then ALXmlDocError(CALXmlParseError);

    //mean the node was not update (empty stream?)
    if UseContainerNodeInsteadOfAddingChildNode then ALXmlDocError(CALXmlParseError);

    DoParseEndDocument;

  finally
    Paths.Free;
    LstParams.Free;
  end;

end;

{**********************************}
procedure TALXMLDocument.ReleaseDoc;
begin
  if assigned(FDocumentNode) then FreeAndNil(FDocumentNode);
end;

{**************************************}
{Loads an XML document and activates it.
 Call LoadFromFile to load the XML document specified by AFileName and set the Active property to true so
 that you can examine or modify the document.
 *AFileName is the name of the XML document to load from disk. If AFileName is an empty string, TALXMLDocument uses the value of the
  FileName property. If AFileName is not an empty string, TALXMLDocument changes the FileName property to AFileName.
 Once you have loaded an XML document, any changes you make to the document are not saved back to disk until you call the SaveToFile method.}
procedure TALXMLDocument.LoadFromFile(const AFileName: AnsiString; const saxMode: Boolean = False);
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
{Loads an XML document from a stream and activates it.
 Call LoadFromStream to load the XML document from a stream.
 *Stream is a stream object that can be used to read the string of XML that makes up the document.
 After loading the document from Stream, LoadFromStream sets the Active property to true.}
procedure TALXMLDocument.LoadFromStream(const Stream: TStream; const saxMode: Boolean = False);
begin
  if saxMode then SetActive(False)
  else begin
    releaseDoc;
    SetActive(True);
  end;
  ParseXmlStream(Stream, FDocumentNode);
end;

{*****************************************************************}
{Loads a string representation of an XML document and activates it.
 Call LoadFromXML to assign a string as the value of the XML document. Unlike the XML property, which lets you assign XML on a line-by-line
 basis, LoadFromXML treats the text of the XML document as a whole.
 The XML parameter is a string containing the text of an XML document. It should represent the XML text encoded using 8 bits char (utf-8, iso-8859-1, etc)
 After assigning the XML property as the contents of the document, LoadFromXML sets the Active property to true.}
procedure TALXMLDocument.LoadFromXML(const XML: AnsiString; const saxMode: Boolean = False);
var StringStream: TALStringStream;
begin
  StringStream := TALStringStream.Create(XML);
  try
    LoadFromStream(StringStream, saxMode);
  finally
    StringStream.Free;
  end;
end;

{******************************}
{Saves the XML document to disk.
 Call SaveToFile to save any modifications you have made to the parsed XML document.
 AFileName is the name of the file to save. If AFileName is an empty string, TXMLDocument uses the value of the FileName property.}
procedure TALXMLDocument.SaveToFile(const AFileName: AnsiString);
Var LfileStream: TfileStream;
begin
  LfileStream := TfileStream.Create(String(AFileName),fmCreate);
  Try
    SaveToStream(LfileStream);
  finally
    LfileStream.Free;
  end;
end;

{************************************************}
{Saves the XML document to a string-type variable.
 Call SaveToXML to save the contents of the XML document to the string-type variable specified by XML. SaveToXML writes the contents of XML document
 using 8 bits char (utf-8, iso-8859-1, etc) as an encoding system, depending on the type of the XML parameter.
 Unlike the XML property, which lets you write individual lines from the XML document, SaveToXML writes the entire text of the XML document.}
procedure TALXMLDocument.SaveToXML(var XML: AnsiString);
Var StringStream: TALStringStream;
begin
  StringStream := TALstringStream.Create('');
  Try
    SaveToStream(StringStream);
    XML := StringStream.DataString;
  finally
    StringStream.Free;
  end;
end;

{**********************************}
{Saves the XML document to a stream.
 Call SaveToStream to save the contents of the XML document to the stream specified by Stream.}
procedure TALXMLDocument.SaveToStream(const Stream: TStream);
begin
  CheckActive;
  node.SaveToStream(Stream);
end;

{*************************************}
{Returns the value of the XML property.
 GetXML is the read implementation of the XML property.}
function TALXMLDocument.GetXML: AnsiString;
begin
  SaveToXML(Result);
end;

{**********************************}
{Sets the value of the XML property.
 SetXML is the write implementation of the XML property.
 *Value contains the raw (unparsed) XML to assign.}
procedure TALXMLDocument.SetXML(const Value: AnsiString);
begin
  LoadFromXML(Value, False);
end;

{***********************************}
procedure TALXMLDocument.CheckActive;
begin
  if not Assigned(FDocumentNode) then ALXMLDocError(CALXmlNotActive);
end;

{***********************************************************}
{Creates and adds a node to the child nodes of this document.
 Call AddChild to add a new child element node to the document itself. The new node is added to the end of the list maintained
 by the ChildNodes property.
 *TagName is the tag name for the new element node.
 AddChild returns the interface for the newly created node.
 Note:	Do not call AddChild to add a child to the document element of this document. When adding data to the XML document, use the
 AddChild method of the document element or of the node in the hierarchy that should be the parent of the new node.}
function TALXMLDocument.AddChild(const TagName: AnsiString): TALXMLNode;
begin
  Result := Node.AddChild(TagName);
end;

{****************************************************************}
{Creates a new element node that is associated with this document.
 Call CreateElement to create a new generic element node. The resulting node does not have a parent, but can be added to
 the ChildNodes list of any node in the document (including this document’s ChildNodes property). It can also be assigned to
 the DocumentElement property to replace the entire data hierarchy of this document.
 *TagOrData provides the tag name of the newly created node.
 *CreateElement returns the interface for the new node.
 Note:	To add a node as the child of another node in this document, it is simpler to call that node’s AddChild method.}
function TALXMLDocument.CreateElement(const TagOrData: AnsiString): TALXMLNode;
begin
  Result := CreateNode(TagOrData, ntElement, '');
end;

{********************************************************}
{Creates a new node that is associated with this document.
 Call CreateNode to create a new generic XML node. The resulting node does not have a parent, but can be added to the ChildNodes or AttributeNodes
 list of any node in the document (including this document’s ChildNodes property). It can also be assigned to the DocumentElement property to
 replace the entire data hierarchy of this document.
 *NameOrData provides the tag name or value of the newly created node. Its interpretation depends on the type of node created, as indicated in the
 following table:

 NodeType	          NameOrData
 ntElement	        The tag name.
 ntAttribute	      The attribute name.
 ntText	            The value of the node.
 ntCData	          The value of the CDATA section.
 ntEntityRef	      The name of the referenced entity.
 ntProcessingInstr	The target of the processing instruction.
 ntComment	        The value (text) of the comment.
 ntDocFragment	    Not used.

 *NodeType indicates the type of node to create. It can only be one of the types listed in the previous table.
 *The meaning of AddlData depends on the node type, as indicated in the following table:

 NodeType	AddlData
 ntProcessingInstr	The content of the processing instruction, except for the target.

 CreateNode returns the interface for the new node.
 Note:	To add a node as the child of another node in this document, it is simpler to call that node’s AddChild method.}
function TALXMLDocument.CreateNode(const NameOrData: AnsiString; NodeType: TALXMLNodeType = ntElement; const AddlData: AnsiString = ''): TALXMLNode;
begin
  Result := ALCreateXmlNode(NameOrData, NodeType, AddlData);
end;

{********************************************}
{Returns the value of the ChildNodes property.
 GetChildNodes is the read implementation of the ChildNodes property.}
function TALXMLDocument.GetChildNodes: TALXMLNodeList;
begin
  Result := Node.ChildNodes;
end;

{************************************************************************}
{Indicates whether the TXMLDocument instance represents an empty document.
 Call IsEmptyDoc to determine whether the TALXMLDocument instance represents an empty document.
 IsEmptyDoc returns true if the Document property is not set or if this object represents a
 document with no child nodes.}
function TALXMLDocument.IsEmptyDoc: Boolean;
begin
  Result := not (Assigned(FDocumentNode) and FDocumentNode.hasChildNodes);
end;

{**************************************}
{Returns the value of the Node property.
 GetDocumentNode is the read implementation of the Node property.}
function TALXMLDocument.GetDocumentNode: TALXMLNode;
begin
  CheckActive;
  Result := FDocumentNode;
end;

{*************************************************}
{Returns the value of the DocumentElement property.
 GetDocumentElement is the read implementation of the DocumentElement property.}
function TALXMLDocument.GetDocumentElement: TALXMLNode;
begin
  CheckActive;
  Result := nil;
  if Node.HasChildNodes then begin
    Result := Node.ChildNodes.Last;
    while Assigned(Result) and (Result.NodeType <> ntElement) do Result := Result.PreviousSibling;
  end;
end;

{**********************************************}
{Sets the value of the DocumentElement property.
 SetDocumentElement is the write implementation of the DocumentElement property.
 Value is the interface for the node tree that replaces the current document element.}
procedure TALXMLDocument.SetDocumentElement(const Value: TALXMLNode);
var OldDocElement: TALXMLNode;
begin
  CheckActive;
  OldDocElement := GetDocumentElement;
  if Assigned(OldDocElement) then Node.ChildNodes.ReplaceNode(OldDocElement, Value).free
  else Node.ChildNodes.Add(Value);
end;

{***********************************************}
{Returns the value of the NodeIndentStr property.
 GetNodeIndentStr is the read implementation of the NodeIndentStr property.}
function TALXMLDocument.GetNodeIndentStr: AnsiString;
begin
  Result := FNodeIndentStr;
end;

{********************************************}
{Sets the value of the NodeIndentStr property.
 SetNodeIndentStr is the write implementation of the NodeIndentStr property.
 *Value is the string that is inserted before nested nodes to indicate a level of nesting.}
procedure TALXMLDocument.SetNodeIndentStr(const Value: AnsiString);
begin
  FNodeIndentStr := Value;
end;

{*****************************************}
{Returns the value of the Options property.
 GetOptions is the read implementation of the Options property.}
function TALXMLDocument.GetOptions: TALXMLDocOptions;
begin
  Result := FOptions;
end;

{**************************************}
{Sets the value of the Options property.
 GetOptions is the write implementation of the Options property.
 *Value is the set of options to assign.}
procedure TALXMLDocument.SetOptions(const Value: TALXMLDocOptions);
begin
  FOptions := Value;
end;

{**********************************************}
{Returns the value of the ParseOptions property.
 GetParseOptions is the read implementation of the ParseOptions property.}
function TALXMLDocument.GetParseOptions: TALXMLParseOptions;
begin
  Result := FParseOptions;
end;

{*******************************************}
{Sets the value of the ParseOptions property.
 GetParseOptions is the write implementation of the ParseOptions property.
 *Value is the set of parser options to assign.}
procedure TALXMLDocument.SetParseOptions(const Value: TALXMLParseOptions);
begin
  FParseOptions := Value;
end;

{***************************************************************}
procedure TALXMLDocument.SetPathSeparator(const Value: AnsiChar);
begin
  FPathSeparator := Value;
end;

{*************************************************}
function TALXMLDocument.GetPathSeparator: ansiChar;
begin
  result := fPathSeparator;
end;

{************************************************}
function TALXMLDocument.GetPrologNode: TALXMLNode;
begin
  CheckActive;
  if (Node.ChildNodes.Count > 0) and
     (Node.ChildNodes[0].NodeType = ntProcessingInstr) and
     (Node.ChildNodes[0].NodeName = CALXML)
  then Result := Node.ChildNodes[0]
  else Result := nil;
end;

{***************************************************************************************************************}
function TALXMLDocument.GetPrologValue(PrologItem: TALXMLPrologItem; const Default: AnsiString = ''): AnsiString;
var PrologNode: TALXMLNode;
    PrologAttrs: AnsiString;
begin
  PrologNode := GetPrologNode;
  if Assigned(PrologNode) then begin
    PrologAttrs := PrologNode.NodeValue;
    case PrologItem of
      xpVersion:    Result := ALExtractAttrValue(CALVersion, PrologAttrs, Default);
      xpEncoding:   Result := ALExtractAttrValue(CALEncoding, PrologAttrs, Default);
      xpStandalone: Result := ALExtractAttrValue(CALStandalone, PrologAttrs, Default);
    end
  end
  else Result := Default;
end;

{**********************************************************************************************************************************************}
function TALXMLDocument.InternalSetPrologValue(const PrologNode: TALXMLNode; const Value: AnsiString; PrologItem: TALXMLPrologItem): AnsiString;
var Version, Encoding, Standalone: AnsiString;
begin
  if Assigned(PrologNode) then begin
    { Initialize values from existing prolog entry }
    Version := GetPrologValue(xpVersion, '1.0');
    Encoding := GetPrologValue(xpEncoding);
    Standalone := GetPrologValue(xpStandalone);
  end
  else Version := '1.0';

  { Set the new value }
  case PrologItem of
    xpVersion: Version := Value;
    xpEncoding: Encoding := Value;
    xpStandalone: Standalone := Value;
  end;

  { Build a string with all of the values }
  Result := '';
  ALAppendItem(Result, CALVersion, Version);
  ALAppendItem(Result, CALEncoding, Encoding);
  ALAppendItem(Result, CALStandalone, Standalone);
end;

{*********************************************************************************************}
procedure TALXMLDocument.SetPrologValue(const Value: AnsiString; PrologItem: TALXMLPrologItem);
var PrologAttrs: AnsiString;
    NewPrologNode, PrologNode: TALXMLNode;
begin
  PrologNode := GetPrologNode;
  PrologAttrs := InternalSetPrologValue(PrologNode, Value, PrologItem);
  NewPrologNode := CreateNode('xml', ntProcessingInstr, PrologAttrs);
  if Assigned(PrologNode) then Node.ChildNodes.ReplaceNode(PrologNode, NewPrologNode).free
  else ChildNodes.Insert(0, NewPrologNode);
end;

{******************************************}
{Returns the value of the Encoding property.
GetEncoding is the read implementation of the Encoding property.}
function TALXMLDocument.GetEncoding: AnsiString;
begin
  Result := GetPrologValue(xpEncoding);
end;

{***************************************}
{Sets the value of the Encoding property.
 SetEncoding is the write implementation of the Encoding property.
 *Value is the value of the encoding attribute of the document type node.}
procedure TALXMLDocument.SetEncoding(const Value: AnsiString);
begin
  SetPrologValue(Value, xpEncoding);
end;

{*****************************************}
{Returns the value of the Version property.
GetVersion is the read implementation of the Version property.}
function TALXMLDocument.GetVersion: AnsiString;
begin
  Result := GetPrologValue(xpVersion);
end;

{**************************************}
{Sets the value of the Version property.
 SetVersion is the write implementation of the Version property.
 *Value is the value of the version attribute of the document type node.}
procedure TALXMLDocument.SetVersion(const Value: AnsiString);
begin
  SetPrologValue(Value, xpVersion);
end;

{********************************************}
{Returns the value of the StandAlone property.
 GetStandAlone is the read implementation of the StandAlone property.}
function TALXMLDocument.GetStandAlone: AnsiString;
begin
  Result := GetPrologValue(xpStandalone);
end;

{*****************************************}
{Sets the value of the StandAlone property.
 SetStandAlone is the write implementation of the StandAlone property.
 *Value is the value of the stand-alone attribute of the document type node.}
procedure TALXMLDocument.SetStandAlone(const Value: AnsiString);
begin
  SetPrologValue(Value, xpStandalone);
end;

{*******************************************************************}
procedure TALXMLDocument.DoParseComment(const Path, Str: AnsiString);
begin
  if Assigned(FOnParseComment) then FOnParseComment(Self, Path, Str);
end;

{*****************************************************************}
procedure TALXMLDocument.DoParseCData(const Path, Str: AnsiString);
begin
  if Assigned(FOnParseCData) then FOnParseCData(Self, Path, Str);
end;

{******************************************}
procedure TALXMLDocument.DoParseEndDocument;
begin
  if Assigned(FOnParseEndDocument) then FOnParseEndDocument(Self);
end;

{***********************************************************************}
procedure TALXMLDocument.DoParseEndElement(const Path, Name: AnsiString);
begin
  if Assigned(FOnParseEndElement) then FOnParseEndElement(Self, Path, Name);
end;

{******************************************************************************************}
procedure TALXMLDocument.DoParseProcessingInstruction(const Path, Target, Data: AnsiString);
begin
  if Assigned(FOnParseProcessingInstruction) then FOnParseProcessingInstruction(Self, Path, Target, Data);
end;

{********************************************}
procedure TALXMLDocument.DoParseStartDocument;
begin
  if Assigned(FOnParseStartDocument) then FOnParseStartDocument(Self);
end;

{*************************************************************************************************}
procedure TALXMLDocument.DoParseStartElement(const Path, Name: AnsiString; Attributes: TALStrings);
begin
  if Assigned(FOnParseStartElement) then FOnParseStartElement(Self, Path, Name, Attributes);
end;

{****************************************************************}
procedure TALXMLDocument.DoParseText(const Path, Str: AnsiString);
begin
  if Assigned(FOnParseText) then FOnParseText(Self, Path, Str);
end;

{********************************}
{Instantiates a TALXMLNode object.
 Create instantiates a new TALXMLNode object.
 *AParentNode is the implementation of the parent node of this node.
 *OwnerDoc represents the XML document in which the new node occurs.
 Typically, applications do not directly call the TALXMLNode constructor. Instead, new nodes are
 created automatically for an XML document as necessary. To add new nodes to a document,
 applications call the parent node’s AddChild method.}
constructor TALXMLNode.Create(const NameOrData: AnsiString;
                              const AddlData: AnsiString = '');
Begin
  FDocument := nil;
end;

{*************************************************************}
{CloneNode returns a copy of this node that has no parent. Any
 attributes of this node are copied as well, including attribute
 nodes that represent default values.
 Deep indicates whether the child nodes of this node should be
 cloned as well. For example, if an element node contains any text,
 CloneNode only copies then text when deep is true, because in
 the underlying DOM implementation, the text is contained in
 a child node.
 After calling CloneNode to create a new node, you can add that
 node into the ChildNodes property of another node (or, if this
 node represents an attribute, into the AttributeNodes property)
 by calling ChildNodes. ->Add or ChildNodes. ->Insert.}
function TALXMLNode.CloneNode(Deep: Boolean): TALXMLNode;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function InternalCloneNode(const SourceNode: TALXmlNode;
                             const TargetParentNode: TALXmlNode): TalXmlNode;
  var I: Integer;
  begin
    case SourceNode.nodeType of
      ntElement: begin
                   result := ALCreateXmlNode(SourceNode.NodeName, SourceNode.nodeType, '');
                   if TargetParentNode <> nil then TargetParentNode.ChildNodes.Add(result);
                   for I := 0 to SourceNode.AttributeNodes.Count - 1 do
                     InternalCloneNode(SourceNode.AttributeNodes[I],result);
                   if Deep then
                     for I := 0 to SourceNode.ChildNodes.Count - 1 do
                      InternalCloneNode(SourceNode.ChildNodes[I],result);
                 end;
      ntAttribute: begin
                     result := ALCreateXmlNode(SourceNode.NodeName, SourceNode.nodeType, '');
                     Result.NodeValue := SourceNode.NodeValue;
                     if TargetParentNode <> nil then TargetParentNode.AttributeNodes.Add(result);
                   end;
      ntText,
      ntCData,
      ntComment: begin
                   result := ALCreateXmlNode(SourceNode.NodeName, SourceNode.nodeType, '');
                   Result.NodeValue := SourceNode.NodeValue;
                   if TargetParentNode <> nil then TargetParentNode.ChildNodes.Add(result);
                 end;
      ntEntityRef: result := nil; // todo
      ntProcessingInstr: begin
                           result := ALCreateXmlNode(SourceNode.NodeName, SourceNode.nodeType, SourceNode.nodeValue);
                           if TargetParentNode <> nil then TargetParentNode.ChildNodes.Add(result);
                         end;
      ntDocFragment: result := nil; // todo
      else begin
        result := nil; // to hide warning
        ALXMLDocError(cALXmlInvalidNodeType);
      end;
    end;
  end;

begin
  result := InternalCloneNode(self, nil);
end;

{**************************************************************}
{Creates the object that implements the AttributeNodes property.
 Applications can’t call the protected CreateAttributeList method. This method is used internally the
 first time an application reads the AttributeNodes property.}
function TALXMLNode.CreateAttributeList: TALXMLNodeList;
begin
  result := TALXMLNodeList.Create(Self);
end;

{************************************************************************************}
procedure TALXMLNode.DoBeforeChildNodesInsert(Index: Integer; const Node: TALXMLNode);
begin
  //virtual; check here if the node can be insert in the childnodes list.
end;

{************************************************}
{Returns the value of the AttributeNodes property.
 GetAttributeNodes is the read implementation of the AttributeNodes property on the TALXMLNode for this node.
 GetAttributeNodes generates the list that implements the AttributeNodes property the first time an application
 reads the AttributeNodes property.}
function TALXMLNode.GetAttributeNodes: TALXMLNodeList;
begin
  Result := nil; // hide warning
  //ALXMLDocError(CALXMLOperationError,[GetNodeTypeStr]); => with this exception it's not possible to do something like
  //                                                         If assigned(ANode.AttributeNodes) then ...
  //                                                         and the only option we have is to check the nodetype to know if the GetAttributeNodes is implemented
  //                                                         and anyway TXMLNodeList don't raise any exception here so i prefer to stay compatible
end;

{*********************************************}
{Sets the value of the AttributeNodes property.
 Applications can’t call this protected method. It is called internally the first time an application reads the
 AttributeNodes property to supply the interface that implements that property.}
procedure TALXMLNode.SetAttributeNodes(const Value: TALXMLNodeList);
begin
  ALXMLDocError(CALXMLOperationError,[GetNodeTypeStr]);
end;

{*****************************************************************}
{Indicates whether the node has an attribute with a specified name.
 *Name is the name of the attribute about which you want to know.
 HasAttribute returns true if this node has the specified attribute, false otherwise.}
function TALXMLNode.HasAttribute(const Name: AnsiString): Boolean;
Var LNodeList: TALXMLNodeList;
begin
  LNodeList := InternalGetAttributeNodes;
  if assigned(LNodeList) then Result := LNodeList.IndexOf(Name) <> -1
  else Result := False;
end;

{**************************************************}
{Returns the value of one of this node’s attributes.
 GetAttribute returns the value of an attribute of this node.
 *AttrName is the name of the attribute.
 GetAttribute returns the value of the specified attribute. Typically, this value is a string. If the node does
 not have an attribute with the name specified by AttrName, the value that GetAttribute returns is determined by
 the document’s Options property. When Options include doAttrNull, GetAttribute returns a Null Variant for
 missing attributes. When Options does not include doAttrNull, GetAttribute returns an empty string as the value
 of missing attributes. If this node is not an element node, GetAttribute raises an EXMLDocError exception.}
function TALXMLNode.GetAttribute(const AttrName: AnsiString): AnsiString;
Var LNode: TALXmlNode;
    LNodeList: TALXMLNodeList;
begin
  LNodeList := InternalGetAttributeNodes;
  if assigned(LNodeList) then LNode := LNodeList.findNode(AttrName)
  else LNode := Nil;

  if assigned(LNode) then Result := LNode.NodeValue
  else Result := '';
end;

{***********************************************}
{Sets the value of one of this node’s attributes.
 SetAttribute sets the value of a specified attribute of this node.
 *AttrName is the name of the attribute. If the attribute already exists for the node, its value is changed.
  If the attribute does not already exist, a new attribute is created.
 *Value is the value to assign to the attribute. This value is a string that is not parsed. Any markup
  (for example, for an entity reference) is treated as literal text, and must include appropriate escape sequences.
  If Value is NULL, the attribute is removed from this node. If Value is an empty string, the attribute is
  assigned an empty value.}
procedure TALXMLNode.SetAttribute(const AttrName: AnsiString; const Value: AnsiString);
var LNode: TALXmlNode;
    LNodeList: TALXMLNodeList;
begin
  LNodeList := AttributeNodes;
  LNode := LNodeList.FindNode(attrName);
  if not assigned(LNode) then begin
    LNode := ALCreateXmlNode(AttrName, ntattribute, '');
    LNode.Text := Value;
    LNodeList.Add(LNode);
  end
  else LNode.Text := Value;
end;

{**********************************************************}
{Creates the object that implements the ChildNodes property.
 Applications can’t call the protected CreateChildList method. This method is used internally the first time an
 application reads the ChildNodes property.}
function TALXMLNode.CreateChildList: TALXMLNodeList;
begin
  result := TALXMLNodeList.Create(Self);
end;

{***********************************************}
{Indicates whether this node has any child nodes.
 GetHasChildNodes returns true if the underlying DOM node has any child nodes, false if it doesn’t.
 Note that even if the underlying DOM node has a child node, there may not be a TXMLNode object to
 represent that child. For example, text nodes do not, by default, have corresponding TXMLNode wrappers.
 Rather, the text becomes the value of the parent element node.}
function TALXMLNode.GetHasChildNodes: Boolean;
Var LNodeList: TALXMLNodeList;
begin
  LNodeList := InternalGetChildNodes;
  Result := assigned(LNodeList) and (LNodeList.Count > 0);
end;

{********************************************}
{Returns the value of the ChildNodes property.
 GetChildNodes is the protected read implementation of the ChildNodes property.
 GetChildNodes generates the list that implements the ChildNodes property the first time an application
 reads the ChildNodes property.}
function TALXMLNode.GetChildNodes: TALXMLNodeList;
begin
  Result := nil; // hide warning
  //ALXMLDocError(CALXMLOperationError,[GetNodeTypeStr]); => with this exception it's not possible to do something like
  //                                                         If assigned(ANode.ChildNodes) then ...
  //                                                         and the only option we have is to check the nodetype to know if the GetChildNodes is implemented
  //                                                         other option is to use HasChildNodes but anyway TXMLNodeList don't raise any exception here
  //                                                         so i prefer to stay compatible
end;

{*****************************************}
{Sets the value of the ChildNodes property.
 Applications can’t call this protected method. It is called internally the first time an application reads the ChildNodes
 property to supply the interface that implements that property.}
procedure TALXMLNode.SetChildNodes(const Value: TALXMLNodeList);
begin
  ALXMLDocError(CALXMLOperationError,[GetNodeTypeStr]);
end;

{****************************************************************}
{AddChild creates a new element node as the child of this node and
 returns its interface.
 *TagName specifies the tag name of the newly created node.
 *Index indicates the position of the child node in this node’s list of children, where 0 is the first position, 1 is the second
 position, and so on. If Index is –1, the new node is added to the end.}
function TALXMLNode.AddChild(const TagName: AnsiString; Index: Integer = -1): TALXMLNode;
begin
  Result := ALCreateXmlNode(TagName,ntElement,'');
  Try
    ChildNodes.Insert(Index, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{******************************************}
{Returns the next child of this node’s parent.
 NextSibling returns the node that follows this one in the parent node’s ChildNodes property list.
 If this node is the last node in its parent’s child list, NextSibling raises an exception.}
function TALXMLNode.NextSibling: TALXMLNode;
begin
  if Assigned(ParentNode) then Result := ParentNode.ChildNodes.FindSibling(Self, 1)
  else Result := nil;
end;

{************************************************}
{Returns the previous child of this node’s parent.
 PreviousSibling returns the node that precedes this one in the parent node’s ChildNodes property list.
 If this node is the first node in its parent’s child list, PreviousSibling raises an exception.}
function TALXMLNode.PreviousSibling: TALXMLNode;
begin
  if Assigned(ParentNode) then Result := ParentNode.ChildNodes.FindSibling(Self, -1)
  else Result := nil;
end;

{*****************************************************************}
{Returns the number of parents for this node in the node hierarchy.
 NestingLevel returns the number of ancestors for this node in the node hierarchy.}
function TALXMLNode.NestingLevel: Integer;
var PNode: TALXMLNode;
begin
  Result := 0;
  PNode := ParentNode;
  while PNode <> nil do begin
    Inc(Result);
    PNode := PNode.ParentNode;
  end;
end;

{**********************************************************************************}
{If the XML node is an element that has child nodes (other than a single text node),
 CheckTextNode raises an exception.}
procedure TALXMLNode.CheckTextNode;
begin
  If not IsTextElement then AlXMLDocError(CALXmlNotSingleTextNode, [nodeName]);
end;

{**********************************}
{Returns the text value of the node.
 GetText returns the text of the node. For example, if the node represents an XML fragment such as
 <Title> Understanding XML </Title>
 GetText returns the string ‘Understanding XML’.
 GetText is intended for use when the GetIsTextElement method returns true. If GetIsTextElement returns false,
 GetText returns an empty string if the node has no children, otherwise it raises an exception.}
function TALXMLNode.GetText: AnsiString;
begin
  case NodeType of
    NtElement:         Begin
                         // NodeList could be created on first access to ChildNodes like Node.ChildNodes.Count for example,
                         // so we need to check also the situation when it's Assigned but empty (has 0 items)
                         CheckTextNode;
                         if (InternalGetChildNodes <> nil) and
                            (ChildNodes.Count > 0) then result := ChildNodes[0].Text
                         else result := '';
                       end;
    NtAttribute:       Begin
                         If (InternalGetChildNodes <> nil) then result := ChildNodes[0].Text
                         else result := '';
                       end;
    ntText:            result := internalValue;
    ntCData:	         result := internalValue;
    ntEntityRef:       result := '';//Todo
    ntEntity:	         result := '';//todo
    ntProcessingInstr: result := internalChildValue;
    ntComment:	       result := internalValue;
    ntDocument:	       result := '';
    ntDocType:         result := '';//todo
    ntDocFragment:	   result := '';//todo
    ntNotation:	       result := '';//todo
    else begin
      Result := ''; //for hide warning
      AlXMLDocError(cAlXmlInvalidNodeType);
    end;
  end;
end;

{*******************************}
{Sets the text value of the node.
 SetText replaces the text of the node.
 SetText is intended for use when the GetIsTextElement method returns true. If GetIsTextElement
 returns false, SetText adds a new text value to a node that has no children, otherwise, it
 raises an exception.}
procedure TALXMLNode.SetText(const Value: AnsiString);
begin
  case NodeType of
    NtElement:         Begin
                         CheckTextNode;
                         if ChildNodes.Count = 1 then ChildNodes[0].Text := Value
                         else ChildNodes.Add(ALCreateXmlNode(Value, NtText, ''))
                       end;
    NtAttribute:       Begin
                         if ChildNodes.Count = 1 then ChildNodes[0].Text := Value
                         else ChildNodes.Add(ALCreateXmlNode(Value, NtText, ''))
                       end;
    ntText:            internalValue := Value;
    ntCData:	         internalValue := Value;
    //ntEntityRef:       ;todo
    //ntEntity:	         ;todo
    ntProcessingInstr: internalChildValue := Value;
    ntComment:	       internalValue := value;
    ntDocument:	       ALXmlDocError(CALXmlOperationError,[CALXmlDocument]);
    //ntDocType:         ;todo
    //ntDocFragment:	   ;todo
    //ntNotation:	       ;todo
    else AlXMLDocError(cAlXmlInvalidNodeType);
  end;
end;

{******************************}
{Returns the value of this node.
 GetNodeValue returns the value of the node.
 This value depends on the type of the node, as indicated in the following table:

 NodeType	          Value
 ntAttribute	      The attribute value
 ntElement	        If the element contains only text, this is that text value. Otherwise, GetNodeValue raises an exception.
 ntText	            The text
 ntCData	          The content of the CDATA section.
 ntEntityRef	      nil (Delphi) or NULL (C++)
 ntProcessingInstr	The content of the processing instruction except for the target.
 ntComment	        The value (text) of the comment.
 ntDocFragment	    nil (Delphi) or NULL (C++)}
function TALXMLNode.GetNodeValue: AnsiString;
begin
  Result := GetText;
end;

{***************************}
{Sets the value of this node.
 SetNodeValue sets the value of this node.
 Value is the value to assign. It’s meaning depends on the type of the node, as indicated in the following table:

 NodeType           Value
 ntAttribute  	    The attribute value
 ntElement	        If the element contains only text, this is that text value. Otherwise, SetNodeValue raises an exception.
 ntText	            The text
 ntCData	          The content of the CDATA section.
 ntProcessingInstr	The content of the processing instruction except for the target.
 ntComment	        The value (text) of the comment.

 If the node is any other node type, SetNodeValue raises an exception.}
procedure TALXMLNode.SetNodeValue(const Value: AnsiString);
begin
  SetText(Value);
end;

{******************************************************************************}
{Returns the XML that corresponds to this node and any child nodes it contains.}
procedure TALXMLNode.SaveToStream(const Stream: TStream; Const SaveOnlyChildNodes: Boolean=False);

Const BufferSize: integer = 32768;
Var NodeStack: Tstack;
    CurrentNode: TalxmlNode;
    CurrentParentNode: TalxmlNode;
    EncodeXmlReferences: Boolean;
    BufferString: AnsiString;
    BufferStringPos: Integer;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure WriteBuffer2Stream(const buffer: ansiString; BufferLength: Integer);
  Begin
    If BufferLength > 0 then stream.WriteBuffer(pointer(buffer)^,BufferLength);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure WriteStr2Buffer(const str:AnsiString);
  var L: integer;
  Begin
    L := Length(Str);
    if l = 0 then exit;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure WriteAttributeNode2Stream(aAttributeNode:TALXmlNode);
  Begin
    with aAttributeNode do
      if EncodeXmlReferences then WriteStr2Buffer(' ' + NodeName + '="' + ALXMLTextElementEncode(text) + '"')
      else begin
        if alpos('"',text) > 0 then WriteStr2Buffer(' ' + NodeName + '=''' + text + '''')
        else WriteStr2Buffer(' ' + NodeName + '="' + text + '"');
      end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure WriteTextNode2Stream(aTextNode:TALXmlNode);
  Begin
    with aTextNode do
      if EncodeXmlReferences then WriteStr2Buffer(ALXMLTextElementEncode(text))
      else WriteStr2Buffer(text);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure WriteCDATANode2Stream(aTextNode:TALXmlNode);
  Begin
    with aTextNode do
      WriteStr2Buffer('<![CDATA[' + text + ']]>');
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure WriteCommentNode2Stream(aCommentNode:TALXmlNode);
  Begin
    with acommentNode do
      WriteStr2Buffer('<!--' + text + '-->');
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure WriteProcessingInstrNode2Stream(aProcessingInstrNode:TALXmlNode);
  Var LText: AnsiString;
  Begin
    with aProcessingInstrNode do begin
      WriteStr2Buffer('<?'+NodeName);

      LText := Text;
      If LText <> '' then
        WriteStr2Buffer(' ' + LText);

      WriteStr2Buffer('?>');
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure WriteStartElementNode2Stream(aElementNode:TALXmlNode);
  var I: integer;
      LNodeList: TALXmlNodeList;
      LTagEnclosed: Boolean;
  Begin
    with aElementNode do begin
      WriteStr2Buffer('<'+NodeName);

      LNodeList := InternalGetAttributeNodes;
      If assigned(LNodeList) then
        with LNodeList do
          For I := 0 to Count - 1 do
            WriteAttributeNode2Stream(Nodes[I]);

      LTagEnclosed := True;
      LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LTagEnclosed := False;
            WriteStr2Buffer('>');
            NodeStack.Push(aElementNode);
            For I := Count - 1 downto 0 do NodeStack.Push(Nodes[I]);
          end
      end;

      If LTagEnclosed then WriteStr2Buffer('/>');
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure WriteEndElementNode2Stream(aElementNode:TALXmlNode);
  Begin
    with aElementNode do
      WriteStr2Buffer('</'+NodeName+'>');
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure WriteDocumentNode2Stream(aDocumentNode:TALXmlNode);
  var I: integer;
      LNodeList: TALXmlNodeList;
  Begin
    with aDocumentNode do begin

      LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then
        with LNodeList do
          For I := Count - 1 downto 0 do NodeStack.Push(Nodes[I]);

    end;
  end;

begin
  If not (NodeType in [ntElement, ntDocument]) then exit;

  CurrentParentNode := nil;
  NodeStack := Tstack.Create;
  Try

    {init buffer string}
    Setlength(BufferString, BufferSize * 2);
    BufferStringPos := 0;

    {EncodeXmlReferences from ParseOptions}
    EncodeXmlReferences := not (poIgnoreXmlReferences in FDocument.ParseOptions);

    {SaveOnlyChildNodes}
    If SaveOnlyChildNodes or (NodeType = ntDocument) then WriteDocumentNode2Stream(self)
    else NodeStack.Push(self);

    {loop on all nodes}
    While NodeStack.Count > 0 Do begin
      CurrentNode := TAlXmlNode(NodeStack.Pop);

      with CurrentNode do
        case NodeType of
          NtElement:         Begin
                               if currentNode = CurrentParentNode then WriteEndElementNode2Stream(CurrentNode)
                               else WriteStartElementNode2Stream(CurrentNode);
                             end;
          ntText:            WriteTextNode2Stream(CurrentNode);
          ntCData:	         WriteCDataNode2Stream(CurrentNode);
          //ntEntityRef:       ;Todo
          //ntEntity:	         ;todo
          ntProcessingInstr: WriteProcessingInstrNode2Stream(CurrentNode);
          ntComment:	       WriteCommentNode2Stream(CurrentNode);
          //ntDocType:         ;todo
          //ntDocFragment:	   ;todo
          //ntNotation:	       ;todo
          else AlXMLDocError(cAlXMLInvalidNodeType);
        end;

      CurrentParentNode := CurrentNode.ParentNode;
      If assigned(CurrentParentNode) and
         (CurrentParentNode.nodeType = ntDocument) and
         (CurrentNode.nodeType <> ntelement) then WriteStr2Buffer(#13#10);
    end;

    {Write the buffer}
    WriteBuffer2Stream(BufferString, BufferStringPos);

  finally
    NodeStack.Free;
  end;
end;

{***********************************************************************************************************}
{StreamContainOnlyChildNodes mean that the stream contain ONLY the child node, so it's not a valid xml stream
 like <root>...</root> but more like <rec>...</rec><rec>...</rec><rec>...</rec>}
procedure TALXMLNode.LoadFromStream(const Stream: TStream; Const StreamContainOnlyChildNodes: Boolean=False; Const ClearChildNodes: Boolean = True);
Begin
  If not (NodeType in [ntElement, ntDocument]) then ALXmlDocError(CALXMLOperationError,[GetNodeTypeStr]);
  if (not StreamContainOnlyChildNodes) or ClearChildNodes then begin
    ChildNodes.Clear;
    AttributeNodes.Clear;
  end;
  Try
    FDocument.ParseXmlStream(Stream, self, StreamContainOnlyChildNodes);
  except
    ChildNodes.Clear;
    AttributeNodes.Clear;
    raise;
  end;
end;

{****************************************************************************************************}
procedure TALXMLNode.SaveToFile(const AFileName: AnsiString; Const SaveOnlyChildNodes: Boolean=False);
Var LfileStream: TfileStream;
begin
  LfileStream := TfileStream.Create(String(AFileName),fmCreate);
  Try
    SaveToStream(LfileStream, SaveOnlyChildNodes);
  finally
    LfileStream.Free;
  end;
end;

{*************************************************************************************************************}
{load FileContainOnlyChildNodes mean the the stream contain ONLY the child node, so it's not a valid xml stream
 like <root>...</root> but more like <rec>...</rec><rec>...</rec><rec>...</rec>}
procedure TALXMLNode.LoadFromFile(const AFileName: AnsiString; Const FileContainOnlyChildNodes: Boolean=False; Const ClearChildNodes: Boolean = True);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(string(AFileName), fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromStream(LfileStream, FileContainOnlyChildNodes, ClearChildNodes);
  finally
    LfileStream.Free;
  end;
end;

{*******************************************************************************************}
procedure TALXMLNode.SaveToXML(var Xml: AnsiString; Const SaveOnlyChildNodes: Boolean=False);
Var LStringStream: TALStringStream;
begin
  LStringStream := TALStringStream.Create('');
  Try
    SaveToStream(LStringStream, SaveOnlyChildNodes);
    Xml := LStringStream.DataString;
  finally
    LStringStream.Free;
  end;
end;

{************************************************************************************************************}
{load XmlContainOnlyChildNodes mean the the stream contain ONLY the child node, so it's not a valid xml stream
 like <root>...</root> but more like <rec>...</rec><rec>...</rec><rec>...</rec>}
procedure TALXMLNode.LoadFromXML(const Xml: AnsiString; Const XmlContainOnlyChildNodes: Boolean=False; Const ClearChildNodes: Boolean = True);
Var LStringStream: TALStringStream;
Begin
  LStringStream := TALStringStream.Create(Xml);
  Try
    LoadFromStream(LStringStream, XmlContainOnlyChildNodes, ClearChildNodes);
  finally
    LStringStream.Free;
  end;
end;

{*******************************************************************}
{Returns the XML that corresponds to the subtree rooted at this node.
 GetXML returns the XML that corresponds to this node and any child nodes it contains.}
function TALXMLNode.GetXML: AnsiString;
begin
  SaveToXML(Result,false);
end;

{************************************************}
{SetXML reload the node with the new given value }
procedure TALXMLNode.SetXML(const Value: AnsiString);
Begin
  LoadFromXML(Value,False{XmlContainOnlyChildNodes},true{ClearChildNodes});
end;

{*********************************************************}
{Returns the name of the node without any namespace prefix.
 For element nodes, the local name is derived from the name that appears in the XML tag. For example, the local name
 for <xsi:Name First="John" Last="Doe"> is ‘Name’.
 For attribute nodes, the local name is derived from the string that appears to the left of the equals sign in the
 attribute declaration. For example the element <Name First=John Last=Doe> results in two attribute nodes, with the
 local names ‘First’ and ‘Last’. For any other node types, the local name is nil (Delphi) or NULL (C++).
 Note:	If both GetPrefix and GetLocalName return a value for a node, the node name (returned by GetNodeName) is the
 combination of these two values.}
function TALXMLNode.GetLocalName: AnsiString;
begin
  If NodeType in [ntElement, ntattribute] then result := ALExtractLocalName(nodeName)
  else result := '';
end;

{*********************}
{Returns the node name.
 GetNodeName returns the name of the underlying DOM node.
 The node’s name depends on the type of the node, as indicated in the following table:

 NodeType	           NodeName
 ntAttribute	       The attribute name
 ntElement	         The tag name
 ntText	             ‘#text’
 ntCData	           ‘#cdata-section’
 ntEntityRef	       The name of the entity reference.
 ntEntity	           The entity name
 ntProcessingInstr	 the target of the processing instruction
 ntComment	         ‘#comment’
 ntDocument	         ‘#document’
 ntDocType	         The document type name
 ntDocFragment	     ‘#document-fragment’
 ntNotation	         The notation name

 Note:	If GetPrefix and GetLocalName both return values for a node,
 the node name is the combination of these two values}
function TALXMLNode.GetNodeName: AnsiString;
begin
  case NodeType of
    NtElement:         Result := InternalValue;
    NtAttribute:       Result := InternalValue;
    ntText:            Result := '#text';
    ntCData:	         Result := '#cdata-section';
    ntEntityRef:       result := ''; //todo;
    ntEntity:	         result := ''; //todo;
    ntProcessingInstr: result := InternalValue;
    ntComment:	       Result := '#comment';
    ntDocument:	       Result := '#document';
    ntDocType:         result := ''; //todo;
    ntDocFragment:	   Result := '#document-fragment';
    ntNotation:	       result := ''; //todo;
    else begin
      Result := ''; //for hide warning
      AlXMLDocError(cAlXmlInvalidNodeType);
    end;
  end;
end;

{*********************************************}
function TALXMLNode.GetNodeTypeStr: AnsiString;
begin
  case NodeType of
    ntElement: result := 'ntElement';
    ntAttribute: result := 'ntAttribute';
    ntText: result := 'ntText';
    ntCData: result := 'ntCData';
    ntEntityRef: result := 'ntEntityRef';
    ntEntity: result := 'ntEntity';
    ntProcessingInstr: result := 'ntProcessingInstr';
    ntComment: result := 'ntComment';
    ntDocument: result := 'ntDocument';
    ntDocType: result := 'ntDocType';
    ntDocFragment: result := 'ntDocFragment';
    ntNotation: result := 'ntNotation';
    else begin
      Result := ''; //for hide warning
      AlXMLDocError(cAlXmlInvalidNodeType);
    end;
  end;
end;

{********************************************************}
procedure TALXMLNode.SetNodeName(const Value: AnsiString);
var I: integer;
begin

  {Check if the name abides the rules. We will be very forgiving here and
   just accept any name that at least does not contain control characters}
  for I := 1 to length(Value) do
    if Value[I] in [' ', #9, #13, #10] then AlXmlDocError(CALXMLParameterIsIncorrect);
  If Value='' then AlXmlDocError(CALXMLParameterIsIncorrect);

  case NodeType of
    NtElement:         InternalValue := Value;
    NtAttribute:       InternalValue := Value;
    ntText:            ALXmlDocError(CALXmlOperationError,['TEXT']);
    ntCData:	         ALXmlDocError(CALXmlOperationError,['CDATA']);
    ntEntityRef:       ;//todo
    ntEntity:	         ;//todo
    ntProcessingInstr: InternalValue := Value;
    ntComment:	       ALXmlDocError(CALXmlOperationError,['COMMENT']);
    ntDocument:	       ALXmlDocError(CALXmlOperationError,['DOCUMENT']);
    ntDocType:         ;//todo
    ntDocFragment:	   ALXmlDocError(CALXmlOperationError,['DOCFRAGMENT']);
    ntNotation:	       ;//todo
    else AlXMLDocError(cAlXmlInvalidNodeType);
  end;

end;

{***********************************************}
{Returns the namespace prefix of the node’s name.
 GetPrefix returns the namespace prefix of an element or attribute node’s name. A namespace prefix is a symbolic
 name for a namespace URI. For element nodes, the namespace prefix is derived from the name that appears in the
 XML tag. For example, the namespace prefix for <xsi:Name First="John" Last="Doe"> is ‘xsi’. For attribute nodes,
 the namespace prefix is derived from the string that appears to the left of the equals sign in the attribute
 declaration. For example the element <xsi:Name xsn:First="John" Last="Doe"> results in two attribute nodes, with
 namespace prefixes of ‘xsn’ and ‘’, respectively. Note that the attribute never inherits a namespace prefix from
 the node to which it is attached. For any other node types, the namespace prefix is nil (Delphi) or NULL (C++).
 Note:	If both GetPrefix and GetLocalName return a value for a node, the node name (returned by GetNodeName) is
 the combination of these two values.}
function TALXMLNode.GetPrefix: AnsiString;
begin
  If NodeType in [ntElement, ntattribute] then Result := ALExtractPrefix(NodeName)
  else result := '';
end;

{**************************************************}
{Indicates whether the node has a single text value.
 GetIsTextElement returns true if the node represents an XML fragment such as
 <Title> Understanding XML </Title>
 where the node has a tag name (in this case Title) and a single text value (in this case ‘Understanding XML’).
 GetIsTextElement returns false if the node is any other type of node.}
function TALXMLNode.GetIsTextElement: Boolean;
Var LNodeList: TALXMLNodeList;
begin
  LNodeList := InternalGetChildNodes;
  Result := (NodeType=NtElement) and
            ((not assigned(LNodeList)) or
             (LNodeList.Count = 0) or // because NodeList could be created on first access to ChildNodes like Node.ChildNodes.Count for example
             ((LNodeList.Count = 1) and
              (LNodeList[0].nodetype in [ntText, ntCData])));
end;

{*******************************************************}
{Returns the document object in which this node appears.}
function TALXMLNode.GetOwnerDocument: TALXMLDocument;
begin
  Result := FDocument;
end;

{************************}
{returns the parent node.}
function TALXMLNode.GetParentNode: TALXMLNode;
begin
  Result := nil;  //virtual
end;

{******************************************}
{Sets the value of the ParentNode property.}
procedure TALXMLNode.SetParentNode(const Value: TALXMLNode);
begin
  If assigned(Value) then SetOwnerDocument(Value.OwnerDocument)
  else SetOwnerDocument(nil);
end;

{****************************************************}
function TALXMLNode.GetInternalChildValue: AnsiString;
begin
  Result := ''; //virtual
end;

{***********************************************}
function TALXMLNode.GetInternalValue: AnsiString;
begin
  Result := ''; //virtual
end;

{******************************************************************}
procedure TALXMLNode.SetInternalChildValue(const Value: AnsiString);
begin
  //virtual
end;

{*************************************************************}
procedure TALXMLNode.SetInternalValue(const Value: AnsiString);
begin
  //virtual
end;

{*************************************************}
{Get attribute node without create it if not exist}
function TALXMLNode.InternalGetAttributeNodes: TALXMLNodeList;
begin
  Result := nil; //virtual
end;

{********************************************}
{Get Childnode without create it if not exist}
function TALXMLNode.InternalGetChildNodes: TALXMLNodeList;
begin
  Result := nil; //virtual;
end;

{*****************************************************************}
procedure TALXMLNode.SetOwnerDocument(const Value: TALXMLDocument);
var I: Integer;
    LNodeList: TALXmlNodeList;
begin
  FDocument := Value;

  LNodeList := InternalGetChildNodes;
  if Assigned(LNodeList) then
    for I := 0 to LNodeList.Count - 1 do
      LNodeList[I].SetOwnerDocument(Value);

  LNodeList := InternalGetAttributeNodes;
  if Assigned(LNodeList) then
    for I := 0 to LNodeList.Count - 1 do
      LNodeList[I].SetOwnerDocument(Value);
end;

{*****************************************************************}
constructor TALXmlElementNode.Create(const NameOrData: AnsiString);
Var I: integer;
begin
  inherited create(NameOrData);
  FParentNode := nil;
  FAttributeNodes:= nil;
  FChildNodes:=nil;

  {Check if the name abides the rules. We will be very forgiving here and
   just accept any name that at least does not contain control characters}
  for I := 1 to length(NameOrData) do
    if NameOrData[I] in [' ', #9, #13, #10] then AlXmlDocError(CALXMLParameterIsIncorrect);
  If NameOrData='' then AlXmlDocError(CALXMLParameterIsIncorrect);

  FInternalValue:= NameOrData;
end;

{***********************************}
destructor TALXmlElementNode.Destroy;
begin
  If assigned(FattributeNodes) then FreeAndNil(FattributeNodes);
  If assigned(FChildNodes) then FreeAndNil(FchildNodes);
  inherited;
end;

{*******************************************************************************************}
procedure TALXmlElementNode.DoBeforeChildNodesInsert(Index: Integer; const Node: TALXMLNode);
begin
  If Not (Node.NodeType in [ntElement,
                            ntText,
                            ntAttribute,
                            ntCData,
                            ntEntityRef,
                            ntProcessingInstr,
                            ntComment]) then ALXmlDocError(CALXMLParameterIsIncorrect);
end;


{***********************************************************}
function TALXmlElementNode.GetAttributeNodes: TALXMLNodeList;
begin
  if not Assigned(FAttributeNodes) then SetAttributeNodes(CreateAttributeList);
  Result := FAttributeNodes;
end;

{*************************************************************************}
procedure TALXmlElementNode.SetAttributeNodes(const Value: TALXMLNodeList);
begin
  If assigned(FAttributeNodes) then FreeAndNil(FAttributeNodes);
  FAttributeNodes := Value;
end;

{*******************************************************}
function TALXmlElementNode.GetChildNodes: TALXMLNodeList;
begin
  if not Assigned(FChildNodes) then SetChildNodes(CreateChildList);
  Result := FChildNodes;
end;

{*********************************************************************}
procedure TALXmlElementNode.SetChildNodes(const Value: TALXMLNodeList);
begin
  If Assigned(FChildNodes) then FreeAndNil(FchildNodes);
  FChildNodes := Value;
end;

{***************************************************}
function TALXmlElementNode.GetNodeType: TALXMLNodeType;
begin
  Result := NtElement;
end;

{************************}
{returns the parent node.}
function TALXmlElementNode.GetParentNode: TALXMLNode;
begin
  Result := FParentNode;
end;

{******************************************}
{Sets the value of the ParentNode property.}
procedure TALXmlElementNode.SetParentNode(const Value: TALXMLNode);
begin
  inherited;
  FParentNode := Value
end;

{******************************************************}
function TALXmlElementNode.GetInternalValue: AnsiString;
begin
  Result := FInternalValue;
end;

{********************************************************************}
procedure TALXmlElementNode.SetInternalValue(const Value: AnsiString);
begin
  FInternalValue := Value;
end;

{*************************************************}
{Get attribute node without create it if not exist}
function TALXmlElementNode.InternalGetAttributeNodes: TALXMLNodeList;
begin
  Result := FAttributeNodes;
end;

{********************************************}
{Get Childnode without create it if not exist}
function TALXMLElementNode.InternalGetChildNodes: TALXMLNodeList;
begin
  Result := FChildNodes;
end;

{*******************************************************************}
constructor TALXMLAttributeNode.Create(const NameOrData: AnsiString);
Var I: integer;
begin
  inherited create(NameOrData);
  FChildNodes:=nil;

  // Check if the name abides the rules. We will be very forgiving here and
  // just accept any name that at least does not contain control characters
  for I := 1 to length(NameOrData) do
    if NameOrData[I] in [' ', #9, #13, #10] then AlXmlDocError(CALXMLParameterIsIncorrect);
  If NameOrData='' then AlXmlDocError(CALXMLParameterIsIncorrect);

  FInternalValue:= NameOrData;
end;

{*************************************}
destructor TALXMLAttributeNode.Destroy;
begin
  If assigned(FChildNodes) then FreeAndNil(FchildNodes);
  inherited;
end;

{*********************************************************************************************}
procedure TALXmlAttributeNode.DoBeforeChildNodesInsert(Index: Integer; const Node: TALXMLNode);
begin
  If Not (Node.NodeType in [ntText, ntEntityRef]) then ALXmlDocError(CALXMLParameterIsIncorrect);
  if FChildNodes.Count > 0 then ALXmlDocError(CALXMLOnlyOneChildError);
end;

{*********************************************************}
function TALXMLAttributeNode.GetChildNodes: TALXMLNodeList;
begin
  if not Assigned(FChildNodes) then SetChildNodes(CreateChildList);
  Result := FChildNodes;
end;

{***********************************************************************}
procedure TALXMLAttributeNode.SetChildNodes(const Value: TALXMLNodeList);
begin
  If Assigned(FChildNodes) then FreeAndNil(FchildNodes);
  FChildNodes := Value;
end;

{*******************************************************}
function TALXMLAttributeNode.GetNodeType: TALXMLNodeType;
begin
  Result := Ntattribute;
end;

{********************************************************}
function TALXMLAttributeNode.GetInternalValue: AnsiString;
begin
  Result := FInternalValue;
end;

{**********************************************************************}
procedure TALXMLAttributeNode.SetInternalValue(const Value: AnsiString);
begin
  FInternalValue := Value;
end;

{********************************************}
{Get Childnode without create it if not exist}
function TALXMLAttributeNode.InternalGetChildNodes: TALXMLNodeList;
begin
  Result := FChildNodes;
end;

{**************************************************************}
constructor TALXmlTextNode.Create(const NameOrData: AnsiString);
begin
  inherited Create(NameOrData);
  FParentNode := nil;
  FInternalValue:= NameOrData;
end;

{********************************}
destructor TALXmlTextNode.Destroy;
begin
  inherited;
end;

{**************************************************}
function TALXmlTextNode.GetNodeType: TALXMLNodeType;
begin
  Result := NtText;
end;

{************************}
{returns the parent node.}
function TALXmlTextNode.GetParentNode: TALXMLNode;
begin
  Result := FParentNode;
end;

{******************************************}
{Sets the value of the ParentNode property.}
procedure TALXmlTextNode.SetParentNode(const Value: TALXMLNode);
begin
  inherited;
  FParentNode := Value
end;

{***************************************************}
function TALXmlTextNode.GetInternalValue: AnsiString;
begin
  Result := FInternalValue;
end;

{*****************************************************************}
procedure TALXmlTextNode.SetInternalValue(const Value: AnsiString);
begin
  FInternalValue := Value;
end;

{***************************************************************}
constructor TALXmlCDataNode.Create(const NameOrData: AnsiString);
begin
  inherited Create(NameOrData);
  FParentNode := nil;
  FInternalValue:= NameOrData;
end;

{*********************************}
destructor TALXmlCDataNode.Destroy;
begin
  inherited;
end;

{****************************************************}
function TALXmlCDataNode.GetInternalValue: AnsiString;
begin
  Result := FInternalValue;
end;

{***************************************************}
function TALXmlCDataNode.GetNodeType: TALXMLNodeType;
begin
  Result := NtcData;
end;

{*************************************************}
function TALXmlCDataNode.GetParentNode: TALXMLNode;
begin
  Result := FParentNode;
end;

{******************************************************************}
procedure TALXmlCDataNode.SetInternalValue(const Value: AnsiString);
begin
  FInternalValue := Value;
end;

{***************************************************************}
procedure TALXmlCDataNode.SetParentNode(const Value: TALXMLNode);
begin
  inherited;
  FParentNode := Value
end;

{*****************************************************************}
constructor TALXmlCommentNode.Create(const NameOrData: AnsiString);
begin
  inherited Create(NameOrData);
  FParentNode := nil;
  FInternalValue:= NameOrData;
end;

{***********************************}
destructor TALXmlCommentNode.Destroy;
begin
  inherited;
end;

{*****************************************************}
function TALXmlCommentNode.GetNodeType: TALXMLNodeType;
begin
  Result := NtComment;
end;

{************************}
{returns the parent node.}
function TALXmlCommentNode.GetParentNode: TALXMLNode;
begin
  Result := FParentNode;
end;

{******************************************}
{Sets the value of the ParentNode property.}
procedure TALXmlCommentNode.SetParentNode(const Value: TALXMLNode);
begin
  inherited;
  FParentNode := Value
end;

{******************************************************}
function TALXmlCommentNode.GetInternalValue: AnsiString;
begin
  Result := FInternalValue;
end;

{********************************************************************}
procedure TALXmlCommentNode.SetInternalValue(const Value: AnsiString);
begin
  FInternalValue := Value;
end;

{************************************************************************}
constructor TALXmlProcessingInstrNode.Create(const NameOrData: AnsiString;
                                             const AddlData: AnsiString = '');
Var I: integer;
begin
  inherited;
  FParentNode := nil;

  // Check if the name abides the rules. We will be very forgiving here and
  // just accept any name that at least does not contain control characters
  for I := 1 to length(NameOrData) do
    if NameOrData[I] in [' ', #9, #13, #10] then AlXmlDocError(CALXMLParameterIsIncorrect);
  If NameOrData='' then AlXmlDocError(CALXMLParameterIsIncorrect);

  FInternalValue:= NameOrData;
  FInternalChildValue:= addldata;
end;

{*******************************************}
destructor TALXmlProcessingInstrNode.Destroy;
begin
  inherited;
end;

{*************************************************************}
function TALXmlProcessingInstrNode.GetNodeType: TALXMLNodeType;
begin
  Result := ntProcessingInstr;
end;

{************************}
{returns the parent node.}
function TALXmlProcessingInstrNode.GetParentNode: TALXMLNode;
begin
  Result := FParentNode;
end;

{******************************************}
{Sets the value of the ParentNode property.}
procedure TALXmlProcessingInstrNode.SetParentNode(const Value: TALXMLNode);
begin
  inherited;
  FParentNode := Value
end;

{*******************************************************************}
function TALXmlProcessingInstrNode.GetInternalChildValue: AnsiString;
begin
  Result := FInternalChildValue;
end;

{**************************************************************}
function TALXmlProcessingInstrNode.GetInternalValue: AnsiString;
begin
  Result := FInternalValue;
end;

{*********************************************************************************}
procedure TALXmlProcessingInstrNode.SetInternalChildValue(const Value: AnsiString);
begin
  FInternalChildValue := Value;
end;

{****************************************************************************}
procedure TALXmlProcessingInstrNode.SetInternalValue(const Value: AnsiString);
begin
  FInternalValue := Value;
end;

{********************************************************************}
constructor TALXmlDocumentNode.Create(const OwnerDoc: TALXMLDocument);
begin
  inherited Create('');
  FDocument := OwnerDoc;
  FChildNodes:=nil;
end;

{************************************}
destructor TALXmlDocumentNode.Destroy;
begin
  If assigned(FChildNodes) then FreeAndNil(FchildNodes);
  inherited;
end;

{********************************************************************************************}
procedure TALXmlDocumentNode.DoBeforeChildNodesInsert(Index: Integer; const Node: TALXMLNode);
var LNodeList: TALXMLNodelist;
    I: integer;
begin
  LNodeList := InternalGetChildNodes;
  If assigned(LNodeList) then
    For I:=0 to LNodeList.Count - 1 do
      If LNodeList[I].NodeType = ntelement then alXmlDocError(CalXMLOnlyOneTopLevelError);

  If Not (Node.NodeType in [ntelement, ntProcessingInstr, ntComment, ntDocType]) then ALXmlDocError(CALXMLParameterIsIncorrect);

  inherited;
end;

{********************************************************}
function TALXmlDocumentNode.GetChildNodes: TALXMLNodeList;
begin
  if not Assigned(FChildNodes) then SetChildNodes(CreateChildList);
  Result := FChildNodes;
end;

{**********************************************************************}
procedure TALXmlDocumentNode.SetChildNodes(const Value: TALXMLNodeList);
begin
  If Assigned(FChildNodes) then FreeAndNil(FchildNodes);
  FChildNodes := Value;
end;

{******************************************************}
function TALXmlDocumentNode.GetNodeType: TALXMLNodeType;
begin
  Result := NtDocument;
end;

{********************************************}
{Get Childnode without create it if not exist}
function TALXmlDocumentNode.InternalGetChildNodes: TALXMLNodeList;
begin
  Result := FChildNodes;
end;

{************************************}
{Instantiates a TALXMLNodeList object.
 Create instantiates a new TALXMLNodeList object. Typically, TALXMLNodeList is created by another object
 that represents a node. The TALXMLNodeList represents a set of nodes that are of interest to the other
 object that creates it.
 *AOwner is the node that creates TALXMLNodeList to represent a set of its child nodes or attributes.}
constructor TALXMLNodeList.Create(Owner: TALXMLNode);
begin
  FList:= nil;
  FCount:= 0;
  FCapacity := 0;
  FOwner := Owner;
end;

{********************************}
destructor TALXMLNodeList.Destroy;
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
function TALXMLNodeList.IndexOf(const Node: TALXMLNode): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList[Result] <> Node) do Inc(Result);
  if Result = FCount then Result := -1;
end;

{*************************************}
{Returns the index of a specified node.
 Call IndexOf to locate a node in the list.
 *Name is the LocalName property of the node to locate.
 IndexOf returns the index of the specified node, where 0 is the index of the first node, 1 is the
 index of the second node, and so on. If the specified node is not in the list, IndexOf returns -1.}
function TALXMLNodeList.IndexOf(const Name: AnsiString): Integer;
begin
  for Result := 0 to Count - 1 do
    if ALNodeMatches(Get(Result), Name) then Exit;
  Result := -1;
end;

{**************************************}
{Returns a specified node from the list.
 Call FindNode to access a particular node in the list.
 *NodeName is the node to access. It specifies the LocalName property of the desired node.
 FindNode returns the object of the node if it is in the list. If NodeName does not specify a node in the list,
 FindNode returns nil (Delphi) or NULL (C++).}
function TALXMLNodeList.FindNode(const NodeName: AnsiString): TALXMLNode;
var Index: Integer;
begin
  Index := IndexOf(NodeName);
  if Index >= 0 then Result := Get(Index)
  else Result := nil;
end;

{**************************************}
{Returns a specified node from the list.
 Call FindNode to access a particular node in the list.
 *NodeName is the node to access. It specifies the LocalName property of the desired node.
 *NodeAttributes is the attributes the node must have. ex: ['attribute1=value1', 'attribute2=value2']
 FindNode returns the object of the node if it is in the list. If NodeName and NodeAttributes  does not specify a node in the list,
 FindNode returns nil (Delphi) or NULL (C++).}
function TALXMLNodeList.FindNode(const NodeName: AnsiString; NodeAttributes: Array of ansiString): TALXMLNode; // [added from TXMLNodeList]

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _AttributesMatches(const Node: TALXMlNode): boolean;
  var I, P: integer;
  begin
    Result := True;
    for I := low(NodeAttributes) to high(NodeAttributes) do begin
      P := ALpos('=',NodeAttributes[i]);
      if (P <= 0) or
         (Node.Attributes[AlCopyStr(NodeAttributes[i], 1, P-1)] <> AlCopyStr(NodeAttributes[i], P+1, maxint)) then begin
        Result := False;
        Exit;
      end;
    end;
  end;

var Index: Integer;

begin
  for Index := 0 to Count - 1 do begin
    Result := Get(Index);
    if ALNodeMatches(Result, NodeName) and _AttributesMatches(Result) then Exit;
  end;
  Result := nil;
end;

{**********************************}
{Returns the first node in the list.
Call First to access the first node in the list. If the list is empty, First raises an exception}
function TALXMLNodeList.First: TALXMLNode;
begin
  if Count > 0 then Result := Get(0)
  else Result := nil;
end;

{*********************************}
{Returns the last node in the list.
 Call Last to access the last node in the list. If the list is empty, Last raises an exception.}
function TALXMLNodeList.Last: TALXMLNode;
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
function TALXMLNodeList.FindSibling(const Node: TALXMLNode; Delta: Integer): TALXMLNode;
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
function TALXMLNodeList.Get(Index: Integer): TALXMLNode;
begin
  if (Index < 0) or (Index >= FCount) then ALXMLDocError(CALXmlListIndexError, [Index]);
  Result := FList[Index];
end;

{**************************************}
{Returns a specified node from the list.
 GetNode is the read implementation of the Nodes property.
 *Index identify the desired node. 0 is the index of the first node,
  1 is the index of the second node, and so on}
function TALXMLNodeList.GetNodeByIndex(const Index: Integer): TALXMLNode;
begin
  Result := Get(Index);
end;

{**************************************}
{Returns a specified node from the list.
 GetNode is the read implementation of the Nodes property.
 *Name identify the desired node. it is the LocalName property of a node in the list.
 If Name does not identify a node in the list, GetNode tries to create a new node with the name specified by
 Name. If it can’t create the new node, GetNode raises an exception.}
function TALXMLNodeList.GetNodeByName(const Name: AnsiString): TALXMLNode;
begin
  Result := FindNode(Name);
  if (not Assigned(Result)) and
     (assigned(fOwner.OwnerDocument)) and
     (doNodeAutoCreate in fOwner.OwnerDocument.Options) and
     ((FOwner.GetNodeType <> ntDocument) or (FOwner.OwnerDocument.DocumentElement = nil)) // Don't try to autocreate a second document element !
  then Result := FOwner.AddChild(Name);
  if not Assigned(Result) then ALXMLDocError(CALXmlNodeNotFound, [Name]);
end;

{*************************************************************************************}
procedure TALXMLNodeList.QuickSort(L, R: Integer; ACompare: TALXMLNodeListSortCompare);
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

{**********************************************************************}
procedure TALXMLNodeList.CustomSort(Compare: TALXMLNodeListSortCompare);
begin
  if (FList <> nil) and (Count > 1) then
    QuickSort(0, Count - 1, Compare);
end;

{**************************************}
{Adds a new node to the end of the list.
 Call Add to add a node to the end of the list. Add returns the index of the node once it is added, where 0 is the index
 of the first node in the list, 1 is the index of the second node, and so on.
 *Node is the node to add to the list.}
function TALXMLNodeList.Add(const Node: TALXMLNode): Integer;
begin
  Insert(-1, Node);
  Result := FCount - 1;
end;

{**********************************************************}
{Provides the underlying implementation of the Insert method
 The Insert method calls InternalInsert to add a node at the position specified by Index.
 Index specifies where to insert the node, where 0 is the first position, 1 is second position, and so on.
 If Index is –1, the node is added to the end of the list.
 Node is the node to add to the list.
 InternalInsert returns the index of the node after it is inserted. This is the same as the Index parameter when
 Index is not –1.}
Function TALXMLNodeList.InternalInsert(Index: Integer; const Node: TALXMLNode): integer;
begin
  Owner.DoBeforeChildNodesInsert(Index, Node);

  if Index = -1 then begin
    index := FCount;
    if index = FCapacity then Grow;
  end
  else begin
    if (Index < 0) or (Index > FCount) then ALXMLDocError(CALXmlListIndexError, [Index]);
    if FCount = FCapacity then Grow;
    if Index < FCount then ALMove(FList[Index],
                                  FList[Index + 1],
                                  (FCount - Index) * SizeOf(Pointer));
  end;
  Pointer(FList[index]) := nil;
  FList[index] := Node;
  Inc(FCount);
  Node.SetParentNode(Fowner);
  result := index;
end;

{********************************************************}
{Inserts a new node into a specified position in the list.
 Call Insert to add a node at the position specified by Index.
 *Index specifies where to insert the node, where 0 is the first position, 1 is second position, and so on. If Index does not
  specify a valid index, Insert raises an exception.
 *Node is the node to add to the list.}
procedure TALXMLNodeList.Insert(Index: Integer; const Node: TALXMLNode);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure InsertFormattingNode(const Len, Index: Integer; Break: Boolean = True);
  var I: Integer;
      IndentNode: TALXMLNode;
      IndentStr: AnsiString;
  begin
    for I := 1 to Len do IndentStr := IndentStr + Owner.OwnerDocument.NodeIndentStr;
    if Break then IndentStr := SLineBreak + IndentStr;
    with Owner do
      IndentNode := ALCreateXmlNode(IndentStr, ntText, '');
    InternalInsert(Index, IndentNode);
  end;

var TrailIndent, NewIndex: Integer;

begin
  { Determine if we should add do formatting here }
  if Assigned(Owner.ParentNode) and
     Assigned(Owner.OwnerDocument) and
     (doNodeAutoIndent in Owner.OwnerDocument.Options) and
     (not (Node.NodeType in [ntText, ntAttribute])) then
  begin
    { Insert formatting before the node }
    if Count = 0 then InsertFormattingNode(Owner.ParentNode.NestingLevel, -1);
    if Index = -1 then InsertFormattingNode(1, -1, False);
    { Insert the actual node }
    NewIndex := InternalInsert(Index, Node);
    { Insert formatting after the node }
    if Index = -1 then TrailIndent := Owner.ParentNode.NestingLevel
    else TrailIndent := Owner.NestingLevel;
    InsertFormattingNode(TrailIndent, NewIndex + 1)
  end
  else InternalInsert(Index, Node);
end;

{**************************************}
{Removes a specified node from the list.
 Delete removes the node specified by the Index or Name parameter.
 *Index identifies the node to remove by index rather than name. Index ranges from 0 to one less than the value of the Count property.
 Delete returns the index of the node that was removed. If there was no node that matched the value of Index Delete returns –1.}
function TALXMLNodeList.Delete(const Index: Integer): Integer;
var Node: TALXMLNode;
begin
  Node := Get(Index);
  Dec(FCount);
  if Index < FCount then begin
    FList[Index] := nil;
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
function TALXMLNodeList.Delete(const Name: AnsiString): Integer;
begin
  result := indexOf(Name);
  if Result >= 0 then Delete(Result);
end;

{**************************************}
{Removes a specified node from the list.
 Remove removes the specified node from the list.
 *Node is the node to remove from the list.
 Remove returns the index of Node before it was removed. If node is not a node in the list, Remove returns -1.}
function TALXMLNodeList.Remove(const Node: TALXMLNode): Integer;
begin
  Result := IndexOf(Node);
  if Result >= 0 then Delete(Result);
end;

{************************************************************}
{Removes a specified object from the list without freeing it.
 Call Extract to remove an object from the list without freeing the object itself.
 After an object is removed, all the objects that follow it are moved up in index position and Count is decremented.}
function TALXMLNodeList.Extract(const Node: TALXMLNode): TALXMLNode;
var I: Integer;
begin
  Result := nil;
  I := IndexOf(Node);
  if I >= 0 then result := Extract(i);
end;

{*********************************************************}
procedure TALXMLNodeList.Exchange(Index1, Index2: Integer);
var Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then ALXMLDocError(cALXmlListIndexError, [Index1]);
  if (Index2 < 0) or (Index2 >= FCount) then ALXMLDocError(cALXmlListIndexError, [Index2]);
  Item := pointer(FList[Index1]);
  pointer(FList[Index1]) := pointer(FList[Index2]);
  pointer(FList[Index2]) := Item;
end;

{***********************************************************}
{Removes a specified object from the list without freeing it.
 Call Extract to remove an object from the list without freeing the object itself.
 After an object is removed, all the objects that follow it are moved up in index position and Count is decremented.}
function TALXMLNodeList.Extract(const index: integer): TALXMLNode;
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
function TALXMLNodeList.ReplaceNode(const OldNode, NewNode: TALXMLNode): TALXMLNode;
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
procedure TALXMLNodeList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

{****************************}
procedure TALXMLNodeList.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4
  else if FCapacity > 8 then Delta := 16
  else Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

{*********************************************************}
procedure TALXMLNodeList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) then ALXMLDocError(CALXmlListCapacityError, [NewCapacity]);
  if NewCapacity <> FCapacity then begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{***************************************************}
procedure TALXMLNodeList.SetCount(NewCount: Integer);
var I: Integer;
begin
  if (NewCount < 0) then ALXMLDocError(CALXmlListCountError, [NewCount]);
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else for I := FCount - 1 downto NewCount do Delete(I);
  FCount := NewCount;
end;

{********************************************************}
Function  ALFindXmlNodeByChildNodeValue(xmlrec:TalxmlNode;
                                        Const ChildNodeName, ChildNodeValue : AnsiString;
                                        Const Recurse: Boolean = False): TalxmlNode;
var I, J : integer;
Begin
  result := nil;
  if not (xmlrec is TalXmlElementNode) then Exit;
  for i := 0 to xmlrec.ChildNodes.Count - 1 do begin
    for J := 0 to xmlrec.ChildNodes[i].ChildNodes.Count - 1 do begin
      If ALSametext(xmlrec.ChildNodes[i].ChildNodes[j].NodeName,ChildNodeName) and
         ALSametext(xmlrec.ChildNodes[i].ChildNodes[j].text,ChildNodeValue) then begin
        result := xmlrec.ChildNodes[i];
        exit;
      end;
    end;
    if Recurse then begin
      result := ALFindXmlNodeByChildNodeValue(xmlrec.ChildNodes[i],
                                              ChildNodeName,
                                              ChildNodeValue,
                                              Recurse);
      if assigned(Result) then break;
    end;
  end;
end;

{***************************************************************}
Function  ALFindXmlNodeByNameAndChildNodeValue(xmlrec:TalxmlNode;
                                               Const NodeName: ansiString;
                                               Const ChildNodeName, ChildNodeValue: AnsiString;
                                               Const Recurse: Boolean = False): TalxmlNode;
var I, J : integer;
Begin
  result := nil;
  if not (xmlrec is TalXmlElementNode) then Exit;
  for i := 0 to xmlrec.ChildNodes.Count - 1 do begin
    if ALSametext(xmlrec.ChildNodes[i].NodeName,NodeName) then begin
      for J := 0 to xmlrec.ChildNodes[i].ChildNodes.Count - 1 do begin
        If ALSametext(xmlrec.ChildNodes[i].ChildNodes[j].NodeName,ChildNodeName) and
           ALSametext(xmlrec.ChildNodes[i].ChildNodes[j].text,ChildNodeValue) then begin
          result := xmlrec.ChildNodes[i];
          exit;
        end;
      end;
    end;
    if Recurse then begin
      result := ALFindXmlNodeByNameAndChildNodeValue(xmlrec.ChildNodes[i],
                                                     NodeName,
                                                     ChildNodeName,
                                                     ChildNodeValue,
                                                     Recurse);
      if assigned(Result) then break;
    end;
  end;
end;

{***************************************************}
Function  ALFindXmlNodeByAttribute(xmlrec:TalxmlNode;
                                   Const AttributeName, AttributeValue : AnsiString;
                                   Const Recurse: Boolean = False): TalxmlNode;

var I : integer;
Begin
  result := nil;
  if not (xmlrec is TalXmlElementNode) then Exit;
  for I := 0 to xmlrec.ChildNodes.Count - 1 do begin
    If ALSametext(xmlrec.ChildNodes[I].Attributes[AttributeName], AttributeValue) then begin
      result := xmlrec.ChildNodes[I];
      break;
    end;
    if Recurse then begin
      result := ALFindXmlNodeByAttribute(xmlrec.ChildNodes[I],
                                         AttributeName,
                                         AttributeValue,
                                         Recurse);
      if assigned(Result) then break;
    end;
  end;
end;

{**********************************************************}
Function  ALFindXmlNodeByNameAndAttribute(xmlrec:TalxmlNode;
                                          Const NodeName: ansiString;
                                          Const AttributeName, AttributeValue: AnsiString;
                                          Const Recurse: Boolean = False): TalxmlNode;
var I : integer;
Begin
  result := nil;
  if not (xmlrec is TalXmlElementNode) then Exit;
  for I := 0 to xmlrec.ChildNodes.Count - 1 do begin
    If ALSametext(xmlrec.ChildNodes[I].NodeName, NodeName) and
       ALSametext(xmlrec.ChildNodes[I].Attributes[AttributeName], AttributeValue) then begin
      result := xmlrec.ChildNodes[I];
      break;
    end;
    if Recurse then begin
      result := ALFindXmlNodeByNameAndAttribute(xmlrec.ChildNodes[I],
                                                NodeName,
                                                AttributeName,
                                                AttributeValue,
                                                Recurse);
      if assigned(Result) then break;
    end;
  end;
end;

initialization
  vALDefaultNodeIndent := '  '; { 2 spaces }

end.
