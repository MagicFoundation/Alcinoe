{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALXmlDocument
Version:      3.56

Description:  TALXmlDocument is exactly like Delphi TXmlDocument (Same functions 
              and procedures) but 10 to 100 times more fastly (see demo) and can 
              work in sax mode ! 

              Use TAlXMLDocument to represent an XML document. TAlXMLDocument can
              read an existing XML document from a file, it can be associated with
              an in-memory string that is the contents of an XML document, or it 
              can create a new, empty XML document.

              You can use TALXMLDocument directly to load an XML document, read and 
              edit it, and save any changes.

              TALXMLDocument uses an internal parser to analyze the XML document.

              When you set the Active property to true, TALXMLDocument parse the XML
              document so that you can examine or modify it. In particular, the
              DocumentElement property provides an interface to the root node of the 
              document. You can use that interface to access its child nodes and to
              add or delete child nodes.

Legal issues: Copyright (C) 1999-2010 by Arkadia Software Engineering

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

History :     27/05/2006: Add loadfromstream, loadfromfile,
                          savetostream and savetofile to TALXmlNode;
              25/10/2007: Add support for <![CDATA[ ... ]]>
              24/11/2007: Add CustomSort to TALXMLNodeList
                          Add Exchange to TALXMLNodeList
                          Add AttributesAsString property
              02/11/2008: correct bug in setattribute when
                          2 attributes have the same name
              10/11/2008: add poIgnoreXMLReferences in TALXMLParseOption
              						can decode references like &#39; or &#x20AC;
              						(and the 5 main reference, &lt; &gt; &apos; &aquot; &amp;
              27/12/2008: add that isTextElement return true if node have only one
                          child of type CDATA
              02/01/2009: don't check if ]]> is present in the text when creating
                          cdata node
              12/01/2009: rename loadonlychildnode by streamcontainonlychildnode in
                          TalXmlNode.loadFromStream to not make any confusion
              01/03/2009: Correct "parse error" bug
Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALXmlDoc;

interface

uses Classes,
     sysutils;

resourcestring
  cALXmlActive               = 'Document is active';
  cALXmlNotActive            = 'No active document';
  cAlXmlNodeNotFound         = 'Node "%s" not found';
  cALXmlInvalidNodeType      = 'Invalid node type';
  cALXmlNotSingleTextNode    = 'Element does not contain a single text node';
  cALXmlListCapacityError    = 'Node list capacity out of bounds (%d)';
  cALXmlListCountError       = 'Node list count out of bounds (%d)';
  cALXmlListIndexError       = 'Node list index out of bounds (%d)';
  cALXmlMissingFileName      = 'FileName cannot be blank';
  cALXmlNoRefresh            = 'Refresh is only supported if the FileName or XML properties are set';
  cALXmlOperationError       = 'This operation can not be performed with a node of type %s';
  cALXmlParseError           = 'Xml Parse error';
  CALXMLOnlyOneTopLevelError = 'Only one top level element is allowed in an XML document';
  CALXMLOnlyOneChildError    = 'Only one child is allowed in an attribute node';
  CALXMLParameterIsIncorrect = 'The parameter is incorrect';


type

  {class definition}
  TALXMLNode = Class;
  TALXMLNodeList= Class;
  TALXMLDocument= Class;

  {String definition. The component use only AnsiString and work with multi-bites characteres throught UTF-8 encoding}
  TALXMLString=String;

  TAlXMLParseProcessingInstructionEvent = procedure (Sender: TObject; const Target, Data: TALXMLString) of object;
  TAlXMLParseTextEvent = procedure (Sender: TObject; const str: TALXMLString) of object;
  TAlXMLParseStartElementEvent = procedure (Sender: TObject; const Name: TALXMLString; const Attributes: Tstrings) of object;
  TAlXMLParseEndElementEvent = procedure (Sender: TObject; const Name: TALXMLString) of object;

  TALXMLNodeListSortCompare = function(List: TALXMLNodeList; Index1, Index2: Integer): Integer;

  TALXMLEncodingType = (
    xetUnknown,          //The character set is unknown.
    xetUCS_4BE,          //[not implemented yet !] UCS-4, big-endian (1234).
    xetUCS_4LE,          //[not implemented yet !] UCS-4, little-endian (4321).
    xetUCS_4Order2134,   //[not implemented yet !] UCS-4, unusual octet order (2134)
    xetUCS_4Order3412,   //[not implemented yet !] UCS-4, unusual octet order (3412)
    xetUTF_16BE,         //[not implemented yet !] UTF-16, big-endian order
    xetUTF_16LE,         //[not implemented yet !] UTF-16, little-endian order
    xetUTF_8,            //UTF-8
    xetUCS_4Like,        //[not implemented yet !] Any UCS-4 encoding
    xetUTF_16BELike,     //[not implemented yet !] Any UTF-16, big-endian encoding, may be without byte order mark
    xetUTF_16LELike,     //[not implemented yet !] Any UTF-16, little-endian encoding, my be without byte order mark
    xetUTF_8Like,        //UTF-8, ISO 646, ASCII, or other 7- or 8-bit encoding, including multi-byte character sets such as Shift-JIS.
    xetEBCDICLike        //[not implemented yet !] EBCDIC
  );

  TALXmlNodeType = (
                    ntReserved,         //Not used
                    ntElement,          //The node represents an element. Element nodes represent simple tags that have child nodes. The child nodes of an element node can have the following node types: ntElement, ntText, ntCData, ntEntityRef, ntProcessingInstr, and ntComment. Element nodes can also have attributes (ntAttribute). Element nodes can be the child of a node of type ntDocument, ntDocFragment, ntEntityRef, and ntElement.
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
                    ntNotation          //[not implemented yet !] A node represents a notation in the document type declaration. It always appears as the child of an ntDocType node and never has any child nodes.
                   );

  TALXMLDocOption = (
                     doNodeAutoIndent, //When formatting the XML text from the parsed set of nodes, child nodes are automatically indented from their parent nodes.
                     doAttrNull,       //When reading the value of an attribute that does not exist, the value is given as a Null Variant (as opposed to a value of an empty string).
                     doAutoSave        //When the XML document closes, any changes are automatically saved back to the XML document file or to the XML property.
                    );
  TALXMLDocOptions = set of TALXMLDocOption;

  TALXMLParseOption = (
                       poPreserveWhiteSpace, //White space in the text of the XML document is not stripped off.
                       poIgnoreXMLReferences //don't decode xml entities (like &amp;) and not encode them also (when save / load)
                      );
  TALXMLParseOptions = set of TALXMLParseOption;

  TALXMLDocumentSource = (
                          xdsNone,
                          xdsXMLProperty,
                          xdsFile,
                          xdsStream
                         );

  TALXMLPrologItem = (
                      xpVersion,
                      xpEncoding,
                      xpStandalone
                     );

  PALPointerXMLNodeList = ^TALPointerXMLNodeList;
  TALPointerXMLNodeList = array[0..MaxListSize - 1] of TALXMLNode;

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
    FList: PALPointerXMLNodeList; //[Replace from TXMLNodeList] FList: IInterfaceList;
    FOwner: TALXMLNode;
    procedure QuickSort(L, R: Integer; XCompare: TALXMLNodeListSortCompare); // [added from TXMLNodeList]
  protected
    //[Deleted from TXMLNodeList] function DoNotify(Operation: TNodeListOperation; const Node: IXMLNode; const IndexOrName: OleVariant; BeforeOperation: Boolean): IXMLNode;
    //[Deleted from TXMLNodeList] function InternalInsert(Index: Integer; const Node: TALXMLNode): Integer;
    //[Deleted from TXMLNodeList] property DefaultNamespaceURI: TALXMLString read GetDefaultNamespaceURI;
    //[Deleted from TXMLNodeList] property List: IInterfaceList read FList;
    //[Deleted from TXMLNodeList] property NotificationProc: TNodeListNotification read FNotificationProc;
    procedure Grow; // [added from TXMLNodeList]
    procedure SetCapacity(NewCapacity: Integer); // [added from TXMLNodeList]
    procedure SetCount(NewCount: Integer); // [added from TXMLNodeList]
    property Owner: TALXMLNode read FOwner;
    function GetCount: Integer;
    function Get(Index: Integer): TALXMLNode;
    function GetNode(const IndexOrName: OleVariant): TALXMLNode;
    function GetNodeByIndex(Const Index: Integer): TALXMLNode; // [added from TXMLNodeList]
    function GetNodeByName(Const Name: TALXmlString): TALXMLNode; // [added from TXMLNodeList]
    Function InternalInsert(Index: Integer; const Node: TALXMLNode): integer;
  public
    //[Deleted from TXMLNodeList] function Delete(const Name, NamespaceURI: TALXMLString): Integer; overload;
    //[Deleted from TXMLNodeList] function FindNode(NodeName, NamespaceURI: TALXMLString): TALXMLNode; overload;
    //[Deleted from TXMLNodeList] function FindNode(ChildNodeType: TGuid): IXMLNode; overload;
    //[Deleted from TXMLNodeList] function GetUpdateCount: Integer;
    //[Deleted from TXMLNodeList] function IndexOf(const Name, NamespaceURI: TALXMLString): Integer; overload;
    //[Deleted from TXMLNodeList] procedure BeginUpdate;
    //[Deleted from TXMLNodeList] procedure EndUpdate;
    //[Deleted from TXMLNodeList] property UpdateCount: Integer read GetUpdateCount;
    constructor Create(Owner: TALXMLNode); //[Replace from TXMLNodeList] constructor Create(Owner: TXMLNode; const DefaultNamespaceURI: DOMString; NotificationProc: TNodeListNotification);
    destructor Destroy; override;
    procedure CustomSort(Compare: TALXMLNodeListSortCompare); // [added from TXMLNodeList]
    function Add(const Node: TALXMLNode): Integer;
    function Delete(const Index: Integer): Integer; overload;
    function Delete(const Name: TALXMLString): Integer; overload;
    function Extract(const index: integer): TALXMLNode; overload; // [added from TXMLNodeList]
    function Extract(const Node: TALXMLNode): TALXMLNode; overload; // [added from TXMLNodeList]
    procedure Exchange(Index1, Index2: Integer); // [added from TXMLNodeList]
    function FindNode(NodeName: TALXMLString): TALXMLNode; overload;
    function FindSibling(const Node: TALXMLNode; Delta: Integer): TALXMLNode;
    function First: TALXMLNode;
    function IndexOf(const Name: TALXMLString): Integer; overload;
    function IndexOf(const Node: TALXMLNode): Integer; overload;
    function Last: TALXMLNode;
    function Remove(const Node: TALXMLNode): Integer;
    function ReplaceNode(const OldNode, NewNode: TALXMLNode): TALXMLNode;
    procedure Clear;
    procedure Insert(Index: Integer; const Node: TALXMLNode);
    property Count: Integer read GetCount;
    property Nodes[const IndexOrName: OleVariant]: TALXMLNode read GetNode; default;
    property NodeByName[const Name: TALXMLString]: TALXMLNode read GetNodeByName; // [added from TXMLNodeList]
    property NodeByIndex[const Index: integer]: TALXMLNode read GetNodeByIndex; // [added from TXMLNodeList]
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
    FDocument: TALXMLDocument;
  protected
    //[Deleted from TXMLNode] function _AddRef: Integer; stdcall;
    //[Deleted from TXMLNode] function _Release: Integer; stdcall;
    //[Deleted from TXMLNode] function GetHostedNodes: TALXMLNodeArray;
    //[Deleted from TXMLNode] function GetPrefixedName(const Name, NamespaceURI: TALXMLString): TALXMLString;
    //[Deleted from TXMLNode] function GetChildNodeClasses: TNodeClassArray;
    //[Deleted from TXMLNode] function GetCollection: TALXMLNodeCollection;
    //[Deleted from TXMLNode] function GetDOMNode: IDOMNode;
    //[Deleted from TXMLNode] function GetHostNode: TALXMLNode;
    //[Deleted from TXMLNode] function GetNamespaceURI: TALXMLString;
    //[Deleted from TXMLNode] function GetNodeObject: TALXMLNode;
    //[Deleted from TXMLNode] function GetReadOnly: Boolean;
    //[Deleted from TXMLNode] function HasChildNode(const ChildTag, NamespaceURI: TALXMLString): Boolean; overload;
    //[Deleted from TXMLNode] function HasChildNode(const ChildTag: TALXMLString): Boolean; overload;
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
    //[Deleted from TXMLNode] function CreateCollection(const CollectionClass: TALXMLNodeCollectionClass; const ItemIterface: TGuid; const ItemTag: TALXMLString; ItemNS: TALXMLString = ''): TALXMLNodeCollection;
    //[Deleted from TXMLNode] function DOMElement: IDOMElement;
    //[Deleted from TXMLNode] function FindHostedNode(const NodeClass: TALXMLNodeClass): TALXMLNode;
    //[Deleted from TXMLNode] procedure RegisterChildNode(const TagName: TALXMLString; ChildNodeClass: TALXMLNodeClass; NamespaceURI: TALXMLString = '');
    //[Deleted from TXMLNode] procedure RegisterChildNodes(const TagNames: array of TALXMLString; const NodeClasses: array of TALXMLNodeClass);
    //[Deleted from TXMLNode] property ChildNodeClasses: TNodeClassArray read GetChildNodeClasses;
    //[Deleted from TXMLNode] property HostNode: TALXMLNode read GetHostNode;
    procedure DoBeforeChildNodesInsert(Index: Integer; const Node: TALXMLNode); virtual; // [added from TXMLNode]
    function CreateAttributeList: TALXMLNodeList;
    function CreateChildList: TALXMLNodeList;
    function InternalGetAttributeNodes: TALXMLNodeList; virtual; // [added from TXMLNode]
    function InternalGetChildNodes: TALXMLNodeList; virtual; // [added from TXMLNode]
    function GetInternalChildValue: TALXMLString; virtual; // [added from TXMLNode]
    function GetInternalValue: TALXMLString; virtual; // [added from TXMLNode]
    function GetAttribute(const AttrName: TALXMLString): OleVariant;
    function GetAttributeAsString(const AttrName: TALXMLString): TALXMLString; // [added from TXMLNode]
    function GetAttributeNodes: TALXMLNodeList; virtual;
    function GetChildNodes: TALXMLNodeList; virtual;
    function GetChildValue(const IndexOrName: OleVariant): OleVariant;
    function GetHasChildNodes: Boolean;
    function GetIsTextElement: Boolean;
    function GetLocalName: TALXMLString;
    function GetNodeName: TALXMLString;
    function GetNodeType: TALXMLNodeType; virtual;
    function GetNodeValue: OleVariant;
    function GetOwnerDocument: TALXMLDocument;
    function GetParentNode: TALXMLNode; virtual;
    function GetPrefix: TALXMLString;
    function GetText: TALXMLString;
    function GetXML: TALXMLString;
    function InternalAddChild(const NodeName: TALXMLString; Index: Integer): TALXMLNode; //[Replace from TXMLNode] function InternalAddChild(NodeClass: TALXMLNodeClass; const NodeName, NamespaceURI: TALXMLString; Index: Integer): TALXMLNode;
    function NestingLevel: Integer;
    procedure CheckTextNode;
    procedure ClearDocumentRef;
    procedure SetAttributeNodes(const Value: TALXMLNodeList); virtual;
    procedure SetChildNodes(const Value: TALXMLNodeList); virtual;
    procedure SetDocumentRef(ADocumentRef: TALXmlDocument); // [added from TXMLNode]
    procedure SetInternalChildValue(const Value: TALXMLString); virtual; // [added from TXMLNode]
    procedure SetInternalValue(const Value: TALXMLString); virtual; // [added from TXMLNode]
    procedure SetOwnerDocument(const Value: TALXMLDocument); // [added from TXMLNode]
    procedure SetParentNode(const Value: TALXMLNode); virtual;
    procedure SetChildValue(const IndexOrName: OleVariant; const Value: OleVariant);
    procedure SetNodeValue(const Value: OleVariant);
    procedure SetText(const Value: TALXMLString);
    procedure SetAttribute(const AttrName: TALXMLString; const Value: OleVariant);
    procedure SetAttributeAsString(const AttrName: TALXMLString; const Value: TALXMLString); // [added from TXMLNode]
    property InternalChildValue: TALXMLString read GetInternalChildValue write SetInternalChildValue; // [added from TXMLNode]
    property InternalValue: TALXMLString read GetInternalValue write SetInternalValue; // [added from TXMLNode]
  public
    //[Deleted from TXMLNode] constructor CreateHosted(HostNode: TALXMLNode);
    //[Deleted from TXMLNode] destructor Destroy; override;
    //[Deleted from TXMLNode] function AddChild(const TagName, NamespaceURI: TALXMLString; GenPrefix: Boolean = False; Index: Integer = -1): TALXMLNode; overload;
    //[Deleted from TXMLNode] function AddChild(const TagName, NamespaceURI: TALXMLString; NodeClass: TALXMLNodeClass; Index: Integer = -1): TALXMLNode; overload;
    //[Deleted from TXMLNode] function CloneNode(Deep: Boolean): TALXMLNode;
    //[Deleted from TXMLNode] function FindNamespaceDecl(const NamespaceURI: TALXMLString): TALXMLNode;
    //[Deleted from TXMLNode] function FindNamespaceURI(const TagOrPrefix: TALXMLString): TALXMLString;
    //[Deleted from TXMLNode] function GetAttributeNS(const AttrName, NamespaceURI: TALXMLString): OleVariant;
    //[Deleted from TXMLNode] function HasAttribute(const Name, NamespaceURI: TALXMLString): Boolean; overload;
    //[Deleted from TXMLNode] procedure DeclareNamespace(const Prefix, URI: TALXMLString);
    //[Deleted from TXMLNode] procedure Normalize;
    //[Deleted from TXMLNode] procedure Resync;
    //[Deleted from TXMLNode] procedure SetAttributeNS(const AttrName, NamespaceURI: TALXMLString; const Value: OleVariant);
    //[Deleted from TXMLNode] procedure TransformNode(const stylesheet: TALXMLNode; const output: TALXMLDocument); overload;
    //[Deleted from TXMLNode] procedure TransformNode(const stylesheet: TALXMLNode; var output: TALXMLString); overload;
    //[Deleted from TXMLNode] property Collection: TALXMLNodeCollection read FCollection write FCollection;
    //[Deleted from TXMLNode] property DOMNode: IDOMNode read GetDOMNode;
    //[Deleted from TXMLNode] property NamespaceURI: TALXMLString read GetNameSpaceURI;
    //[Deleted from TXMLNode] property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    constructor Create(const NameOrData: TALXmlString; const AddlData: TALXmlString = ''); virtual; //[Replace from TXMLNode] constructor Create(const ADOMNode: IDOMNode; const AParentNode: TALXMLNode; const OwnerDoc: TXMLDocument);
    function AddChild(const TagName: TALXMLString; Index: Integer = -1): TALXMLNode; overload;
    function HasAttribute(const Name: TALXMLString): Boolean; overload;
    function NextSibling: TALXMLNode;
    function PreviousSibling: TALXMLNode;
    property AttributeNodes: TALXMLNodeList read GetAttributeNodes;
    property Attributes[const AttrName: TALXMLString]: OleVariant read GetAttribute write SetAttribute;
    property AttributesAsString[const AttrName: TALXMLString]: String read GetAttributeAsString write SetAttributeAsString; // [added from TXMLNode]
    property ChildNodes: TALXMLNodeList read GetChildNodes write SetChildNodes;
    property ChildValues[const IndexOrName: OleVariant]: OleVariant read GetChildValue write SetChildValue; default;
    property HasChildNodes: Boolean read GetHasChildNodes;
    property IsTextElement: Boolean read GetIsTextElement;
    property LocalName: TALXMLString read GetLocalName;
    property NodeName: TALXMLString read GetNodeName;
    property NodeType: TALXMLNodeType read GetNodeType;
    property NodeValue: OleVariant read GetNodeValue write SetNodeValue;
    property OwnerDocument: TALXMLDocument read GetOwnerDocument Write SetOwnerDocument;
    property ParentNode: TALXMLNode read GetParentNode;
    property Prefix: TALXMLString read GetPrefix;
    property Text: TALXMLString read GetText write SetText;
    property XML: TALXMLString read GetXML;
    procedure SaveToStream(const Stream: TStream; Const SaveOnlyChildNode: Boolean=False); // [added from TXMLNode]
    procedure SaveToFile(const AFileName: TALXMLString; Const SaveOnlyChildNode: Boolean=False); // [added from TXMLNode]
    procedure LoadFromFile(const AFileName: TALXMLString; Const FileContainOnlyChildNode: Boolean=False); // [added from TXMLNode]
    procedure LoadFromStream(const Stream: TStream; Const StreamContainOnlyChildNode: Boolean=False); // [added from TXMLNode]
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
    FParentNode: TALXMLNode;
    FInternalValue: TALXMLString;
  protected
    procedure DoBeforeChildNodesInsert(Index: Integer; const Node: TALXMLNode); override; // [added from TXMLNode]
    function GetNodeType: TALXMLNodeType; override;
    function GetAttributeNodes: TALXMLNodeList; override;
    function GetChildNodes: TALXMLNodeList; override;
    function GetParentNode: TALXMLNode; override;
    function GetInternalValue: TALXMLString; override;
    procedure SetAttributeNodes(const Value: TALXMLNodeList); override;
    procedure SetChildNodes(const Value: TALXMLNodeList); override;
    procedure SetParentNode(const Value: TALXMLNode); override;
    procedure SetInternalValue(const Value: TALXMLString); override;
    function InternalGetAttributeNodes: TALXMLNodeList; override;
    function InternalGetChildNodes: TALXMLNodeList; override;
  public
    constructor Create(const NameOrData: TALXmlString); reintroduce;
    Destructor Destroy; override;
  end;

  {The node represents an attribute of an element. It is not the child of another node, but
   its value can be accessed using the Attribute property of the element node. An attribute
   node can have child nodes of type ntText and ntEntityRef.}
  TALXmlAttributeNode = Class(TALXMLNode)
  private
    FChildNodes: TALXMLNodeList;
    FInternalValue: TALXMLString;
  protected
    procedure DoBeforeChildNodesInsert(Index: Integer; const Node: TALXMLNode); override; // [added from TXMLNode]
    function GetNodeType: TALXMLNodeType; override;
    function GetChildNodes: TALXMLNodeList; override;
    function GetInternalValue: TALXMLString; override;
    procedure SetChildNodes(const Value: TALXMLNodeList); override;
    procedure SetInternalValue(const Value: TALXMLString); override;
    function InternalGetChildNodes: TALXMLNodeList; override;
  public
    constructor Create(const NameOrData: TALXmlString); reintroduce;
    Destructor Destroy; override;
  end;

  {The node represents the text content of a tag. A text node cannot have any child nodes,
   but can appear as the child node of a node of type ntAttribute, ntDocFragment, ntElement,
   or ntEntityRef.}
  TALXmlTextNode = Class(TALXMLNode)
  private
    FParentNode: TALXMLNode;
    FInternalValue: TALXMLString;
  protected
    function GetNodeType: TALXMLNodeType; override;
    function GetParentNode: TALXMLNode; override;
    function GetInternalValue: TALXMLString; override;
    procedure SetParentNode(const Value: TALXMLNode); override;
    procedure SetInternalValue(const Value: TALXMLString); override;
  public
    constructor Create(const NameOrData: TALXmlString); reintroduce;
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
    FParentNode: TALXMLNode;
    FInternalValue: TALXMLString;
  protected
    function GetNodeType: TALXMLNodeType; override;
    function GetParentNode: TALXMLNode; override;
    function GetInternalValue: TALXMLString; override;
    procedure SetParentNode(const Value: TALXMLNode); override;
    procedure SetInternalValue(const Value: TALXMLString); override;
  public
    constructor Create(const NameOrData: TALXmlString); reintroduce;
    Destructor Destroy; override;
  end;

  {The node represents a processing instruction (PI) from the XML document. A PI node cannot
   have any child nodes, but can appear as the child node of a node of type ntDocument,
   ntDocFragment, ntElement, or ntEntityRef.}
  TALXmlProcessingInstrNode = Class(TALXMLNode)
  private
    FParentNode: TALXMLNode;
    FInternalValue: TALXMLString;
    FInternalChildValue: TALXMLString;
  protected
    function GetNodeType: TALXMLNodeType; override;
    function GetParentNode: TALXMLNode; override;
    Function GetInternalChildValue: TALXMLString; override;
    function GetInternalValue: TALXMLString; override;
    procedure SetParentNode(const Value: TALXMLNode); override;
    procedure SetInternalChildValue(const Value: TALXMLString); override;
    procedure SetInternalValue(const Value: TALXMLString); override;
  public
    constructor Create(const NameOrData: TALXmlString; const AddlData: TALXmlString = ''); override;
    Destructor Destroy; override;
  end;


  //[not implemented yet !]
  {The node represents a CDATA section in the XML source. CDATA sections identify blocks of
   text that would otherwise be interpreted as markup. An ntCData node can’t have any child
   nodes. They can appear as the child of an ntDocFragment, ntEntityRef, or ntElement node.}
  TALXmlCDataNode = Class(TALXMLNode)
  private
    FParentNode: TALXMLNode;
    FInternalValue: TALXMLString;
  protected
    function GetNodeType: TALXMLNodeType; override;
    function GetParentNode: TALXMLNode; override;
    function GetInternalValue: TALXMLString; override;
    procedure SetParentNode(const Value: TALXMLNode); override;
    procedure SetInternalValue(const Value: TALXMLString); override;
  public
    constructor Create(const NameOrData: TALXmlString); reintroduce;
    Destructor Destroy; override;
  end;

  //[not implemented yet !]
  {The node represents a reference to an entity in the XML document. This can be any type of
   entity, including character entity references. The children of an entity reference node can
   be of the following types: ntElement, ntProcessingInstr, ntComment, ntText, ntCData, and
   ntEntityRef. The entity reference node can appear as the child of an ntAttribute, ntDocFragment,
   ntElement, or ntEntityRef node.}
  TALXmlEntityRefNode = Class(TALXMLNode)
  private
  protected
  public
  end;

  //[not implemented yet !]
  {The node represents an expanded entity. Entity nodes can have child nodes that represent the
   expanded entity (for example, ntText and ntEntityRef nodes). Entity nodes only appear as the
   child of an ntDocType node.}
  TALXmlEntityNode = Class(TALXMLNode)
  private
  protected
  public
  end;

  //[not implemented yet !]
  {The node represents the document type declaration, indicated by the <!DOCTYPE > tag. The document
   type node can child nodes of type ntNotation and ntEntity. It always appears as the child of the
   document node.}
  TALXmlDocTypeNode = Class(TALXMLNode)
  private
  protected
  public
  end;

  //[not implemented yet !]
  {The node represents a document fragment. A document fragment node associates a node or subtree
   with a document without actually being contained in the document. Document fragment nodes can
   have child nodes of type ntElement, ntProcessingInstr, ntComment, ntText, ntCData, and ntEntityRef.
   It never appears as the child of another node.}
  TALXmlDocFragmentNode = Class(TALXMLNode)
  private
  protected
  public
  end;

  //[not implemented yet !]
  {A node represents a notation in the document type declaration. It always appears as the child of an
   ntDocType node and never has any child nodes.}
  TALXmlNotationNode = Class(TALXMLNode)
  private
  protected
  public
  end;


  {TALXMLDocument}
  TALXMLDocument = class(Tcomponent)
  private
    //[Deleted from TXMLDocument] FXMLData: TALXMLString;
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
    //[Deleted from TXMLDocument] FNSPrefixBase: TALXmlString;
    //[Deleted from TXMLDocument] FOnAsyncLoad: TAsyncEventHandler;
    //[Deleted from TXMLDocument] FOwnerIsComponent: Boolean;
    //[Deleted from TXMLDocument] FPrefixID: Integer;
    //[Deleted from TXMLDocument] FXMLStrRead: Integer;
    //[Deleted from TXMLDocument] function GetDOMParseOptions: IDOMParseOptions;
    //[Deleted from TXMLDocument] procedure SetDOMImplementation(const Value: IDOMImplementation);
    //[Deleted from TXMLDocument] procedure SetDOMVendor(const Value: TDOMVendor);
    FSrcStream: TStream;
    FXMLStrings: TStringList;
    FAfterClose: TNotifyEvent;
    FAfterOpen: TNotifyEvent;
    FBeforeClose: TNotifyEvent;
    FBeforeOpen: TNotifyEvent;
    FDocSource: TalXMLDocumentSource;
    FDocumentNode: TALXMLNode;
    FFileName: TALXmlString;
    FNodeIndentStr: TALXmlString;
    FOptions: TALXMLDocOptions;
    FParseOptions: TALXMLParseOptions;
    FStreamedActive: Boolean;
    FOnParseProcessingInstruction: TAlXMLParseProcessingInstructionEvent; // [added from TXMLDocument]
    FOnParseStartDocument: TNotifyEvent; // [added from TXMLDocument]
    FOnParseEndDocument: TNotifyEvent; // [added from TXMLDocument]
    FOnParseStartElement: TAlXMLParseStartElementEvent; // [added from TXMLDocument]
    FOnParseEndElement: TAlXMLParseEndElementEVent; // [added from TXMLDocument]
    FonParseText: TAlXMLParseTextEvent; // [added from TXMLDocument]
    FonParseComment: TAlXMLParseTextEvent; // [added from TXMLDocument]
    FonParseCData: TAlXMLParseTextEvent; // [added from TXMLDocument]
    function IsXMLStored: Boolean;
    function NodeIndentStored: Boolean;
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
    //[Deleted from TXMLDocument] function GetSchemaRef: TALXMLString;
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
    function GetPrologNode: TALXMLNode;
    function GetPrologValue(PrologItem: TALXMLPrologItem; const Default: TALXMLString = ''): TALXMLString;
    function InternalSetPrologValue(const PrologNode: TALXMLNode; const Value: TALXMLString; PrologItem: TALXMLPrologItem): TALXmlstring; //[Replace from TXMLNodeList] function InternalSetPrologValue(const PrologNode: IXMLNode; const Value: Variant; PrologItem: TXMLPrologItem): string;
    procedure CheckActive;
    procedure CheckAutoSave;
    procedure DoAfterClose;
    procedure DoAfterOpen;
    procedure DoBeforeClose;
    procedure DoBeforeOpen;
    procedure DoParseProcessingInstruction(const Target, Data: TALXMLString); // [added from TXMLDocument]
    procedure DoParseStartDocument; // [added from TXMLDocument]
    procedure DoParseEndDocument; // [added from TXMLDocument]
    procedure DoParseStartElement(const Name: TALXMLString; const Attributes: Tstrings); // [added from TXMLDocument]
    procedure DoParseEndElement(const Name: TALXMLString); // [added from TXMLDocument]
    procedure DoParseText(const str: TALXMLString); // [added from TXMLDocument]
    procedure DoParseComment(const str: TALXMLString); // [added from TXMLDocument]
    procedure DoParseCData(const str: TALXMLString); // [added from TXMLDocument]
    procedure LoadData;
    procedure Loaded; override;
    Procedure InternalParseXml(Const RawXmlStream: TStream; Const FirstNode: TALXmlNode; Const SkipFirstNode: Boolean=False); // [added from TXMLDocument]
    procedure ReleaseDoc(const CheckSave: Boolean = True);
    procedure SaveToXMLStrings;
    procedure SetPrologValue(const Value: TALXMLString; PrologItem: TALXMLPrologItem); //[Replace from TXMLNodeList] procedure SetPrologValue(const Value: Variant; PrologItem: TXMLPrologItem);
    procedure SetXMLStrings(const Value: string);
    function GetActive: Boolean;
    function GetChildNodes: TALXMLNodeList;
    function GetDocumentElement: TALXMLNode;
    function GetDocumentNode: TALXMLNode;
    function GetEncoding: TALXMLString;
    function GetFileName: TALXMLString;
    function GetNodeIndentStr: TALXMLString;
    function GetOptions: TALXMLDocOptions;
    function GetParseOptions: TALXMLParseOptions;
    function GetStandAlone: TALXMLString;
    function GetVersion: TALXMLString;
    function GetXML: TStrings;
    procedure SetActive(const Value: Boolean);
    procedure SetDocumentElement(const Value: TALXMLNode);
    procedure SetOptions(const Value: TALXMLDocOptions);
    procedure SetParseOptions(const Value: TALXMLParseOptions);
    procedure SetStandAlone(const Value: TALXMLString);
    procedure SetVersion(const Value: TALXMLString);
    procedure SetXML(const Value: TStrings);
    procedure SetEncoding(const Value: TALXMLString);
    procedure SetFileName(const Value: TALXMLString);
    procedure SetNodeIndentStr(const Value: TALXMLString);
    procedure XMLStringsChanging(Sender: TObject);
    property DocSource: TALXMLDocumentSource read FDocSource write FDocSource;
  public
    //[Deleted from TXMLDocument] class function NewInstance: TObject; override;
    //[Deleted from TXMLDocument] function AddChild(const TagName, NamespaceURI: TALXMLString): TALXMLNode; overload;
    //[Deleted from TXMLDocument] function GetDocBinding(const TagName: TALXMLString; DocNodeClass: TClass; NamespaceURI: TALXMLString = ''): TALXMLNode;
    //[Deleted from TXMLDocument] procedure LoadFromXML(const XML: WideString); overload;
    //[Deleted from TXMLDocument] procedure RegisterDocBinding(const TagName: TALXMLString; DocNodeClass: TClass; NamespaceURI: TALXMLString = '');
    //[Deleted from TXMLDocument] procedure Resync;
    //[Deleted from TXMLDocument] procedure SaveToXML(var XML: WideString); overload;
    //[Deleted from TXMLDocument] property AsyncLoadState: Integer read GetAsyncLoadState;
    //[Deleted from TXMLDocument] property DOMDocument: IDOMDocument read GetDOMDocument write SetDOMDocument;
    //[Deleted from TXMLDocument] property DOMImplementation: IDOMImplementation read FDOMImplementation write SetDOMImplementation;
    //[Deleted from TXMLDocument] function GeneratePrefix(const Node: TALXMLNode): TALXMLString;
    //[Deleted from TXMLDocument] property Modified: Boolean read GetModified;
    //[Deleted from TXMLDocument] property NSPrefixBase: TALXMLString read FNSPrefixBase write FNSPrefixBase;
    //[Deleted from TXMLDocument] property SchemaRef: TALXMLString read GetSchemaRef;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(const AFileName: TALXMLString); reintroduce; overload;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function AddChild(const TagName: TALXMLString): TALXMLNode; overload;
    function CreateElement(const TagOrData: TALXMLString): TALXMLNode;  //[Replace from TXMLDocument] function CreateElement(const TagOrData, NamespaceURI: TALXMLString): TALXMLNode;
    function CreateNode(const NameOrData: TALXMLString; NodeType: TALXMLNodeType = ntElement; const AddlData: TALXMLString = ''): TALXMLNode;
    function IsEmptyDoc: Boolean;
    procedure LoadFromFile(const AFileName: TALXMLString = '');
    procedure LoadFromStream(const Stream: TStream); //[Replace from TXMLNodeList] procedure LoadFromStream(const Stream: TStream; EncodingType: TALXMLEncodingType = xetUnknown);
    procedure LoadFromXML(const XML: string); overload;
    procedure Refresh;
    procedure SaveToFile(const AFileName: TALXMLString = '');
    procedure SaveToStream(const Stream: TStream);
    procedure SaveToXML(var XML: TalXmlString);
    procedure parseXML;
    property ChildNodes: TALXMLNodeList read GetChildNodes;
    property DocumentElement: TALXMLNode read GetDocumentElement write SetDocumentElement;
    property Encoding: TALXMLString read GetEncoding write SetEncoding;
    property Node: TALXMLNode read GetDocumentNode;
    property StandAlone: TALXMLString read GetStandAlone write SetStandAlone;
    property Version: TALXMLString read GetVersion write SetVersion;
  published
    //[Deleted from TXMLDocument] property DOMVendor: TDOMVendor read FDOMVendor write SetDOMVendor;
    //[Deleted from TXMLDocument] property BeforeNodeChange: TNodeChangeEvent read FBeforeNodeChange write FBeforeNodeChange;
    //[Deleted from TXMLDocument] property AfterNodeChange: TNodeChangeEvent read FAfterNodeChange write FAfterNodeChange;
    //[Deleted from TXMLDocument] property OnAsyncLoad: TAsyncEventHandler read FOnAsyncLoad write SetOnAsyncLoad;
    property Active: Boolean read GetActive write SetActive default False;
    property FileName: TALXMLString read GetFileName write SetFileName;
    property NodeIndentStr: TALXMLString read GetNodeIndentStr write SetNodeIndentStr stored NodeIndentStored;
    property Options: TALXMLDocOptions read GetOptions write SetOptions default [doAttrNull];
    property ParseOptions: TALXMLParseOptions read GetParseOptions write SetParseOptions default [];
    property XML: TStrings read GetXML write SetXML stored IsXMLStored;
    property BeforeOpen: TNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterOpen: TNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforeClose: TNotifyEvent read FBeforeClose write FBeforeClose;
    property AfterClose: TNotifyEvent read FAfterClose write FAfterClose;
    property OnParseProcessingInstruction: TAlXMLParseProcessingInstructionEvent read FOnParseProcessingInstruction write FOnParseProcessingInstruction; // [added from TXMLDocument]
    property OnParseStartDocument: TNotifyEvent read FOnParseStartDocument write FOnParseStartDocument; // [added from TXMLDocument]
    property OnParseEndDocument: TNotifyEvent read FOnParseEndDocument write FOnParseEndDocument; // [added from TXMLDocument]
    property OnParseStartElement: TAlXMLParseStartElementEvent read FOnParseStartElement write FOnParseStartElement; // [added from TXMLDocument]
    property OnParseEndElement: TAlXMLParseEndElementEVent read FOnParseEndElement Write FOnParseEndElement; // [added from TXMLDocument]
    property OnParseText: TAlXMLParseTextEvent read FonParseText Write FonParseText; // [added from TXMLDocument]
    property OnParseComment: TAlXMLParseTextEvent read FonParseComment Write FonParseComment; // [added from TXMLDocument]
    property OnParseCData: TAlXMLParseTextEvent read FonParseCData Write FonParseCData; // [added from TXMLDocument]
  end;

{misc constante}
Const cAlXMLUTF8EncodingStr = 'UTF-8';
      cALXmlUTF8HeaderStr = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'#13#10;

{misc function}
Function  ALCreateEmptyXMLDocument(Rootname:string):TalXMLDocument;
procedure ALClearXMLDocument(rootname:string; xmldoc: TalXMLDocument; const EncodingStr: String = cAlXMLUTF8EncodingStr);
Function  ALFindXmlNodeByAttribute(xmlrec:TalxmlNode;
                                   AttributeName, AttributeValue : string;
                                   Const SearchAlsoInChildNodes: Boolean = False): TalxmlNode;

procedure Register;

implementation

uses Variants,
     Contnrs,
     AlFcnHTML,
     AlFcnString;

const
  CALNSDelim               = ':';
  CALXML                   = 'xml';
  CALVersion               = 'version';
  CALEncoding              = 'encoding';
  CALStandalone            = 'standalone';
  CALDefaultNodeIndent     = '  '; { 2 spaces }
  CALXmlDocument           = 'DOCUMENT';

{$R ..\resource\ALXmlDoc.dcr}

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALXMLDocument]);
end;




/////////////////////////////////////
//////////Utility Functions//////////
/////////////////////////////////////

{**********************************}
{Raises an EALXMLDocError exception.
 Call ALXMLDocError to raise an EALXMLDocError exception. Using ALXMLDocError rather than explicitly creating
 the EALXMLDocError instance can result in more space-efficient code.
 *Msg is the error message associated with the EALXMLDocError instance.}
procedure ALXMLDocError(const Msg: string); overload;
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
procedure ALXMLDocError(const Msg: string; const Args: array of const); overload;
begin
  raise EALXMLDocError.CreateFmt(Msg, Args);
end;

{****************************************************************************}
{Indicates whether a specified node matches a given tag name. Call NodeMatches
 to determine whether the node with the object specified by Node refers to the same node as is described by the TagName
 NodeMatches returns true if The value of the local name or tag name of Node is the same as the TagName parameter.}
function ALNodeMatches(const Node: TALXMlNode; const TagName: TALXMLString): Boolean;
begin
  Result := (Node.NodeName = TagName) or (Node.LocalName = TagName);
end;

{******************************************************}
{Returns the namespace prefix of an XML node’s tag name.
 Call ExtractPrefix to determine the namespace prefix, if any, of a full tag name for an XML node.
 *AName is the tag name of the XML node.
 ExtractPrefix returns the namespace prefix, not including the colon (:) that separates the namespace from the local name of
 the XML node. If the tag name does not include a namespace prefix, ExtractPrefix returns an empty string.}
function ALExtractPrefix(const AName: TALXMLString): TALXMLString;
var SepPos: Integer;
begin
  SepPos := ALCharPos(CALNSDelim,Aname);
  if SepPos > 0 then begin
    setlength(Result,SepPos - 1);
    ALMove(aName[1], Result[1], SepPos - 1);
  end
  else result := '';
end;

{*************************************************************************}
{Strips the namespace prefix, if present, from the tag name of an XML node.
 Call ExtractLocalName to convert a full tag name for an XML node into the corresponding local name.
 *AName is the tag name to be converted.
 ExtractLocalName returns the value of AName with any namespace prefix stripped away.}
function ALExtractLocalName(const AName: TALXMLString): TALXMLString;
var SepPos: Integer;
    aLength: integer;
begin
  SepPos := ALCharPos(CALNSDelim,Aname);
  if SepPos > 0 then begin
    aLength := Length(Aname) - seppos;
    setlength(Result,aLength);
    ALMove(aName[SepPos + 1], Result[1], aLength); // faster than system.move
  end
  else begin
    aLength := Length(Aname);
    setlength(Result,aLength);
    ALMove(aName[1], Result[1], aLength); // faster than system.move
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
function ALCreateXmlNode(const NameOrData: TALXmlString;
                         NodeType: TALXMLNodeType = ntElement;
                         const AddlData: TALXmlString = ''): TALXMLNode;
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

{******************************************************************************************************************}
function ALExtractAttrValue(const AttrName, AttrLine: TALXmlstring; const Default: TALXMLString = ''): TALXMLString;
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

{*****************************************************************************************}
procedure ALAppendItem(var AttrStr: TALXmlstring; const AttrName, AttrValue: TALXmlstring);
begin
  if AttrValue <> '' then begin
    if AttrStr <> '' then AttrStr := AttrStr + ' ' + AttrName + '="' + AttrValue + '"'
    else AttrStr := AttrStr + AttrName + '="' + AttrValue + '"';
  end;
end;



//////////////////////////////////
//////////TALXMLDocument//////////
//////////////////////////////////

{************************************}
{Instantiates a TALXMLDocument object.
 Call Create to instantiate a TALXMLDocument component at runtime.}
constructor TALXMLDocument.create(AOwner: TComponent);
begin
  { Need to have this to make this constructor visible to C++ classes }
  inherited;
end;

{************************************}
{Instantiates a TALXMLDocument object.
 Call Create to instantiate a TALXMLDocument component at runtime.
 *AFileName specifies the file that the new TALXMLDocument instance represents.}
constructor TALXMLDocument.Create(const AFileName: TALXMLString);
begin
  inherited Create(nil);
  FFileName := AFileName;
end;

{*****************************************}
procedure TALXMLDocument.AfterConstruction;
begin
  inherited;

  FAfterClose:= nil;
  FAfterOpen:= nil;
  FBeforeClose:= nil;
  FBeforeOpen:= nil;
  FStreamedActive := False;
  FDocSource:= xdsNone;
  FDocumentNode:= nil;
  FFileName:= '';
  FParseOptions:= [];
  FSrcStream := nil;
  FOnParseProcessingInstruction:= nil;
  FOnParseStartDocument:= nil;
  FOnParseEndDocument:= nil;
  FOnParseStartElement:= nil;
  FOnParseEndElement:= nil;
  FonParseText:= nil;
  FonParseComment:= nil;
  FonParseCData:= nil;

  FOptions := [doAttrNull];
  NodeIndentStr := CALDefaultNodeIndent;
  FXMLStrings := TStringList.Create;
  FXMLStrings.OnChanging := XMLStringsChanging;
  if FFileName <> '' then SetActive(True);
end;

{************************************}
{Disposes of a TALXMLDocument object.}
destructor TALXMLDocument.Destroy;
begin
  Destroying;
  SetActive(False);
  FreeAndNil(FXMLStrings);
  inherited;
end;

{******************************}
procedure TALXMLDocument.Loaded;
begin
  inherited;
  try
    if FStreamedActive then SetActive(True);
  except
    if csDesigning in ComponentState then
      if Assigned(Classes.ApplicationHandleException) then
        Classes.ApplicationHandleException(ExceptObject)
      else
        ShowException(ExceptObject, ExceptAddr)
    else
      raise;
  end;
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
  if (csReading in ComponentState) then FStreamedActive := Value
  else if Value <> GetActive then begin
    if Value then begin
      DoBeforeOpen;
      FDocumentNode := TALXmlDocumentNode.Create(self);
      try
        LoadData;
      except
        ReleaseDoc(False);
        raise;
      end;
      DoAfterOpen;
    end
    else begin
      DoBeforeClose;
      ReleaseDoc;
      DoAfterClose;
    end;
  end;
end;

{********************************************************************}
Procedure TALXMLDocument.InternalParseXml(Const RawXmlStream: TStream;
                                          Const FirstNode: TALXmlNode;
                                          Const SkipFirstNode: Boolean=False);
Const BufferSize: integer = 8192;
Var RawXmlString: TALXMLString;
    RawXmlStringLength: Integer;
    RawXmlStringPos: Integer;
    PreserveWhiteSpace: Boolean;
    LstParams: TstringList;
    NodeStack: TStack;
    NotSaxMode: Boolean;
    CurrentNode: TALXmlNode;
    FirstTagElement: Boolean;
    DecodeXmlReferences: Boolean;
    EncodingType: TALXMLEncodingType;

  {-----------------------------------}
  function ExpandRawXmlString: boolean;
  Var ByteReaded, Byte2Read: Integer;
  Begin
    If RawXmlStringPos > 1 then begin
      Byte2Read := RawXmlStringPos - 1;
      ALMove(RawXmlString[RawXmlStringPos],RawXmlString[1],RawXmlStringLength - RawXmlStringPos + 1);
      RawXmlStringPos := 1;
    end
    else begin
      Byte2Read := BufferSize;
      RawXmlStringLength := RawXmlStringLength + BufferSize;
      SetLength(RawXmlString, RawXmlStringLength);
    end;

    ByteReaded := RawXmlStream.Read(RawXmlString[RawXmlStringLength - Byte2Read + 1],Byte2Read);

    If ByteReaded <> Byte2Read then begin
      RawXmlStringLength := RawXmlStringLength - Byte2Read + ByteReaded;
      SetLength(RawXmlString, RawXmlStringLength);
      Result := ByteReaded > 0;
    end
    else result := True;
  end;

  {--------------------------------------------------------------------}
  Function PosInXmlString(Substr: String; Offset: integer = 1): integer;
  Begin
    Result := ALPosEx(Substr,RawXmlString,OffSet);
    While (Result <= 0) do begin
      Offset := RawXmlStringlength - RawXmlStringPos + 3 - length(Substr);
      If not ExpandRawXmlString then break;
      Result := ALPosEx(Substr,RawXmlString,offset);
    end;
  end;

  {----------------------------------------------------------------------}
  Function CharPosInXmlString(Substr: Char; Offset: integer = 1): integer;
  Begin
    Result := ALCharPosEx(Substr,RawXmlString,OffSet);
    While (Result <= 0) do begin
      Offset := RawXmlStringlength - RawXmlStringPos + 2;
      If not ExpandRawXmlString then break;
      Result := ALCharPosEx(Substr,RawXmlString,offset);
    end;
  end;

  {-----------------------------------------------------------------------------------------------}
  Function CharPosInString(const Substr: array of char; str: String; Offset: integer = 1): integer;
  Var i: integer;
      LowsubStr, highSubStr: integer;
      lnStr: integer;
      c: Char;
  Begin
    Result := 0;
    LowsubStr := low(SubStr);
    highSubStr := high(substr);
    lnStr := length(Str);
    while offset <= lnStr do begin
      c := str[offset];
      for i := LowsubStr to highSubStr do
        if c = Substr[i] then begin
          result := Offset;
          exit;
        end;

      inc(offset);
    end;
  end;

  {---------------------------------------------}
  Procedure CheckAttributes(TagParams: TStrings);
  Var i: integer;
      S1, S2, S3: String;
      L1: integer;
      P1: integer;
  Begin
    i := 0;
    While i <= TagParams.Count - 3 do begin
      S1 := TagParams[i];
      S2 := TagParams[i+1];
      S3 := TagParams[i+2];
      L1 := length(S1);
      P1 := AlCharPos('=',S1);
      IF (P1 <= 0) and
         (S2 = '=') then begin {aname = "obie2.html"}
        TagParams[i] := S1 + S2 + S3;
        tagParams.Delete(i+2);
        tagParams.Delete(i+1);
      end
      else if (L1 > 0) and
              (P1 = L1) then begin {aname= "obie2.html"}
        TagParams[i] := S1 + S2;
        tagParams.Delete(i+1);
      end
      else if (L1 > 0) and
              (P1 <= 0) and
              (AlCharPos('=',S2) = 1)  then begin {aname ="obie2.html"}
        TagParams[i] := S1 + S2;
        tagParams.Delete(i+1);
      end;
      inc(i);
    end;
  end;

  {------------------------------------}
  {Analyze Processing Instructions (PI)}
  procedure AnalyzePI;
  Var P1, P2: Integer;
      aName, aContent: String;
  Begin
    { <?name?>...
      <?name content?>... }
    If FirstTagElement then ALXmlDocError(CALXmlParseError);
    P2 := PosInXmlString('?>', RawXmlStringPos + 2);
    If P2 > 0 then begin
      P1 := CharPosInString([' ',#9,#13,#10],RawXmlString, RawXmlStringPos+2);
      If (P1 <= 0) or (P1 > P2) then P1 := P2;
      aName := ALCopyStr(RawXmlString,RawXmlStringPos + 2,P1-RawXmlStringPos - 2);
      aContent := ALCopyStr(RawXmlString,P1+1,P2-P1-1);
      If NotSaxMode then
        CurrentNode.ChildNodes.Add(
                                   CreateNode(
                                              aName,
                                              ntProcessingInstr,
                                              aContent
                                             )
                                  );
      DoParseProcessingInstruction(aName, aContent);

      {calculate the encoding}
      if RawXmlStringPos = 1 then begin
        if (ALExtractAttrValue(CALEncoding, aContent, '') = 'UTF-8') then EncodingType := xetUTF_8
        else EncodingType := xetUnknown;
      end;

      RawXmlStringPos := P2 + 2;
    end
    else ALXmlDocError(CALXmlParseError);
  end;

  {---------------}
  {Analyze comment}
  procedure AnalyzeComment;
  Var P1: Integer;
      aContent: String;
  Begin
    { <!-- name -->... }
    If FirstTagElement then ALXmlDocError(CALXmlParseError);
    P1 := PosInXmlString('-->',RawXmlStringPos + 4);
    If P1 > 0 then begin
      aContent := ALCopyStr(RawXmlString,RawXmlStringPos + 4,P1-RawXmlStringPos - 4);
      if NotSaxMode then
        CurrentNode.ChildNodes.Add(
                                   CreateNode(
                                              acontent,
                                              ntcomment,
                                              ''
                                             )
                                  );
      DoParseComment(aContent);
      RawXmlStringPos := P1 + 3;
    end
    else ALXmlDocError(CALXmlParseError);
  end;

  {-------------}
  {Analyze cdata}
  procedure AnalyzeCDATA;
  Var P1: Integer;
      aContent: String;
  Begin
    { <!-- name -->... }
    If FirstTagElement then ALXmlDocError(CALXmlParseError);
    P1 := PosInXmlString(']]>',RawXmlStringPos + 9);
    If P1 > 0 then begin
      aContent := ALCopyStr(RawXmlString,RawXmlStringPos + 9,P1-RawXmlStringPos - 9);
      if NotSaxMode then
        CurrentNode.ChildNodes.Add(
                                   CreateNode(
                                              acontent,
                                              ntcdata,
                                              ''
                                             )
                                  );
      DoParseCdata(aContent);
      RawXmlStringPos := P1 + 3;
    end
    else ALXmlDocError(CALXmlParseError);
  end;

  {-----------}
  {Analyze tag}
  procedure AnalyzeTag;
  Var P1, P2: Integer;
      aName, aContent: String;
      aContentLn: Integer;
      TagEnclosed : Boolean;
      aNode: TALXmlNode;
      i:Integer;
  Begin
    //<name>...
    //<name attrname="attrvalue">...
    //<name/>...
    //<name attrname="attrvalue"/>...
    //</name>...
    P1 := CharPosInXmlString('>',RawXmlStringPos + 1);
    If P1 > 0 then begin

      {end tag}
      If RawXmlString[RawXmlStringPos + 1] = '/' then begin
        aName := trimRight(ALCopyStr(RawXmlString,RawXmlStringPos + 2,P1-RawXmlStringPos - 2));
        if NotSaxMode then begin
          aNode := TALXmlNode(NodeStack.pop);
          if (aNode = nil) or
             (aNode <> currentNode) or
             (aNode.NodeName <> aName) then ALXmlDocError(CALXmlParseError);
          If CurrentNode<>FirstNode then CurrentNode := currentNode.ParentNode
          Else FirstTagElement := not SkipFirstNode;
        end;
        DoParseEndElement(aName);
        RawXmlStringPos := P1 + 1;
        exit;
      end;

      {tag is enclosed?}
      If (RawXmlString[P1-1] = '/') then Begin
        TagEnclosed := True;
        dec(P1);
      end
      else TagEnclosed := False;

      P2 := CharPosInString([' ',#9,#13,#10],RawXmlString,RawXmlStringPos + 1);
      If (P2 <= 0) or (P2 > P1) then P2 := P1;

      aName := ALCopyStr(RawXmlString,RawXmlStringPos + 1,P2-RawXmlStringPos - 1);
      aContent := ALCopyStr(RawXmlString,P2+1,P1-P2-1);

      if NotSaxMode then begin
        If FirstTagElement then begin
          aNode := FirstNode;
          FirstTagElement := False
        end
        else begin
          aNode := CreateNode(
                              aName,
                              ntelement,
                              ''
                             );
          CurrentNode.ChildNodes.Add(aNode);
        end
      end
      else aNode := nil; {for hide warning}

      LstParams.Clear;
      If (aContent <> '') then begin
        ALExtractHeaderFields(
                              [' ', #9, #13, #10],    //Separators
                              [' ', #9, #13, #10],    //WhiteSpace
                              ['"', ''''],            //Quotes
                              Pchar(aContent),        //Content
                              lstParams,              //Strings
                              False,                  //Decode
                              False                   //StripQuotes
                             );
        CheckAttributes(LstParams);
        For i := 0 to LstParams.Count - 1 do begin
          aContent := LstParams.ValueFromIndex[i];
          aContentLn := length(aContent);
          if (aContentLn < 2) or
             (aContent[1] <> aContent[aContentLN]) or
             (not (aContent[1] in ['''','"'])) then ALXmlDocError(CALXmlParseError)
          else begin
            if DecodeXmlReferences then begin
              if EncodingType = xetUTF_8 then LstParams[i] := LstParams.Names[i] + '=' + ALUTF8XMLTextElementDecode(alCopyStr(aContent,2,aContentLN-2))
              else LstParams[i] := LstParams.Names[i] + '=' + ALANSIXMLTextElementDecode(alCopyStr(aContent,2,aContentLN-2))
            end
            else LstParams[i] := LstParams.Names[i] + '=' + alCopyStr(aContent,2,aContentLN-2)
          end;
        end;

        if NotSaxMode then
          For i := 0 to LstParams.Count - 1 do
            aNode.Attributes[LstParams.Names[i]] := LstParams.ValueFromIndex[i];
      end;

      DoParseStartElement(aName,LstParams);

      if not TagEnclosed then begin
        if NotSaxMode then begin
          CurrentNode := aNode;
          NodeStack.Push(CurrentNode);
        end;
        RawXmlStringPos := P1+1;
      end
      else begin
        DoParseEndElement(aName);
        RawXmlStringPos := P1+2;
        If NotSaxMode and (aNode = FirstNode) and (not SkipFirstNode) then FirstTagElement := true;
      end;

    end
    else ALXmlDocError(CALXmlParseError);
  end;

  {------------}
  {Analyze text}
  Procedure AnalyzeText;
  Var P1: Integer;
      Str1: String;
  Begin
    P1 := CharPosInXmlString('<',RawXmlStringPos);
    If (P1<=0) then P1 := RawXmlStringLength + 1;

    Str1 := ALCopyStr(RawXmlString,RawXmlStringPos,P1-RawXmlStringPos);
    If (PreserveWhiteSpace) or (Trim(Str1) <> '') then Begin
      If FirstTagElement then ALXmlDocError(CALXmlParseError);

      if DecodeXmlReferences then begin
        if EncodingType = xetUTF_8 then str1 := ALUTF8XMLTextElementDecode(Str1)
        else str1 := ALANSIXMLTextElementDecode(Str1);
      end;

      if (notSaxMode) and
         (currentNode.NodeType <> ntdocument) then
        CurrentNode.ChildNodes.Add(
                                   CreateNode(
                                              Str1,
                                              ntText,
                                              ''
                                             )
                                  );
      DoParseText(Str1);
    end;

    RawXmlStringPos := P1;
  end;

Begin
  LstParams := TstringList.Create;
  NodeStack := Tstack.create;
  Try

    DoParseStartDocument;
    RawXmlString := '';
    RawXmlStringLength := 0;
    RawXmlStringPos := 1;
    if not ExpandRawXmlString then exit;
    PreserveWhiteSpace := (poPreserveWhiteSpace in ParseOptions) and (not (doNodeAutoIndent in Options));
    CurrentNode := FirstNode;
    NotSaxMode := assigned(FirstNode);
    FirstTagElement := NotSaxMode and (FirstNode.NodeType <> ntdocument) and (not SkipFirstNode);
    DecodeXmlReferences := not (poIgnoreXmlReferences in ParseOptions);
    EncodingType := xetUnknown;

    While RawXmlStringPos <= RawXmlStringLength do begin

      If RawXmlString[RawXmlStringPos] = '<' then begin
        If (RawXmlStringPos < RawXmlStringLength) and (RawXmlString[RawXmlStringPos + 1] = '?') then AnalyzePI
        else if (RawXmlStringPos + 2 < RawXmlStringLength) and
                (RawXmlString[RawXmlStringPos + 1] = '!') and
                (RawXmlString[RawXmlStringPos + 2] = '-') and
                (RawXmlString[RawXmlStringPos + 3] = '-') then AnalyzeComment
        else if (RawXmlStringPos + 7 < RawXmlStringLength) and
                (RawXmlString[RawXmlStringPos + 1] = '!') and
                (RawXmlString[RawXmlStringPos + 2] = '[') and
                (RawXmlString[RawXmlStringPos + 3] = 'C') and
                (RawXmlString[RawXmlStringPos + 4] = 'D') and
                (RawXmlString[RawXmlStringPos + 5] = 'A') and
                (RawXmlString[RawXmlStringPos + 6] = 'T') and
                (RawXmlString[RawXmlStringPos + 7] = 'A') and
                (RawXmlString[RawXmlStringPos + 8] = '[') then AnalyzeCData
        else AnalyzeTag
      end
      else AnalyzeText;

      if RawXmlStringPos + 7 >= RawXmlStringLength then ExpandRawXmlString;

    end;

    DoParseEndDocument;

  finally
    LstParams.Free;
    NodeStack.Free;
  end;
end;

{********************************}
procedure TALXMLDocument.ParseXml;
begin
  If active then alXmldocError(cALXmlActive);
  LoadData;
end;


{********************************}
procedure TALXMLDocument.LoadData;
var StringStream: TStringStream;
    FileStream: TfileStream;
begin

  Try

    DocSource := xdsNone;
    if (FXMLStrings.Count > 0) then begin
      StringStream := TStringStream.Create(FXMLStrings.Text);
      try
        InternalParseXml(StringStream, FDocumentNode);
      finally
        StringStream.Free;
      end;
      DocSource := xdsXMLProperty;
    end
    else if Assigned(FSrcStream) then begin
      FSrcStream.Seek(0, 0);
      InternalParseXml(FSrcStream, FDocumentNode);
      DocSource := xdsStream;
      FSrcStream := nil;
    end
    else if FFileName <> '' then begin
      FileStream := TfileStream.Create(FFilename,fmOpenRead or fmShareDenyWrite);
      Try
        InternalParseXml(FileStream, FDocumentNode);
      finally
        FileStream.Free;
      end;
      DocSource := xdsFile;
    end;

  except
    DocSource := xdsNone;
    raise;
  end;

end;

{*******************************************************************}
procedure TALXMLDocument.ReleaseDoc(const CheckSave: Boolean = True);
begin
  if CheckSave then CheckAutoSave;
  if assigned(FDocumentNode) then FreeAndNil(FDocumentNode);
  if not (DocSource in [xdsNone, xdsXMLProperty]) then SetXMLStrings('');
end;

{***********************************************************}
{Updates the parsed XML document to reflect external changes.
 Call Refresh when the XML document specified by FileName has changed due to an external cause. Refresh updates the parsed version of the XML
 document (which can be traversed starting with DocumentElement) to reflect the current contents of the file specified by FileName.
 The Active property must be true when you call Refresh. If the XML document is not active when you call Refresh, TALXMLDocument raises
 an EALXMLDocError exception.
 Note:	Do not call Refresh after making changes directly to the XML property, because assigning a value to XML automatically closes the XML document.
 To refresh the XML document after assigning a new value to XML, set the Active property to true.}
procedure TALXMLDocument.Refresh;
begin
  CheckActive;
  ReleaseDoc(False); //=> at the same time set active to False !!
  if not (DocSource in [xdsXMLProperty, xdsFile]) then ALXMLDocError(CALXmlNoRefresh);

  FDocumentNode := TALXmlDocumentNode.Create(self);
  try
    LoadData;
  except
    ReleaseDoc(False);
    raise;
  end;
end;

{**************************************}
{Loads an XML document and activates it.
 Call LoadFromFile to load the XML document specified by AFileName and set the Active property to true so
 that you can examine or modify the document.
 *AFileName is the name of the XML document to load from disk. If AFileName is an empty string, TALXMLDocument uses the value of the
  FileName property. If AFileName is not an empty string, TALXMLDocument changes the FileName property to AFileName.
 Once you have loaded an XML document, any changes you make to the document are not saved back to disk until you call the SaveToFile method.}
procedure TALXMLDocument.LoadFromFile(const AFileName: TALXMLString = '');
begin
  SetActive(False);
  if AFileName <> '' then FileName := AFileName;
  if FileName = '' then ALXMLDocError(CALXmlMissingFileName);
  SetXMLStrings('');
  SetActive(True);
end;

{****************************************************}
{Loads an XML document from a stream and activates it.
 Call LoadFromStream to load the XML document from a stream.
 *Stream is a stream object that can be used to read the string of XML that makes up the document.
 After loading the document from Stream, LoadFromStream sets the Active property to true.}
procedure TALXMLDocument.LoadFromStream(const Stream: TStream);
begin
  SetActive(False);
  SetXMLStrings('');
  FSrcStream := Stream;
  SetActive(True);
end;

{*****************************************************************}
{Loads a string representation of an XML document and activates it.
 Call LoadFromXML to assign a string as the value of the XML document. Unlike the XML property, which lets you assign XML on a line-by-line
 basis, LoadFromXML treats the text of the XML document as a whole.
 The XML parameter is a string containing the text of an XML document. It should represent the XML text encoded using UTF-8, Unicode, or UTF-16.
 After assigning the XML property as the contents of the document, LoadFromXML sets the Active property to true.}
procedure TALXMLDocument.LoadFromXML(const XML: String);
var StringStream: TStringSTream;
begin
  StringStream := TStringStream.Create(XML);
  try
    LoadFromStream(StringStream);
  finally
    StringStream.Free;
  end;
end;

{******************************}
{Saves the XML document to disk.
 Call SaveToFile to save any modifications you have made to the parsed XML document.
 AFileName is the name of the file to save. If AFileName is an empty string, TXMLDocument uses the value of the FileName property.}
procedure TALXMLDocument.SaveToFile(const AFileName: TALXMLString = '');
Var afileStream: TfileStream;
begin
  if AFileName = '' then aFileStream := TfileStream.Create(FFilename,fmCreate)
  else aFileStream := TfileStream.Create(AFileName,fmCreate);
  Try
    SaveToStream(aFileStream);
  finally
    aFileStream.Free;
  end;
end;

{****************************************}
procedure TALXMLDocument.SaveToXMLStrings;
var XMLData: TALXMLstring;
begin
  SaveToXML(XMLData);
  SetXMLStrings(XMLData);
end;

{************************************************}
{Saves the XML document to a string-type variable.
 Call SaveToXML to save the contents of the XML document to the string-type variable specified by XML. SaveToXML writes the contents of XML document
 using UTF-8 or UTF-16 as an encoding system, depending on the type of the XML parameter.
 Unlike the XML property, which lets you write individual lines from the XML document, SaveToXML writes the entire text of the XML document.}
procedure TALXMLDocument.SaveToXML(var XML: TalXmlString);
Var StringStream: TstringStream;
begin
  StringStream := TstringStream.Create('');
  Try
    SaveToStream(StringStream);
    XML := StringStream.DataString;
  finally
    StringStream.Free;
  end;
end;

{**********************************}
{Saves the XML document to a stream.
 Call SaveToStream to save the contents of the XML document to the stream specified by Stream. Regardless of the encoding system
 of the original XML document, SaveToStream always saves the stream in UTF-16.}
procedure TALXMLDocument.SaveToStream(const Stream: TStream);
begin
  CheckActive;
  node.SaveToStream(Stream);
end;

{***********************************************************}
procedure TALXMLDocument.XMLStringsChanging(Sender: TObject);
begin
  if not (csLoading in ComponentState) and Active then SetActive(False);
  FFileName := '';
end;

{**********************************************************}
procedure TALXMLDocument.SetXMLStrings(const Value: string);
begin
  { Unhook the OnChanging event so we don't close the doc when refreshing }
  FXMLStrings.OnChanging := nil;
  try
    FXMLStrings.Text := Value;
  finally
    FXMLStrings.OnChanging := XMLStringsChanging;
  end;
end;

{*************************************}
{Returns the value of the XML property.
 GetXML is the read implementation of the XML property.}
function TALXMLDocument.GetXML: TStrings;
begin
  if Active then SaveToXMLStrings;
  Result := FXMLStrings;
end;

{**********************************}
{Sets the value of the XML property.
 SetXML is the write implementation of the XML property.
 *Value contains the raw (unparsed) XML to assign.}
procedure TALXMLDocument.SetXML(const Value: TStrings);
begin
  FXMLStrings.Assign(Value);
end;

{***********************************}
procedure TALXMLDocument.CheckActive;
begin
  if not Assigned(FDocumentNode) then ALXMLDocError(CALXmlNotActive);
end;

{*************************************}
procedure TALXMLDocument.CheckAutoSave;
begin
  if (doAutoSave in FOptions) then
    case DocSource of
      xdsXMLProperty: SaveToXMLStrings;
      xdsFile: SaveToFile(FileName);
    end;
end;

{************************************************************************}
{Creates and adds a node to the child nodes of this document.
 Call AddChild to add a new child element node to the document itself. The new node is added to the end of the list maintained
 by the ChildNodes property.
 *TagName is the tag name for the new element node.
 AddChild returns the interface for the newly created node.
 Note:	Do not call AddChild to add a child to the document element of this document. When adding data to the XML document, use the
 AddChild method of the document element or of the node in the hierarchy that should be the parent of the new node.}
function TALXMLDocument.AddChild(const TagName: TALXMLString): TALXMLNode;
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
function TALXMLDocument.CreateElement(const TagOrData: TALXMLString): TALXMLNode;
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
function TALXMLDocument.CreateNode(const NameOrData: TALXMLString; NodeType: TALXMLNodeType = ntElement; const AddlData: TALXMLString = ''): TALXMLNode;
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

{******************************************}
{Returns the value of the FileName property.
 GetFileName is the read implementation of the FileName property.}
function TALXMLDocument.GetFileName: TALXMLString;
begin
  Result := FFileName;
end;

{***************************************}
{Sets the value of the FileName property.
 SetFileName is the write implementation of the FileName property.
 *Value is the name of an XML document.}
procedure TALXMLDocument.SetFileName(const Value: TALXMLString);
begin
  if Value <> FFileName then begin
    if Active and (DocSource = xdsFile) then SetActive(False)
    else SetXMLStrings('');
    FFileName := Value;
  end;
end;

{***********************************************}
{Returns the value of the NodeIndentStr property.
 GetNodeIndentStr is the read implementation of the NodeIndentStr property.}
function TALXMLDocument.GetNodeIndentStr: TALXMLString;
begin
  Result := FNodeIndentStr;
end;

{********************************************}
{Sets the value of the NodeIndentStr property.
 SetNodeIndentStr is the write implementation of the NodeIndentStr property.
 *Value is the string that is inserted before nested nodes to indicate a level of nesting.}
procedure TALXMLDocument.SetNodeIndentStr(const Value: TALXMLString);
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

{*******************************************}
function TALXMLDocument.IsXMLStored: Boolean;
begin
  Result := (Active and (DocSource = xdsXMLProperty)) or
      	    ((not Active) and (FXMLStrings.Count > 0));
end;

{************************************************}
function TALXMLDocument.NodeIndentStored: Boolean;
begin
  Result := NodeIndentStr <> cALDefaultNodeIndent;
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

{*******************************************************************************************************************}
function TALXMLDocument.GetPrologValue(PrologItem: TALXMLPrologItem; const Default: TALXMLString = ''): TALXMLString;
var PrologNode: TALXMLNode;
    PrologAttrs: string;
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

{**************************************************************************************************************************************************}
function TALXMLDocument.InternalSetPrologValue(const PrologNode: TALXMLNode; const Value: TALXmlString; PrologItem: TALXMLPrologItem): TALxmlString;
var Version, Encoding, Standalone: TALXmlString;
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

{***********************************************************************************************}
procedure TALXMLDocument.SetPrologValue(const Value: TALXmlString; PrologItem: TALXMLPrologItem);
var PrologAttrs: TALXmlString;
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
function TALXMLDocument.GetEncoding: TALXMLString;
begin
  Result := GetPrologValue(xpEncoding);
end;

{***************************************}
{Sets the value of the Encoding property.
 SetEncoding is the write implementation of the Encoding property.
 *Value is the value of the encoding attribute of the document type node.}
procedure TALXMLDocument.SetEncoding(const Value: TALXMLString);
begin
  SetPrologValue(Value, xpEncoding);
end;

{*****************************************}
{Returns the value of the Version property.
GetVersion is the read implementation of the Version property.}
function TALXMLDocument.GetVersion: TALXMLString;
begin
  Result := GetPrologValue(xpVersion);
end;

{**************************************}
{Sets the value of the Version property.
 SetVersion is the write implementation of the Version property.
 *Value is the value of the version attribute of the document type node.}
procedure TALXMLDocument.SetVersion(const Value: TALXMLString);
begin
  SetPrologValue(Value, xpVersion);
end;

{********************************************}
{Returns the value of the StandAlone property.
 GetStandAlone is the read implementation of the StandAlone property.}
function TALXMLDocument.GetStandAlone: TALXMLString;
begin
  Result := GetPrologValue(xpStandalone);
end;

{*****************************************}
{Sets the value of the StandAlone property.
 SetStandAlone is the write implementation of the StandAlone property.
 *Value is the value of the stand-alone attribute of the document type node.}
procedure TALXMLDocument.SetStandAlone(const Value: TALXMLString);
begin
  SetPrologValue(Value, xpStandalone);
end;

{************************************}
procedure TALXMLDocument.DoAfterClose;
begin
  if Assigned(FAfterClose) then FAfterClose(Self);
end;

{***********************************}
procedure TALXMLDocument.DoAfterOpen;
begin
  if Assigned(FAfterOpen) then FAfterOpen(Self);
end;

{*************************************}
procedure TALXMLDocument.DoBeforeClose;
begin
  if Assigned(FBeforeClose) then FBeforeClose(Self);
end;

{************************************}
procedure TALXMLDocument.DoBeforeOpen;
begin
  if Assigned(FBeforeOpen) then FBeforeOpen(Self);
end;

{***************************************************************}
procedure TALXMLDocument.DoParseComment(const str: TALXMLString);
begin
  if Assigned(FOnParseComment) then FOnParseComment(Self, Str);
end;

{*************************************************************}
procedure TALXMLDocument.DoParseCData(const str: TALXMLString);
begin
  if Assigned(FOnParseCData) then FOnParseCData(Self, Str);
end;

{******************************************}
procedure TALXMLDocument.DoParseEndDocument;
begin
  if Assigned(FOnParseEndDocument) then FOnParseEndDocument(Self);
end;

{*******************************************************************}
procedure TALXMLDocument.DoParseEndElement(const Name: TALXMLString);
begin
  if Assigned(FOnParseEndElement) then FOnParseEndElement(Self, Name);
end;

{*************************************************************************************}
procedure TALXMLDocument.DoParseProcessingInstruction(const Target, Data: TALXMLString);
begin
  if Assigned(FOnParseProcessingInstruction) then FOnParseProcessingInstruction(Self, Target, Data);
end;

{********************************************}
procedure TALXMLDocument.DoParseStartDocument;
begin
  if Assigned(FOnParseStartDocument) then FOnParseStartDocument(Self);
end;

{*************************************************************************************************}
procedure TALXMLDocument.DoParseStartElement(const Name: TALXMLString; const Attributes: Tstrings);
begin
  if Assigned(FOnParseStartElement) then FOnParseStartElement(Self, Name, Attributes);
end;

{************************************************************}
procedure TALXMLDocument.DoParseText(const str: TALXMLString);
begin
  if Assigned(FOnParseText) then FOnParseText(Self, Str);
end;




//////////////////////////////
//////////TALXMLNode//////////
//////////////////////////////

{********************************}
{Instantiates a TALXMLNode object.
 Create instantiates a new TALXMLNode object.
 *AParentNode is the implementation of the parent node of this node.
 *OwnerDoc represents the XML document in which the new node occurs.
 Typically, applications do not directly call the TALXMLNode constructor. Instead, new nodes are
 created automatically for an XML document as necessary. To add new nodes to a document,
 applications call the parent node’s AddChild method.}
constructor TALXMLNode.Create(const NameOrData: TALXmlString;
                              const AddlData: TALXmlString = '');
Begin
  FDocument := nil;
end;

{*************************************************************}
{Sets the value of OwnerDocument to nil (Delphi) or NULL (C++).
 In addition to setting the OwnerDocument property to nil (Delphi) or NULL (C++),
 ClearDocumentRef propagates this call to all of this nodes child nodes and attribute nodes.}
procedure TALXMLNode.ClearDocumentRef;
begin
  SetDocumentRef(nil);
end;

{******************************}
{Sets the value of OwnerDocument
 In addition to setting the OwnerDocument property, ClearDocumentRef propagates this call to all
 of this nodes child nodes and attribute nodes.}
procedure TALXMLNode.SetDocumentRef(ADocumentRef: TALXmlDocument);
var I: Integer;
    aNodeList: TALXmlNodeList;
begin
  FDocument := ADocumentRef;

  aNodeList := InternalGetChildNodes;
  if Assigned(aNodeList) then
    for I := 0 to aNodeList.Count - 1 do
      aNodeList.NodeByIndex[I].SetDocumentRef(ADocumentRef);

  aNodeList := InternalGetAttributeNodes;
  if Assigned(aNodeList) then
    for I := 0 to aNodeList.Count - 1 do
      aNodeList.NodeByIndex[I].SetDocumentRef(ADocumentRef);
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
  Result := nil; //Virtual;
end;

{*********************************************}
{Sets the value of the AttributeNodes property.
 Applications can’t call this protected method. It is called internally the first time an application reads the
 AttributeNodes property to supply the interface that implements that property.}
procedure TALXMLNode.SetAttributeNodes(const Value: TALXMLNodeList);
begin
  //Virtual;
end;

{*****************************************************************}
{Indicates whether the node has an attribute with a specified name.
 *Name is the name of the attribute about which you want to know.
 HasAttribute returns true if this node has the specified attribute, false otherwise.}
function TALXMLNode.HasAttribute(const Name: TALXMLString): Boolean;
Var aNodeList: TALXMLNodeList;
begin
  aNodeList := InternalGetAttributeNodes;
  if assigned(aNodeList) then Result := aNodeList.IndexOf(Name) <> -1
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
function TALXMLNode.GetAttribute(const AttrName: TALXMLString): OleVariant;
Var aNode: TALXmlNode;
    aNodeList: TALXMLNodeList;
begin
  aNodeList := InternalGetAttributeNodes;
  if assigned(aNodeList) then aNode := aNodeList.findNode(AttrName)
  else aNode := Nil;

  if assigned(aNode) then Result := aNode.NodeValue
  else Begin
    if not Assigned(FDocument) or (doAttrNull in FDocument.Options) then Result := Null
    else Result := '';
  end
end;

{***********************************************************************************}
function TALXMLNode.GetAttributeAsString(const AttrName: TALXMLString): TALXMLString;
Var aNode: TALXmlNode;
    aNodeList: TALXMLNodeList;
begin
  aNodeList := InternalGetAttributeNodes;
  if assigned(aNodeList) then aNode := aNodeList.findNode(AttrName)
  else aNode := Nil;

  if assigned(aNode) then Result := aNode.Text
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
procedure TALXMLNode.SetAttribute(const AttrName: TALXMLString; const Value: OleVariant);
var aNode: TALXmlNode;
    aNodeList: TALXMLNodeList;
begin
  aNodeList := AttributeNodes;
  if not VarIsNull(Value) then begin
    aNode := aNodeList.FindNode(attrName);
    if not assigned(aNode) then begin
      ANode := ALCreateXmlNode(
                               AttrName,
                               ntattribute,
                               ''
                              );
      aNode.Text := Value;
      aNodeList.Add(aNode);
    end
    else aNode.Text := Value;
  end
  else aNodeList.Delete(attrName)
end;

{*************************************************************************************************}
procedure TALXMLNode.SetAttributeAsString(const AttrName: TALXMLString; const Value: TALXMLString);
var aNode: TALXmlNode;
    aNodeList: TALXMLNodeList;
begin
  aNodeList := AttributeNodes;
  aNode := aNodeList.FindNode(attrName);
  if not assigned(aNode) then begin
    ANode := ALCreateXmlNode(
                             AttrName,
                             ntattribute,
                             ''
                            );
    aNode.Text := Value;
    aNodeList.Add(aNode);
  end
  else aNode.Text := Value;
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
Var aNodeList: TALXMLNodeList;
begin
  aNodeList := InternalGetChildNodes;
  Result := assigned(aNodeList) and (aNodeList.Count > 0);
end;

{********************************************}
{Returns the value of the ChildNodes property.
 GetChildNodes is the protected read implementation of the ChildNodes property.
 GetChildNodes generates the list that implements the ChildNodes property the first time an application
 reads the ChildNodes property.}
function TALXMLNode.GetChildNodes: TALXMLNodeList;
begin
  result := nil; //virtual
end;

{*****************************************}
{Sets the value of the ChildNodes property.
 Applications can’t call this protected method. It is called internally the first time an application reads the ChildNodes
 property to supply the interface that implements that property.}
procedure TALXMLNode.SetChildNodes(const Value: TALXMLNodeList);
begin
  //virtual
end;

{***************************************************************************************}
function TALXMLNode.AddChild(const TagName: TALXMLString; Index: Integer = -1): TALXMLNode;
begin
  Result := InternalAddChild(TagName, Index);
end;

{**************************************************************}
{Provides the underlying implementation for the AddChild method.
 Applications can’t call the protected InternalAddChild method. It is used internally to perform all the tasks common to
 the various overloads of the AddChild method. InternalAddChild creates a new element node as the child of this node and
 returns its interface.
 *NodeName specifies the tag name of the newly created node.
 *NamespaceURI identifies the namespace that includes the new node’s definition.
 *Index indicates the position of the child node in this node’s list of children, where 0 is the first position, 1 is the second
 position, and so on. If Index is –1, the new node is added to the end.}
function TALXMLNode.InternalAddChild(const NodeName: TALXMLString; Index: Integer): TALXMLNode;
begin
  Result := ALCreateXmlNode(NodeName,ntElement,'');
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
function TALXMLNode.GetText: TALXMLString;
begin
  case NodeType of
    NtElement:         Begin
                         CheckTextNode;
                         If (InternalGetChildNodes <> nil) then result := ChildNodes[0].Text
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
  end;
end;

{*******************************}
{Sets the text value of the node.
 SetText replaces the text of the node.
 SetText is intended for use when the GetIsTextElement method returns true. If GetIsTextElement
 returns false, SetText adds a new text value to a node that has no children, otherwise, it
 raises an exception.}
procedure TALXMLNode.SetText(const Value: TALXMLString);
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
function TALXMLNode.GetNodeValue: OleVariant;
begin
  Result := GetText;
  if Result = '' then Result := Null;
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
procedure TALXMLNode.SetNodeValue(const Value: OleVariant);
begin
  SetText(VarToStr(Value));
end;

{*******************************************}
{Returns the value of a specified child node.
 GetChildValue returns the value of the node.
 IndexOrName identifies the desired child node by either its interface, or by a string that is the local name of the child node.
 If IndexOrName does not identify a child of this node, GetChildValue nil (Delphi) or NULL (C++). Otherwise, the return value
 depends on the type of the child node, as indicated in the following table:

 NodeType           Value
 ntAttribute        The attribute value
 ntElement	        If the element contains only text, this is that text value. Otherwise, GetChildValue raises an exception.
 ntText	            The text
 ntCData	          The content of the CDATA section.
 ntEntityRef	      nil (Delphi) or NULL (C++)
 ntProcessingInstr	The content of the processing instruction except for the target.
 ntComment	        The value (text) of the comment.
 ntDocFragment	    nil (Delphi) or NULL (C++)

 Note: Attributes are not considered child nodes of a node. To get the values of this node’s attributes,
 use the GetAttribute method instead.}
function TALXMLNode.GetChildValue(const IndexOrName: OleVariant): OleVariant;
var aNode: TALXMLNode;
    aNodeList: TALXMLNodelist;
begin
  aNodeList := InternalGetChildNodes;

  If assigned(aNodeList) then aNode := ChildNodes.FindNode(TALXMLString(IndexOrName))
  else aNode := nil;

  if Assigned(aNode) then Result := aNode.NodeValue
  else Result := Null;
end;

{****************************************}
{Sets the value of a specified child node.
 SetChildValue sets the value of a specified child node to a specified value.
 *IndexOrName identifies the desired child node by either its interface, or by a string that is the local name of the child node.
  If IndexOrName does not identify a child of this node, SetChildValue raises an exception.
 *Value is the value to assign. It’s interpretation depends on the type of the child node, as indicated in the following table:

  NodeType          Value
  ntAttribute	      The attribute value
  ntElement         If the element contains only text, this is that text value. Otherwise, SetChildValue raises an exception.
  ntText            The text
  ntCData           The content of the CDATA section.
  ntProcessingInstr	The content of the processing instruction except for the target.
  ntComment	        The value (text) of the comment.

 If the child node is any other node type, SetChildValue raises an exception.
 Note: Attributes are not considered child nodes of a node. To set the values of this node’s attributes, use the
 SetAttribute method instead.}
procedure TALXMLNode.SetChildValue(const IndexOrName, Value: OleVariant);
Var aNode: TALXmlNode;
    aNodeList: TALXmlNodeList;
begin
  aNodeList := ChildNodes;
  if assigned(aNodeList) then begin
    aNode := aNodeList[IndexOrName];
    if assigned(aNode) then aNode.NodeValue := Value;
  end;
end;

{******************************************************************************}
{Returns the XML that corresponds to this node and any child nodes it contains.}
procedure TALXMLNode.SaveToStream(const Stream: TStream; Const SaveOnlyChildNode: Boolean=False);
Var NodeStack: Tstack;
    CurrentNode: TalxmlNode;
    CurrentParentNode: TalxmlNode;
    EncodeXmlReferences: Boolean;

  {------------------------------------------}
  Procedure WriteStr2Stream(str:TALXmlString);
  Begin
    stream.Write(Str[1],Length(Str));
  end;

  {-------------------------------------------------------------}
  Procedure WriteAttributeNode2Stream(aAttributeNode:TALXmlNode);
  Begin
    with aAttributeNode do
      if EncodeXmlReferences then WriteStr2Stream(' ' + NodeName + '="' + ALXMLTextElementEncode(text) + '"')
      else WriteStr2Stream(' ' + NodeName + '="' + text + '"');
  end;

  {---------------------------------------------------}
  Procedure WriteTextNode2Stream(aTextNode:TALXmlNode);
  Begin
    with aTextNode do
      if EncodeXmlReferences then WriteStr2Stream(ALXMLTextElementEncode(text))
      else WriteStr2Stream(text);
  end;

  {----------------------------------------------------}
  Procedure WriteCDATANode2Stream(aTextNode:TALXmlNode);
  Begin
    with aTextNode do
      WriteStr2Stream('<![CDATA[' + text + ']]>');
  end;

  {---------------------------------------------------------}
  Procedure WriteCommentNode2Stream(aCommentNode:TALXmlNode);
  Begin
    with acommentNode do
      WriteStr2Stream('<!--' + text + '-->');
  end;

  {-------------------------------------------------------------------------}
  Procedure WriteProcessingInstrNode2Stream(aProcessingInstrNode:TALXmlNode);
  Var aText: TALXmlString;
  Begin
    with aProcessingInstrNode do begin
      WriteStr2Stream('<?'+NodeName);

      aText := Text;
      If aText <> '' then
        WriteStr2Stream(' ' + aText);

      WriteStr2Stream('?>');
    end;
  end;

  {--------------------------------------------------------------}
  Procedure WriteStartElementNode2Stream(aElementNode:TALXmlNode);
  var i: integer;
      ANodeList: TALXmlNodeList;
      TagEnclosed: Boolean;
  Begin
    with aElementNode do begin
      WriteStr2Stream('<'+NodeName);

      aNodeList := InternalGetAttributeNodes;
      If assigned(aNodeList) then
        with aNodeList do
          For i := 0 to Count - 1 do
            WriteAttributeNode2Stream(NodeByIndex[i]);

      TagEnclosed := True;
      aNodeList := InternalGetChildNodes;
      If assigned(aNodeList) then begin
        with aNodeList do
          If count > 0 then begin
            TagEnclosed := False;
            WriteStr2Stream('>');
            NodeStack.Push(aElementNode);
            For i := Count - 1 downto 0 do NodeStack.Push(NodeByIndex[i]);
          end
      end;

      If tagEnclosed then WriteStr2Stream('/>');
    end;
  end;

  {------------------------------------------------------------}
  Procedure WriteEndElementNode2Stream(aElementNode:TALXmlNode);
  Begin
    with aElementNode do
      WriteStr2Stream('</'+NodeName+'>');
  end;

  {-----------------------------------------------------------}
  Procedure WriteDocumentNode2Stream(aDocumentNode:TALXmlNode);
  var i: integer;
      ANodeList: TALXmlNodeList;
  Begin
    with aDocumentNode do begin

      aNodeList := InternalGetChildNodes;
      If assigned(aNodeList) then
        with aNodeList do
          For i := Count - 1 downto 0 do NodeStack.Push(NodeByIndex[i]);

    end;
  end;

begin
  If not (NodeType in [ntElement, ntDocument]) then exit;

  CurrentParentNode := nil;
  NodeStack := Tstack.Create;
  Try

    {EncodeXmlReferences from ParseOptions}
    EncodeXmlReferences := not (poIgnoreXmlReferences in FDocument.ParseOptions);

    {SaveOnlyChildNode}
    If SaveOnlyChildNode or (NodeType = ntDocument) then WriteDocumentNode2Stream(self)
    else NodeStack.Push(self);

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
        end;

      CurrentParentNode := CurrentNode.ParentNode;
      If assigned(CurrentParentNode) and
         (CurrentParentNode.nodeType = ntDocument) and
         (CurrentNode.nodeType <> ntelement) then WriteStr2Stream(#13#10);
    end;

  finally
    NodeStack.Free;
  end;
end;

{*****************************************************************************************************}
procedure TALXMLNode.SaveToFile(const AFileName: TALXMLString; Const SaveOnlyChildNode: Boolean=False);
Var afileStream: TfileStream;
begin
  aFileStream := TfileStream.Create(AFileName,fmCreate);
  Try
    SaveToStream(aFileStream, SaveOnlyChildNode);
  finally
    aFileStream.Free;
  end;
end;

{**************************************************************************************************}
{load OnlyChildNode mean the the stream contain ONLY the child node, so it's not a valid xml stream
 like <root>...</root> but more like <rec>...</rec><rec>...</rec><rec>...</rec>}
procedure TALXMLNode.LoadFromFile(const AFileName: TALXMLString; Const FileContainOnlyChildNode: Boolean=False);
Var afileStream: TfileStream;
Begin
  aFileStream := TfileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromStream(aFileStream, FileContainOnlyChildNode);
  finally
    aFileStream.Free;
  end;
end;

{*************************************************************************************************}
{load OnlyChildNode mean the the stream contain ONLY the child node, so it's not a valid xml stream
 like <root>...</root> but more like <rec>...</rec><rec>...</rec><rec>...</rec>}
procedure TALXMLNode.LoadFromStream(const Stream: TStream; Const StreamContainOnlyChildNode: Boolean=False);
Begin
  If not (NodeType in [ntElement, ntDocument]) then exit;
  ChildNodes.Clear;
  AttributeNodes.Clear;
  Try
    FDocument.InternalParseXml(Stream, self, StreamContainOnlyChildNode);
  except
    ChildNodes.Clear;
    AttributeNodes.Clear;
    raise;
  end;
end;

{*******************************************************************}
{Returns the XML that corresponds to the subtree rooted at this node.
 GetXML returns the XML that corresponds to this node and any child nodes it contains.}
function TALXMLNode.GetXML: TALXMLString;
Var StringStream: TstringStream;
begin
  StringStream := TstringStream.Create('');
  Try
    SaveToStream(StringStream);
    Result := StringStream.DataString;
  finally
    StringStream.Free;
  end;
end;

{*****************************}
{Returns the type of the node.}
function TALXMLNode.GetNodeType: TALXMLNodeType;
begin
  Result := ntReserved //virtual
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
function TALXMLNode.GetLocalName: TALXMLString;
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
function TALXMLNode.GetNodeName: TALXMLString;
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
function TALXMLNode.GetPrefix: TALXMLString;
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
Var aNodeList: TALXMLNodeList;
begin
  aNodeList := InternalGetChildNodes;
  Result := (NodeType=NtElement) and
            (
             (not assigned(aNodeList)) or
             (
              (aNodeList.Count = 1) and
              (aNodeList[0].nodetype in [ntText, ntCData])
             )
            );
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
  //virtual;
  If assigned(Value) then setDocumentRef(Value.OwnerDocument)
  else ClearDocumentRef;
end;

{******************************************************}
function TALXMLNode.GetInternalChildValue: TALXMLString;
begin
  Result := ''; //virtual
end;

{*************************************************}
function TALXMLNode.GetInternalValue: TALXMLString;
begin
  Result := ''; //virtual
end;

{********************************************************************}
procedure TALXMLNode.SetInternalChildValue(const Value: TALXMLString);
begin
  //virtual
end;

{***************************************************************}
procedure TALXMLNode.SetInternalValue(const Value: TALXMLString);
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
begin
  FDocument := Value;
end;



/////////////////////////////////////
//////////TALXmlElementNode//////////
/////////////////////////////////////

{*******************************************************************}
constructor TALXmlElementNode.Create(const NameOrData: TALXmlString);
Var i: integer;
begin
  inherited create(NameOrData);
  FParentNode := nil;
  FAttributeNodes:= nil;
  FChildNodes:=nil;

  {Check if the name abides the rules. We will be very forgiving here and
   just accept any name that at least does not contain control characters}
  for i := 1 to length(NameOrData) do
    if NameOrData[i] in [' ', #9, #13, #10] then AlXmlDocError(CALXMLParameterIsIncorrect);
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
  If Not (Node.NodeType in [ntElement, ntText, ntAttribute, ntCData, ntEntityRef, ntProcessingInstr, ntComment]) then ALXmlDocError(CALXMLParameterIsIncorrect);
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

{********************************************************}
function TALXmlElementNode.GetInternalValue: TALXMLString;
begin
  Result := FInternalValue;
end;

{**********************************************************************}
procedure TALXmlElementNode.SetInternalValue(const Value: TALXMLString);
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




///////////////////////////////////////
//////////TALXmlAttributeNode//////////
///////////////////////////////////////

{*********************************************************************}
constructor TALXMLAttributeNode.Create(const NameOrData: TALXmlString);
Var i: integer;
begin
  inherited create(NameOrData);
  FChildNodes:=nil;

  // Check if the name abides the rules. We will be very forgiving here and
  // just accept any name that at least does not contain control characters
  for i := 1 to length(NameOrData) do
    if NameOrData[i] in [' ', #9, #13, #10] then AlXmlDocError(CALXMLParameterIsIncorrect);
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

{**********************************************************}
function TALXMLAttributeNode.GetInternalValue: TALXMLString;
begin
  Result := FInternalValue;
end;

{************************************************************************}
procedure TALXMLAttributeNode.SetInternalValue(const Value: TALXMLString);
begin
  FInternalValue := Value;
end;

{********************************************}
{Get Childnode without create it if not exist}
function TALXMLAttributeNode.InternalGetChildNodes: TALXMLNodeList;
begin
  Result := FChildNodes;
end;




//////////////////////////////////
//////////TALXmlTextNode//////////
//////////////////////////////////

{****************************************************************}
constructor TALXmlTextNode.Create(const NameOrData: TALXmlString);
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

{*****************************************************}
function TALXmlTextNode.GetInternalValue: TALXMLString;
begin
  Result := FInternalValue;
end;

{*******************************************************************}
procedure TALXmlTextNode.SetInternalValue(const Value: TALXMLString);
begin
  FInternalValue := Value;
end;



///////////////////////////////////
//////////TALXmlCDATANode//////////
///////////////////////////////////

{*****************************************************************}
constructor TALXmlCDataNode.Create(const NameOrData: TALXmlString);
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

{******************************************************}
function TALXmlCDataNode.GetInternalValue: TALXMLString;
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

{********************************************************************}
procedure TALXmlCDataNode.SetInternalValue(const Value: TALXMLString);
begin
  FInternalValue := Value;
end;

{***************************************************************}
procedure TALXmlCDataNode.SetParentNode(const Value: TALXMLNode);
begin
  inherited;
  FParentNode := Value
end;



/////////////////////////////////////
//////////TALXmlCommentNode//////////
/////////////////////////////////////

{*******************************************************************}
constructor TALXmlCommentNode.Create(const NameOrData: TALXmlString);
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

{********************************************************}
function TALXmlCommentNode.GetInternalValue: TALXMLString;
begin
  Result := FInternalValue;
end;

{**********************************************************************}
procedure TALXmlCommentNode.SetInternalValue(const Value: TALXMLString);
begin
  FInternalValue := Value;
end;




/////////////////////////////////////////////
//////////TALXmlProcessingInstrNode//////////
/////////////////////////////////////////////

{**************************************************************************}
constructor TALXmlProcessingInstrNode.Create(const NameOrData: TALXmlString;
                                             const AddlData: TALXmlString = '');
Var i: integer;
begin
  inherited;
  FParentNode := nil;

  // Check if the name abides the rules. We will be very forgiving here and
  // just accept any name that at least does not contain control characters
  for i := 1 to length(NameOrData) do
    if NameOrData[i] in [' ', #9, #13, #10] then AlXmlDocError(CALXMLParameterIsIncorrect);
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

{*********************************************************************}
function TALXmlProcessingInstrNode.GetInternalChildValue: TALXMLString;
begin
  Result := FInternalChildValue;
end;

{****************************************************************}
function TALXmlProcessingInstrNode.GetInternalValue: TALXMLString;
begin
  Result := FInternalValue;
end;

{***********************************************************************************}
procedure TALXmlProcessingInstrNode.SetInternalChildValue(const Value: TALXMLString);
begin
  FInternalChildValue := Value;
end;

{******************************************************************************}
procedure TALXmlProcessingInstrNode.SetInternalValue(const Value: TALXMLString);
begin
  FInternalValue := Value;
end;



//////////////////////////////////////
//////////TALXmlDocumentNode//////////
//////////////////////////////////////

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
var aNodeList: TALXMLNodelist;
    i: integer;
begin
  aNodeList := InternalGetChildNodes;
  If assigned(aNodeList) then
    For i:=0 to aNodeList.Count - 1 do
      If aNodeList[i].NodeType = ntelement then alXmlDocError(CalXMLOnlyOneTopLevelError);

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




//////////////////////////////////
//////////TALXMLNodeList//////////
//////////////////////////////////

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
end;

{***************************************}
{Returns the number of nodes in the list.
 GetCount is the read implementation of the Count property.}
function TALXMLNodeList.GetCount: Integer;
begin
  Result := FCount;
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
  while (Result < FCount) and (FList^[Result] <> Node) do Inc(Result);
  if Result = FCount then Result := -1;
end;

{*************************************}
{Returns the index of a specified node.
 Call IndexOf to locate a node in the list.
 *Name is the LocalName property of the node to locate.
 IndexOf returns the index of the specified node, where 0 is the index of the first node, 1 is the
 index of the second node, and so on. If the specified node is not in the list, IndexOf returns -1.}
function TALXMLNodeList.IndexOf(const Name: TALXMLString): Integer;
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
function TALXMLNodeList.FindNode(NodeName: TALXMLString): TALXMLNode;
var Index: Integer;
begin
  Index := IndexOf(NodeName);
  if Index >= 0 then Result := Get(Index)
  else Result := nil;
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
  Result := FList^[Index];
end;

{**************************************}
{Returns a specified node from the list.
 GetNode is the read implementation of the Nodes property.
 *IndexOrName identifies the desired node. It can be The index of the node, where 0 is the index of the first node,
  1 is the index of the second node, and so on. The LocalName property of a node in the list.
 If IndexOrName does not identify a node in the list, GetNode tries to create a new node with the name specified by
 IndexOrName. If it can’t create the new node, GetNode raises an exception.}
function TALXMLNodeList.GetNode(const IndexOrName: OleVariant): TALXMLNode;
begin
  if VarIsOrdinal(IndexOrName) then Result := GetNodeByIndex(IndexOrName)
  else Result := GetNodeByName(TALXMLString(IndexOrName));
end;

{***********************************************************************}
function TALXMLNodeList.GetNodeByIndex(const Index: Integer): TALXMLNode;
begin
  Result := Get(Index);
end;

{**************************************************************************}
function TALXMLNodeList.GetNodeByName(const Name: TALXmlString): TALXMLNode;
begin
  Result := FindNode(TALXMLString(Name));
  if not Assigned(Result) then ALXMLDocError(CALXmlNodeNotFound, [Name]);
end;

{************************************************************************************}
procedure TALXMLNodeList.QuickSort(L, R: Integer; XCompare: TALXMLNodeListSortCompare);
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
    if Index < FCount then System.Move(
                                       FList^[Index],
                                       FList^[Index + 1],
                                       (FCount - Index) * SizeOf(Pointer)
                                      );
  end;
  FList^[index] := Node;
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

  procedure InsertFormattingNode(const Len, Index: Integer; Break: Boolean = True);
  var I: Integer;
      IndentNode: TALXMLNode;
      IndentStr: TALXMLString;
  begin
    for I := 1 to Len do IndentStr := IndentStr + Owner.OwnerDocument.NodeIndentStr;
    if Break then IndentStr := SLineBreak + IndentStr;
    with Owner do
      IndentNode := ALCreateXmlNode(
                                    IndentStr,
                                    ntText,
                                    ''
                                   );
    InternalInsert(Index, IndentNode);
  end;

var TrailIndent, NewIndex: Integer;
begin
  { Determine if we should add do formatting here }
  if Assigned(Owner.ParentNode) and
     (doNodeAutoIndent in Owner.OwnerDocument.Options) and
     not (Node.NodeType in [ntText, ntAttribute]) then
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
  if Index < FCount then System.Move(
                                     FList^[Index + 1],
                                     FList^[Index],
                                     (FCount - Index) * SizeOf(Pointer)
                                    );
  if assigned(Node) then FreeAndNil(Node);
  result := Index;
end;

{**************************************}
{Removes a specified node from the list.
 Delete removes the node specified by the Index or Name parameter.
 *Name identifies the node to remove from the list. This is the local name of the node to remove.
 Delete returns the index of the node that was removed. If there was no node that matched the value of Name, Delete returns –1.}
function TALXMLNodeList.Delete(const Name: TALXMLString): Integer;
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
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

{***********************************************************}
{Removes a specified object from the list without freeing it.
 Call Extract to remove an object from the list without freeing the object itself.
 After an object is removed, all the objects that follow it are moved up in index position and Count is decremented.}
function TALXMLNodeList.Extract(const index: integer): TALXMLNode;
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
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then ALXMLDocError(CALXmlListCapacityError, [NewCapacity]);
  if NewCapacity <> FCapacity then begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

{***************************************************}
procedure TALXMLNodeList.SetCount(NewCount: Integer);
var I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then ALXMLDocError(CALXmlListCountError, [NewCount]);
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else for I := FCount - 1 downto NewCount do Delete(I);
  FCount := NewCount;
end;



/////////////////////////////////
//////////Misc function//////////
/////////////////////////////////

{****************************************************************}
Function ALCreateEmptyXMLDocument(Rootname:string):TalXMLDocument;
begin
  Result := TAlXMLDocument.Create(nil);
  with result do begin
    Options := [];
    ParseOptions := [];
  end;
  ALClearXMLDocument(rootname,Result);
End;

{***********************************************************************************************************************}
procedure ALClearXMLDocument(rootname:string; xmldoc: TalXMLDocument; const EncodingStr: String = cAlXMLUTF8EncodingStr);
begin
  with xmlDoc do begin
    Active := False;
    XML.clear;
    FileName := '';
    Active := true;
    version := '1.0';
    standalone := 'yes';
    Encoding := EncodingStr;
  end;
  If RootName <> '' then XMLDoc.AddChild(rootname);
End;

{***************************************************}
Function  ALFindXmlNodeByAttribute(xmlrec:TalxmlNode;
                                   AttributeName, AttributeValue : string;
                                   Const SearchAlsoInChildNodes: Boolean = False): TalxmlNode;

var i : integer;
Begin
  result := nil;
  if not (xmlrec is TalXmlElementNode) then Exit;
  for i := 0 to xmlrec.ChildNodes.Count - 1 do begin
    If sametext(xmlrec.ChildNodes[i].AttributesAsString[AttributeName],AttributeValue) then begin
      result := xmlrec.ChildNodes[i];
      break;
    end;
    if SearchAlsoInChildNodes then begin
      result := ALFindXmlNodeByAttribute(xmlrec.ChildNodes[i],
                                         AttributeName,
                                         AttributeValue,
                                         SearchAlsoInChildNodes);
      if assigned(Result) then break;
    end;
  end;
end;

end.
