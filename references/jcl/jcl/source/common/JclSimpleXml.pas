{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JvSimpleXML.PAS, released on 2002-06-03.                                    }
{                                                                                                  }
{ The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com].    }
{ Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Christophe Paris,                                                                              }
{   Florent Ouchet (move from the JVCL to the JCL)                                                 }
{   Teträm                                                                                         }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains Xml parser and writter classes                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

// Known Issues: This component does not parse the !DOCTYPE tags but preserves them

unit JclSimpleXml;

interface

{$I jcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF HAS_UNIT_RTLCONSTS}
  System.RTLConsts,
  {$ENDIF HAS_UNIT_RTLCONSTS}
  {$IFDEF MSWINDOWS}
  Winapi.Windows, // Delphi 2005 inline
  {$ENDIF MSWINDOWS}
  System.SysUtils, System.Classes,
  System.Variants,
  System.IniFiles,
  System.Contnrs,
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF HAS_UNIT_RTLCONSTS}
  RTLConsts,
  {$ENDIF HAS_UNIT_RTLCONSTS}
  {$IFDEF MSWINDOWS}
  Windows, // Delphi 2005 inline
  {$ENDIF MSWINDOWS}
  SysUtils, Classes,
  Variants,
  IniFiles,
  Contnrs,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase, JclStreams;

type
  TJclSimpleItem = class(TObject)
  private
    FName: string;
  protected
    procedure SetName(const Value: string); virtual;
  public
    property Name: string read FName write SetName;
  end;

type
  TJclSimpleItemHashedList = class(TObjectList)
  private
    FNameHash: TStringHash;
    FCaseSensitive: Boolean;
    function GetSimpleItemByName(const Name: string): TJclSimpleItem;
    function GetSimpleItem(Index: Integer): TJclSimpleItem;
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(ACaseSensitive: Boolean);
    destructor Destroy; override;
    function Add(Item: TJclSimpleItem): Integer;
    function Extract(Item: TJclSimpleItem): TJclSimpleItem;
    procedure Clear; override;
    function IndexOfSimpleItem(Item: TJclSimpleItem): Integer;
    function IndexOfName(const Name: string): Integer;
    procedure Insert(Index: Integer; Item: TJclSimpleItem);
    procedure InvalidateHash;
    procedure Move(CurIndex, NewIndex: Integer);
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property SimpleItemByNames[const Name: string]: TJclSimpleItem read GetSimpleItemByName;
    property SimpleItems[Index: Integer]: TJclSimpleItem read GetSimpleItem;
  end;

type
  TJclSimpleData = class(TJclSimpleItem)
  private
    FValue: string;
    FData: Pointer;
  protected
    function GetBoolValue: Boolean;
    procedure SetBoolValue(const Value: Boolean);
    function GetFloatValue: Extended;
    procedure SetFloatValue(const Value: Extended);
    function GetAnsiValue: AnsiString;
    procedure SetAnsiValue(const Value: AnsiString);
    function GetIntValue: Int64;
    procedure SetIntValue(const Value: Int64);
  public
    constructor Create; overload; virtual;
    constructor Create(const AName: string); overload;
    constructor Create(const AName, AValue: string); overload;
    property Value: string read FValue write FValue;
    property AnsiValue: AnsiString read GetAnsiValue write SetAnsiValue;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property FloatValue: Extended read GetFloatValue write SetFloatValue;

    property Data: Pointer read FData write FData;
  end;

type
  TJclSimpleXMLData = class(TJclSimpleData)
  private
    FNameSpace: string;
  public
    function FullName:string;
    property NameSpace: string read FNameSpace write FNameSpace;
  end;

type
  TJclSimpleXML = class;
  EJclSimpleXMLError = class(EJclError);
  {$TYPEINFO ON} // generate RTTI for published properties
  TJclSimpleXMLElem = class;
  {$IFNDEF TYPEINFO_ON}
  {$TYPEINFO OFF}
  {$ENDIF ~TYPEINFO_ON}
  TJclSimpleXMLElems = class;
  TJclSimpleXMLProps = class;
  TJclSimpleXMLElemsProlog = class;
  TJclSimpleXMLNamedElems = class;
  TJclSimpleXMLElemComment = class;
  TJclSimpleXMLElemClassic = class;
  TJclSimpleXMLElemCData = class;
  TJclSimpleXMLElemDocType = class;
  TJclSimpleXMLElemText = class;
  TJclSimpleXMLElemHeader = class;
  TJclSimpleXMLElemSheet = class;
  TJclSimpleXMLElemMSOApplication = class;
  TJclOnSimpleXMLParsed = procedure(Sender: TObject; const Name: string) of object;
  TJclOnValueParsed = procedure(Sender: TObject; const Name, Value: string) of object;
  TJclOnSimpleProgress = procedure(Sender: TObject; const Position, Total: Integer) of object;

  //Those hash stuffs are for future use only
  //Plans are to replace current hash by this mechanism
  TJclHashKind = (hkList, hkDirect);
  PJclHashElem = ^TJclHashElem;
  TJclHashElem = packed record
    Next: PJclHashElem;
    Obj: TObject;
  end;
  PJclHashRecord = ^TJclHashRecord;
  TJclHashList = array [0..25] of PJclHashRecord;
  PJclHashList = ^TJclHashList;
  TJclHashRecord = packed record
    Count: Byte;
    case Kind: TJclHashKind of
      hkList: (List: PJclHashList);
      hkDirect: (FirstElem: PJclHashElem);
  end;

  TJclSimpleXMLProp = class(TJclSimpleXMLData)
  private
    FParent: TJclSimpleXMLElem;
  protected
    function GetSimpleXML: TJclSimpleXML;
    procedure SetName(const Value: string); override;
  public
    constructor Create(AParent: TJclSimpleXMLElem; const AName, AValue: string);
    procedure SaveToStringStream(StringStream: TJclStringStream);
    property Parent: TJclSimpleXMLElem read FParent;
    property SimpleXML: TJclSimpleXML read GetSimpleXML;
  end;

  {$IFDEF SUPPORTS_FOR_IN}
  TJclSimpleXMLPropsEnumerator = class
  private
    FIndex: Integer;
    FList: TJclSimpleXMLProps;
  public
    constructor Create(AList: TJclSimpleXMLProps);
    function GetCurrent: TJclSimpleXMLProp; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    function MoveNext: Boolean;
    property Current: TJclSimpleXMLProp read GetCurrent;
  end;
  {$ENDIF SUPPORTS_FOR_IN}

  TJclSimpleXMLProps = class(TObject)
  private
    FProperties: TStringList;
    FParent: TJclSimpleXMLElem;
    function GetCount: Integer;
    function GetItemNamedDefault(const Name, Default: string): TJclSimpleXMLProp;
    function GetItemNamed(const Name: string): TJclSimpleXMLProp;
  protected
    function GetSimpleXML: TJclSimpleXML;
    function GetItem(const Index: Integer): TJclSimpleXMLProp;
    procedure DoItemRename(Value: TJclSimpleXMLProp; const Name: string);
    procedure Error(const S: string);
    procedure FmtError(const S: string; const Args: array of const);
  public
    constructor Create(AParent: TJclSimpleXMLElem);
    destructor Destroy; override;
    procedure SortProperties(const Order: array of string);
    function Add(const Name, Value: string): TJclSimpleXMLProp; overload;
    {$IFDEF SUPPORTS_UNICODE}
    function Add(const Name: string; const Value: AnsiString): TJclSimpleXMLProp; overload;
    {$ENDIF SUPPORTS_UNICODE}
    function Add(const Name: string; const Value: Int64): TJclSimpleXMLProp; overload;
    function Add(const Name: string; const Value: Boolean): TJclSimpleXMLProp; overload;
    function Insert(const Index: Integer; const Name, Value: string): TJclSimpleXMLProp; overload;
    function Insert(const Index: Integer; const Name: string; const Value: Int64): TJclSimpleXMLProp; overload;
    function Insert(const Index: Integer; const Name: string; const Value: Boolean): TJclSimpleXMLProp; overload;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: TJclSimpleXMLPropsEnumerator;
    {$ENDIF SUPPORTS_FOR_IN}
    function Value(const Name: string; const Default: string = ''): string;
    function IntValue(const Name: string; const Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = True): Boolean;
    function FloatValue(const Name: string; const Default: Extended = 0): Extended;
    procedure LoadFromStringStream(StringStream: TJclStringStream);
    procedure SaveToStringStream(StringStream: TJclStringStream);
    property Item[const Index: Integer]: TJclSimpleXMLProp read GetItem; default;
    property ItemNamed[const Name: string]: TJclSimpleXMLProp read GetItemNamed;
    property Count: Integer read GetCount;
    property Parent: TJclSimpleXMLElem read FParent;
  end;

  {$IFDEF SUPPORTS_FOR_IN}
  TJclSimpleXMLElemsPrologEnumerator = class
  private
    FIndex: Integer;
    FList: TJclSimpleXMLElemsProlog;
  public
    constructor Create(AList: TJclSimpleXMLElemsProlog);
    function GetCurrent: TJclSimpleXMLElem; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    function MoveNext: Boolean;
    property Current: TJclSimpleXMLElem read GetCurrent;
  end;
  {$ENDIF SUPPORTS_FOR_IN}

  TJclSimpleXMLElemsProlog = class(TObject)
  private
    FElems: TJclSimpleItemHashedList;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TJclSimpleXMLElem;
    function GetEncoding: string;
    function GetStandalone: Boolean;
    function GetVersion: string;
    procedure SetEncoding(const Value: string);
    procedure SetStandalone(const Value: Boolean);
    procedure SetVersion(const Value: string);
  protected
    FSimpleXML: TJclSimpleXML;
    function FindHeader: TJclSimpleXMLElem;
    procedure Error(const S: string);
    procedure FmtError(const S: string; const Args: array of const);
  public
    constructor Create(ASimpleXML: TJclSimpleXML);
    destructor Destroy; override;
    function AddComment(const AValue: string): TJclSimpleXMLElemComment;
    function AddDocType(const AValue: string): TJclSimpleXMLElemDocType;
    procedure Clear;
    function AddStyleSheet(const AType, AHRef: string): TJclSimpleXMLElemSheet;
    function AddMSOApplication(const AProgId : string): TJclSimpleXMLElemMSOApplication;
    procedure LoadFromStringStream(StringStream: TJclStringStream);
    procedure SaveToStringStream(StringStream: TJclStringStream);
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: TJclSimpleXMLElemsPrologEnumerator;
    {$ENDIF SUPPORTS_FOR_IN}
    property Item[const Index: Integer]: TJclSimpleXMLElem read GetItem; default;
    property Count: Integer read GetCount;
    property Encoding: string read GetEncoding write SetEncoding;
    property SimpleXML: TJclSimpleXML read FSimpleXML;
    property Standalone: Boolean read GetStandalone write SetStandalone;
    property Version: string read GetVersion write SetVersion;
  end;

  {$IFDEF SUPPORTS_FOR_IN}
  TJclSimpleXMLNamedElemsEnumerator = class
  private
    FIndex: Integer;
    FList: TJclSimpleXMLNamedElems;
  public
    constructor Create(AList: TJclSimpleXMLNamedElems);
    function GetCurrent: TJclSimpleXMLElem; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    function MoveNext: Boolean;
    property Current: TJclSimpleXMLElem read GetCurrent;
  end;
  {$ENDIF SUPPORTS_FOR_IN}

  TJclSimpleXMLNamedElems = class(TJclSimpleItem)
  private
    FElems: TJclSimpleXMLElems;
    function GetCount: Integer;
  protected
    FItems: TList;
    function GetItem(const Index: Integer): TJclSimpleXMLElem;
    procedure SetName(const Value: string); override;
  public
    constructor Create(AElems: TJclSimpleXMLElems; const AName: string);
    destructor Destroy; override;

    function Add: TJclSimpleXMLElemClassic; overload;
    function Add(const Value: string): TJclSimpleXMLElemClassic; overload;
    function Add(const Value: Int64): TJclSimpleXMLElemClassic; overload;
    function Add(const Value: Boolean): TJclSimpleXMLElemClassic; overload;
    function Add(Value: TStream): TJclSimpleXMLElemClassic; overload;
    function AddFirst: TJclSimpleXMLElemClassic;
    function AddComment(const Value: string): TJclSimpleXMLElemComment;
    function AddCData(const Value: string): TJclSimpleXMLElemCData;
    function AddText(const Value: string): TJclSimpleXMLElemText;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer);
    procedure Move(const CurIndex, NewIndex: Integer);
    function IndexOf(const Value: TJclSimpleXMLElem): Integer; overload;
    function IndexOf(const Value: string): Integer; overload;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: TJclSimpleXMLNamedElemsEnumerator;
    {$ENDIF SUPPORTS_FOR_IN}

    property Elems: TJclSimpleXMLElems read FElems;
    property Item[const Index: Integer]: TJclSimpleXMLElem read GetItem; default;
    property Count: Integer read GetCount;
  end;

  {$IFDEF SUPPORTS_FOR_IN}
  TJclSimpleXMLElemsEnumerator = class
  private
    FIndex: Integer;
    FList: TJclSimpleXMLElems;
  public
    constructor Create(AList: TJclSimpleXMLElems);
    function GetCurrent: TJclSimpleXMLElem; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    function MoveNext: Boolean;
    property Current: TJclSimpleXMLElem read GetCurrent;
  end;
  {$ENDIF SUPPORTS_FOR_IN}

  TJclSimpleXMLElemCompare = function(Elems: TJclSimpleXMLElems; Index1, Index2: Integer): Integer of object;
  TJclSimpleXMLElems = class(TObject)
  private
    FParent: TJclSimpleXMLElem;
    function GetCount: Integer;
    function GetItemNamedDefault(const Name, Default: string): TJclSimpleXMLElem;
    function GetItemNamed(const Name: string): TJclSimpleXMLElem;
    function GetNamedElems(const Name: string): TJclSimpleXMLNamedElems;
  protected
    FElems: TJclSimpleItemHashedList;
    FCompare: TJclSimpleXMLElemCompare;
    FNamedElems: TJclSimpleItemHashedList;
    function GetItem(const Index: Integer): TJclSimpleXMLElem;
    procedure AddChild(const Value: TJclSimpleXMLElem);
    procedure AddChildFirst(const Value: TJclSimpleXMLElem);
    procedure InsertChild(const Value: TJclSimpleXMLElem; Index: Integer);
    procedure DoItemRename(Value: TJclSimpleXMLElem; const Name: string);
    procedure CreateElems;
    function SimpleCompare(Elems: TJclSimpleXMLElems; Index1, Index2: Integer): Integer;
  public
    constructor Create(AParent: TJclSimpleXMLElem);
    destructor Destroy; override;

    // Use notify to indicate to a list that the given element is removed
    // from the list so that it doesn't delete it as well as the one
    // that insert it in itself. This method is automatically called
    // by AddChild and AddChildFirst if the Container property of the
    // given element is set.
    procedure Notify(Value: TJclSimpleXMLElem; Operation: TOperation);

    function Add(const Name: string): TJclSimpleXMLElemClassic; overload;
    function Add(const Name, Value: string): TJclSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: Int64): TJclSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: Boolean): TJclSimpleXMLElemClassic; overload;
    function Add(const Name: string; Value: TStream; BufferSize: Integer = 0): TJclSimpleXMLElemClassic; overload;
    function Add(Value: TJclSimpleXMLElem): TJclSimpleXMLElem; overload;
    function AddFirst(Value: TJclSimpleXMLElem): TJclSimpleXMLElem; overload;
    function AddFirst(const Name: string): TJclSimpleXMLElemClassic; overload;
    function AddComment(const Name: string; const Value: string): TJclSimpleXMLElemComment;
    function AddCData(const Name: string; const Value: string): TJclSimpleXMLElemCData;
    function AddText(const Name: string; const Value: string): TJclSimpleXMLElemText;
    function Insert(Value: TJclSimpleXMLElem; Index: Integer): TJclSimpleXMLElem; overload;
    function Insert(const Name: string; Index: Integer): TJclSimpleXMLElemClassic; overload;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;
    function Remove(Value: TJclSimpleXMLElem): Integer;
    procedure Move(const CurIndex, NewIndex: Integer);
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: TJclSimpleXMLElemsEnumerator;
    {$ENDIF SUPPORTS_FOR_IN}
    function IndexOf(const Value: TJclSimpleXMLElem): Integer; overload;
    function IndexOf(const Name: string): Integer; overload;
    function Value(const Name: string; const Default: string = ''): string;
    function IntValue(const Name: string; const Default: Int64 = -1): Int64;
    function FloatValue(const Name: string; const Default: Extended = 0): Extended;
    function BoolValue(const Name: string; Default: Boolean = True): Boolean;
    procedure BinaryValue(const Name: string; Stream: TStream);
    procedure LoadFromStringStream(StringStream: TJclStringStream);
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = '');
    procedure Sort;
    procedure CustomSort(AFunction: TJclSimpleXMLElemCompare);
    property Parent: TJclSimpleXMLElem read FParent;
    property Item[const Index: Integer]: TJclSimpleXMLElem read GetItem; default;
    property ItemNamed[const Name: string]: TJclSimpleXMLElem read GetItemNamed;
    property Count: Integer read GetCount;
    property NamedElems[const Name: string]: TJclSimpleXMLNamedElems read GetNamedElems;
  end;

  {$TYPEINFO ON}
  TJclSimpleXMLElem = class(TJclSimpleXMLData)
  private
    FParent: TJclSimpleXMLElem;
    FSimpleXML: TJclSimpleXML;
    function GetHasItems: Boolean;
    function GetHasProperties: Boolean;
    function GetItemCount: Integer;
    function GetPropertyCount: Integer;
  protected
    FItems: TJclSimpleXMLElems;
    FProps: TJclSimpleXMLProps;
    function GetChildsCount: Integer;
    function GetProps: TJclSimpleXMLProps;
    procedure SetName(const Value: string); override;
    function GetItems: TJclSimpleXMLElems;
    procedure Error(const S: string);
    procedure FmtError(const S: string; const Args: array of const);
  public
    //constructor Create; overload;
    //constructor Create(const AName: string); overload;
    //constructor Create(const AName, AValue: string); overload;
    constructor Create(ASimpleXML: TJclSimpleXML); overload;
    destructor Destroy; override;
    procedure Assign(Value: TJclSimpleXMLElem); virtual;
    procedure Clear; virtual;
    procedure LoadFromStringStream(StringStream: TJclStringStream); virtual; abstract;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''); virtual;
      abstract;
    procedure LoadFromString(const Value: string);
    function SaveToString: string;
    procedure GetBinaryValue(Stream: TStream; BufferSize: Integer = 0);
    function GetChildIndex(const AChild: TJclSimpleXMLElem): Integer;
    function GetNamedIndex(const AChild: TJclSimpleXMLElem): Integer;

    property SimpleXML: TJclSimpleXML read FSimpleXML;
  published
    property Parent: TJclSimpleXMLElem read FParent;
    property ChildsCount: Integer read GetChildsCount;
    property HasItems: Boolean read GetHasItems;
    property HasProperties: Boolean read GetHasProperties;
    property ItemCount: Integer read GetItemCount;
    property PropertyCount: Integer read GetPropertyCount;
    property Items: TJclSimpleXMLElems read GetItems;
    property Properties: TJclSimpleXMLProps read GetProps;
  end;
  {$IFNDEF TYPEINFO_ON}
  {$TYPEINFO OFF}
  {$ENDIF ~TYPEINFO_ON}
  TJclSimpleXMLElemClass = class of TJclSimpleXMLElem;

  TJclSimpleXMLElemComment = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStringStream(StringStream: TJclStringStream); override;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''); override;
  end;

  TJclSimpleXMLElemClassic = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStringStream(StringStream: TJclStringStream); override;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''); override;
  end;

  TJclSimpleXMLElemCData = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStringStream(StringStream: TJclStringStream); override;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''); override;
  end;

  TJclSimpleXMLElemText = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStringStream(StringStream: TJclStringStream); override;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''); override;
  end;

  TJclSimpleXMLElemProcessingInstruction = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStringStream(StringStream: TJclStringStream); override;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''); override;
  end;

  TJclSimpleXMLElemHeader = class(TJclSimpleXMLElemProcessingInstruction)
  private
    function GetEncoding: string;
    function GetStandalone: Boolean;
    function GetVersion: string;
    procedure SetEncoding(const Value: string);
    procedure SetStandalone(const Value: Boolean);
    procedure SetVersion(const Value: string);
  public
    constructor Create; override;

    procedure LoadFromStringStream(StringStream: TJclStringStream); override;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''); override;
    property Version: string read GetVersion write SetVersion;
    property Standalone: Boolean read GetStandalone write SetStandalone;
    property Encoding: string read GetEncoding write SetEncoding;
  end;

  // for backward compatibility
  TJclSimpleXMLElemSheet = class(TJclSimpleXMLElemProcessingInstruction)
  end;

  // for backward compatibility
  TJclSimpleXMLElemMSOApplication = class(TJclSimpleXMLElemProcessingInstruction)
  end;

  TJclSimpleXMLElemDocType = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStringStream(StringStream: TJclStringStream); override;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''); override;
  end;

  TJclSimpleXMLOptions = set of (sxoAutoCreate, sxoAutoIndent, sxoAutoEncodeValue,
    sxoAutoEncodeEntity, sxoDoNotSaveProlog, sxoTrimPrecedingTextWhitespace,
    sxoTrimFollowingTextWhitespace, sxoKeepWhitespace, sxoDoNotSaveBOM, sxoCaseSensitive);
  TJclSimpleXMLEncodeEvent = procedure(Sender: TObject; var Value: string) of object;
  TJclSimpleXMLEncodeStreamEvent = procedure(Sender: TObject; InStream, OutStream: TStream) of object;

  TJclSimpleXML = class(TObject)
  protected
    FEncoding: TJclStringEncoding;
    FCodePage: Word;
    FFileName: TFileName;
    FOptions: TJclSimpleXMLOptions;
    FRoot: TJclSimpleXMLElemClassic;
    FOnTagParsed: TJclOnSimpleXMLParsed;
    FOnValue: TJclOnValueParsed;
    FOnLoadProg: TJclOnSimpleProgress;
    FOnSaveProg: TJclOnSimpleProgress;
    FProlog: TJclSimpleXMLElemsProlog;
    FSaveCount: Integer;
    FSaveCurrent: Integer;
    FIndentString: string;
    FBaseIndentString: string;
    FOnEncodeValue: TJclSimpleXMLEncodeEvent;
    FOnDecodeValue: TJclSimpleXMLEncodeEvent;
    FOnDecodeStream: TJclSimpleXMLEncodeStreamEvent;
    FOnEncodeStream: TJclSimpleXMLEncodeStreamEvent;
    procedure SetIndentString(const Value: string);
    procedure SetBaseIndentString(const Value: string);
    procedure SetRoot(const Value: TJclSimpleXMLElemClassic);
    procedure SetFileName(const Value: TFileName);
  protected
    procedure DoLoadProgress(const APosition, ATotal: Integer);
    procedure DoSaveProgress;
    procedure DoTagParsed(const AName: string);
    procedure DoValueParsed(const AName, AValue: string);
    procedure DoEncodeValue(var Value: string); virtual;
    procedure DoDecodeValue(var Value: string); virtual;
    procedure GetEncodingFromXMLHeader(var Encoding: TJclStringEncoding; var CodePage: Word);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromString(const Value: string);
    procedure LoadFromFile(const FileName: TFileName; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
    procedure LoadFromStream(Stream: TStream; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
    procedure LoadFromStringStream(StringStream: TJclStringStream);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
    procedure SaveToFile(const FileName: TFileName; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
    procedure SaveToStream(Stream: TStream; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
    procedure SaveToStringStream(StringStream: TJclStringStream);
    function SaveToString: string;
    function SaveToStringEncoding(Encoding: TJclStringEncoding; CodePage: Word = CP_ACP): string;
    property CodePage: Word read FCodePage;
    property Prolog: TJclSimpleXMLElemsProlog read FProlog write FProlog;
    property Root: TJclSimpleXMLElemClassic read FRoot write SetRoot;
    property XMLData: string read SaveToString write LoadFromString;
    property FileName: TFileName read FFileName write SetFileName;
    property IndentString: string read FIndentString write SetIndentString;
    property BaseIndentString: string read FBaseIndentString write SetBaseIndentString;
    property Options: TJclSimpleXMLOptions read FOptions write FOptions;
    property OnSaveProgress: TJclOnSimpleProgress read FOnSaveProg write FOnSaveProg;
    property OnLoadProgress: TJclOnSimpleProgress read FOnLoadProg write FOnLoadProg;
    property OnTagParsed: TJclOnSimpleXMLParsed read FOnTagParsed write FOnTagParsed;
    property OnValueParsed: TJclOnValueParsed read FOnValue write FOnValue;
    property OnEncodeValue: TJclSimpleXMLEncodeEvent read FOnEncodeValue write FOnEncodeValue;
    property OnDecodeValue: TJclSimpleXMLEncodeEvent read FOnDecodeValue write FOnDecodeValue;
    property OnEncodeStream: TJclSimpleXMLEncodeStreamEvent read FOnEncodeStream write FOnEncodeStream;
    property OnDecodeStream: TJclSimpleXMLEncodeStreamEvent read FOnDecodeStream write FOnDecodeStream;
  end;

  TXMLVariant = class(TInvokeableVariantType)
  public
    procedure Clear(var V: TVarData); override;
    function IsClear(const V: TVarData): Boolean; override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;

    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
  end;

procedure XMLCreateInto(var ADest: Variant; const AXML: TJclSimpleXMLElem);
function XMLCreate(const AXML: TJclSimpleXMLElem): Variant; overload;
function XMLCreate: Variant; overload;
function VarXML: TVarType;

// Encodes a string into an internal format:
// any character TAB,LF,CR,#32..#127 is preserved
// all other characters are converted to hex notation except
// for some special characters that are converted to XML entities
function SimpleXMLEncode(const S: string): string;
// Decodes a string encoded with SimpleXMLEncode:
// any character TAB,LF,CR,#32..#127 is preserved
// all other characters and substrings are converted from
// the special XML entities to characters or from hex to characters
// NB! Setting TrimBlanks to true will slow down the process considerably
procedure SimpleXMLDecode(var S: string; TrimBlanks: Boolean);

function XMLEncode(const S: string): string;
function XMLDecode(const S: string): string;

// Encodes special characters (', ", <, > and &) into XML entities (@apos;, &quot;, &lt;, &gt; and &amp;)
function EntityEncode(const S: string): string;
// Decodes XML entities (@apos;, &quot;, &lt;, &gt; and &amp;) into special characters (', ", <, > and &)
function EntityDecode(const S: string): string;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  System.Types,
  {$ENDIF HAS_UNITSCOPE}
  JclCharsets,
  JclStrings,
  JclUnicode,
  JclStringConversions,
  JclResources;

const
  cBufferSize = 8192;

var
  GlobalXMLVariant: TXMLVariant = nil;

  PreparedNibbleCharMapping: Boolean = False;
  NibbleCharMapping: array [Low(Char)..High(Char)] of Byte;

function XMLVariant: TXMLVariant;
begin
  if not Assigned(GlobalXMLVariant) then
    GlobalXMLVariant := TXMLVariant.Create;
  Result := GlobalXMLVariant;
end;

procedure AddEntity(var Res: string; var ResIndex, ResLen: Integer; const Entity: string);
var
  EntityIndex, EntityLen: Integer;
begin
  EntityLen := Length(Entity);
  if (ResIndex + EntityLen) > ResLen then
  begin
    if ResLen <= EntityLen then
      ResLen := ResLen * EntityLen
    else
      ResLen := ResLen * 2;
    SetLength(Res, ResLen);
  end;
  for EntityIndex := 1 to EntityLen do
  begin
    Res[ResIndex] := Entity[EntityIndex];
    Inc(ResIndex);
  end;
end;

function EntityEncode(const S: string): string;
var
  C: Char;
  SIndex, SLen, RIndex, RLen: Integer;
  Tmp: string;
begin
  SLen := Length(S);
  RLen := SLen;
  RIndex := 1;
  SetLength(Tmp, RLen);
  for SIndex := 1 to SLen do
  begin
    C := S[SIndex];
    case C of
      '"':
        AddEntity(Tmp, RIndex, RLen, '&quot;');
      '&':
        AddEntity(Tmp, RIndex, RLen, '&amp;');
      #39:
        AddEntity(Tmp, RIndex, RLen, '&apos;');
      '<':
        AddEntity(Tmp, RIndex, RLen, '&lt;');
      '>':
        AddEntity(Tmp, RIndex, RLen, '&gt;');
    else
      if RIndex > RLen then
      begin
        RLen := RLen * 2;
        SetLength(Tmp, RLen);
      end;
      Tmp[RIndex] := C;
      Inc(RIndex);
    end;
  end;
  if RIndex > 1 then
    SetLength(Tmp, RIndex - 1);

  Result := Tmp;
end;

function EntityDecode(const S: string): string;
var
  I, J, L: Integer;
begin
  Result := S;
  I := 1;
  J := 1;
  L := Length(Result);

  while I <= L do
  begin
    if Result[I] = '&' then
    begin
      if StrSame(Copy(Result, I, 5), '&amp;') then
      begin
        Result[J] := '&';
        Inc(J);
        Inc(I, 4);
      end
      else
      if StrSame(Copy(Result, I, 4), '&lt;') then
      begin
        Result[J] := '<';
        Inc(J);
        Inc(I, 3);
      end
      else
      if StrSame(Copy(Result, I, 4), '&gt;') then
      begin
        Result[J] := '>';
        Inc(J);
        Inc(I, 3);
      end
      else
      if StrSame(Copy(Result, I, 6), '&apos;') then
      begin
        Result[J] := #39;
        Inc(J);
        Inc(I, 5);
      end
      else
      if StrSame(Copy(Result, I, 6), '&quot;') then
      begin
        Result[J] := '"';
        Inc(J);
        Inc(I, 5);
      end
      else
      begin
        Result[J] := Result[I];
        Inc(J);
      end;
    end
    else
    begin
      Result[J] := Result[I];
      Inc(J);
    end;
    Inc(I);
  end;
  if J > 1 then
    SetLength(Result, J - 1)
  else
    SetLength(Result, 0);
end;

function SimpleXMLEncode(const S: string): string;
var
  C: Char;
  SIndex, SLen, RIndex, RLen: Integer;
  Tmp: string;
begin
  SLen := Length(S);
  RLen := SLen;
  RIndex := 1;
  SetLength(Tmp, RLen);
  for SIndex := 1 to SLen do
  begin
    C := S[SIndex];
    case C of
      '"':
        AddEntity(Tmp, RIndex, RLen, '&quot;');
      '&':
        AddEntity(Tmp, RIndex, RLen, '&amp;');
      #39:
        AddEntity(Tmp, RIndex, RLen, '&apos;');
      '<':
        AddEntity(Tmp, RIndex, RLen, '&lt;');
      '>':
        AddEntity(Tmp, RIndex, RLen, '&gt;');
      NativeNull..NativeBackspace, // NativeTab, NativeLineFeed
      NativeVerticalTab..NativeFormFeed, // NativeCarriageReturn
      NativeSo..NativeUs,
      Char(128)..Char(255):
        AddEntity(Tmp, RIndex, RLen, Format('&#x%.2x;', [Ord(C)]));
      {$IFDEF SUPPORTS_UNICODE}
      Char(256)..High(Char):
        AddEntity(Tmp, RIndex, RLen, Format('&#x%.4x;', [Ord(C)]));
      {$ENDIF SUPPORTS_UNICODE}
    else
      if RIndex > RLen then
      begin
        RLen := RLen * 2;
        SetLength(Tmp, RLen);
      end;
      Tmp[RIndex] := C;
      Inc(RIndex);
    end;
  end;
  if RIndex > 1 then
    SetLength(Tmp, RIndex - 1);

  Result := Tmp;
end;

procedure SimpleXMLDecode(var S: string; TrimBlanks: Boolean);
  procedure DecodeEntity(var S: string; StringLength: Cardinal;
    var ReadIndex, WriteIndex: Cardinal);
  const
    cHexPrefix: array [Boolean] of string = ('', '$');
  var
    I: Cardinal;
    Value: Integer;
    IsHex: Boolean;
  begin
    Inc(ReadIndex, 2);
    IsHex := (ReadIndex <= StringLength) and ((S[ReadIndex] = 'x') or (S[ReadIndex] = 'X'));
    Inc(ReadIndex, Ord(IsHex));
    I := ReadIndex;
    while ReadIndex <= StringLength do
    begin
      if S[ReadIndex] = ';' then
      begin
        Value := StrToIntDef(cHexPrefix[IsHex] + Copy(S, I, ReadIndex - I), -1); // no characters are less than 0
        if Value >= 0 then
          S[WriteIndex] := Chr(Value)
        else
          ReadIndex := I - (2 + Cardinal(IsHex)); // reset to start
        Exit;
      end;
      Inc(ReadIndex);
    end;
    ReadIndex := I - (2 + Cardinal(IsHex)); // reset to start
  end;

  procedure SkipBlanks(var S: string; StringLength: Cardinal; var ReadIndex: Cardinal);
  begin
    while ReadIndex < StringLength do
    begin
      if S[ReadIndex] = NativeCarriageReturn then
        S[ReadIndex] := NativeLineFeed
      else
      if S[ReadIndex + 1] = NativeCarriageReturn then
        S[ReadIndex + 1] := NativeLineFeed;
      if (S[ReadIndex] < #33) and (S[ReadIndex] = S[ReadIndex + 1]) then
        Inc(ReadIndex)
      else
        Exit;
    end;
  end;

var
  StringLength, ReadIndex, WriteIndex: Cardinal;
begin
  // NB! This procedure replaces the text inplace to speed up the conversion. This
  // works because when decoding, the string can only become shorter. This is
  // accomplished by keeping track of the current read and write points.
  // In addition, the original string length is read only once and passed to the
  // inner procedures to speed up conversion as much as possible
  ReadIndex := 1;
  WriteIndex := 1;
  StringLength := Length(S);
  while ReadIndex <= StringLength do
  begin
    // this call lowers conversion speed by ~30%, ie 21MB/sec -> 15MB/sec (repeated tests, various inputs)
    if TrimBlanks then
      SkipBlanks(S, StringLength, ReadIndex);
    if S[ReadIndex] = '&' then
    begin
      if (ReadIndex < StringLength) and (S[ReadIndex + 1] = '#') then
      begin
        DecodeEntity(S, StringLength, ReadIndex, WriteIndex);
        Inc(WriteIndex);
      end
      else
      if StrSame(Copy(S, ReadIndex, 5), '&amp;') then
      begin
        S[WriteIndex] := '&';
        Inc(WriteIndex);
        Inc(ReadIndex, 4);
      end
      else
      if StrSame(Copy(S, ReadIndex, 4), '&lt;') then
      begin
        S[WriteIndex] := '<';
        Inc(WriteIndex);
        Inc(ReadIndex, 3);
      end
      else
      if StrSame(Copy(S, ReadIndex, 4), '&gt;') then
      begin
        S[WriteIndex] := '>';
        Inc(WriteIndex);
        Inc(ReadIndex, 3);
      end
      else
      if StrSame(Copy(S, ReadIndex, 6), '&apos;') then
      begin
        S[WriteIndex] := #39;
        Inc(WriteIndex);
        Inc(ReadIndex, 5);
      end
      else
      if StrSame(Copy(S, ReadIndex, 6), '&quot;') then
      begin
        S[WriteIndex] := '"';
        Inc(WriteIndex);
        Inc(ReadIndex, 5);
      end
      else
      begin
        S[WriteIndex] := S[ReadIndex];
        Inc(WriteIndex);
      end;
    end
    else
    begin
      S[WriteIndex] := S[ReadIndex];
      Inc(WriteIndex);
    end;
    Inc(ReadIndex);
  end;
  if WriteIndex > 0 then
    SetLength(S, WriteIndex - 1)
  else
    SetLength(S, 0);
    // this call lowers conversion speed by ~65%, ie 21MB/sec -> 7MB/sec (repeated tests, various inputs)
//  if TrimBlanks then
//    S := AdjustLineBreaks(S);
end;

function XMLEncode(const S: string): string;
begin
  Result := SimpleXMLEncode(S);
end;

function XMLDecode(const S: string): string;
begin
  Result := S;
  SimpleXMLDecode(Result, False);
end;

//=== { TJclSimpleItem } =====================================================

procedure TJclSimpleItem.SetName(const Value: string);
begin
  FName := Value;
end;

//=== { TJclSimpleItemHashedList } ===========================================

procedure TJclSimpleItemHashedList.Clear;
begin
  InvalidateHash;
  inherited Clear;
end;

constructor TJclSimpleItemHashedList.Create(ACaseSensitive: Boolean);
begin
  inherited Create(True);
  FCaseSensitive := ACaseSensitive;
end;

destructor TJclSimpleItemHashedList.Destroy;
begin
  FreeAndNil(FNameHash);
  inherited Destroy;
end;

function TJclSimpleItemHashedList.Add(Item: TJclSimpleItem): Integer;
begin
  Result := inherited Add(Item);
  if FNameHash <> nil then
  begin
    if FCaseSensitive then
      FNameHash.Add(Item.Name, Result)
    else
      FNameHash.Add(UpperCase(Item.Name), Result);
  end;
end;

function TJclSimpleItemHashedList.Extract(Item: TJclSimpleItem): TJclSimpleItem;
begin
  Result := TJclSimpleItem(inherited Extract(Item));
  InvalidateHash;
end;

function TJclSimpleItemHashedList.GetSimpleItem(Index: Integer): TJclSimpleItem;
begin
  Result := TJclSimpleItem(GetItem(Index));
end;

function TJclSimpleItemHashedList.GetSimpleItemByName(const Name: string): TJclSimpleItem;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := TJclSimpleItem(Items[I])
  else
    Result := nil;
end;

function TJclSimpleItemHashedList.IndexOfSimpleItem(Item: TJclSimpleItem): Integer;
begin
  Result := IndexOf(Item);
end;

function TJclSimpleItemHashedList.IndexOfName(const Name: string): Integer;
var
  I: Integer;
begin
  if FCaseSensitive then
  begin
    if FNameHash = nil then
    begin
      FNameHash := TStringHash.Create(8);
      for I := 0 to Count - 1 do
        FNameHash.Add(TJclSimpleData(Items[I]).Name, I);
    end;
    Result := FNameHash.ValueOf(Name);
  end
  else
  begin
    if FNameHash = nil then
    begin
      FNameHash := TStringHash.Create(8);
      for I := 0 to Count - 1 do
        FNameHash.Add(UpperCase(TJclSimpleData(Items[I]).Name), I);
    end;
    Result := FNameHash.ValueOf(UpperCase(Name));
  end;
end;

procedure TJclSimpleItemHashedList.Insert(Index: Integer; Item: TJclSimpleItem);
begin
  InvalidateHash;
  inherited Insert(Index, Item);
end;

procedure TJclSimpleItemHashedList.InvalidateHash;
begin
  FreeAndNil(FNameHash);
end;

procedure TJclSimpleItemHashedList.Move(CurIndex, NewIndex: Integer);
begin
  InvalidateHash;
  inherited Move(CurIndex, NewIndex);
end;

procedure TJclSimpleItemHashedList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and (FNameHash <> nil) then
  begin
//    Mantis 0006062 Hotfix
//    if FCaseSensitive then
//      FNameHash.Remove(TJclSimpleItem(Ptr).Name)
//    else
//      FNameHash.Remove(UpperCase(TJclSimpleItem(Ptr).Name));
    InvalidateHash;
  end;
  inherited Notify(Ptr, Action);
end;

procedure TJclSimpleItemHashedList.SetCaseSensitive(const Value: Boolean);
begin
  if FCaseSensitive <> Value then
  begin
    InvalidateHash;
    FCaseSensitive := Value;
  end;
end;

//=== { TJclSimpleData } =====================================================

constructor TJclSimpleData.Create;
begin
  inherited Create;
end;

constructor TJclSimpleData.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

constructor TJclSimpleData.Create(const AName, AValue: string);
begin
  inherited Create;
  FName := AName;
  FValue := AValue;
end;

function TJclSimpleData.GetAnsiValue: AnsiString;
begin
  Result := AnsiString(Value);
end;

function TJclSimpleData.GetBoolValue: Boolean;
begin
  Result := StrToBoolDef(Value, False);
end;

function TJclSimpleData.GetFloatValue: Extended;
begin
  Result := 0.0;
  if not TryStrToFloat(Value, Result) then
    Result := 0.0;
end;

function TJclSimpleData.GetIntValue: Int64;
begin
  Result := StrToInt64Def(Value, -1);
end;

procedure TJclSimpleData.SetAnsiValue(const Value: AnsiString);
begin
  Self.Value := string(Value);
end;

procedure TJclSimpleData.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TJclSimpleData.SetFloatValue(const Value: Extended);
begin
  FValue := FloatToStr(Value);
end;

procedure TJclSimpleData.SetIntValue(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

//=== { TJclSimpleXMLData } ==================================================

function TJclSimpleXMLData.FullName: string;
begin
  if NameSpace <> '' then
    Result := NameSpace + ':' + Name
  else
    Result := Name;
end;

//=== { TJclSimpleXML } ======================================================

constructor TJclSimpleXML.Create;
begin
  inherited Create;
  FRoot := TJclSimpleXMLElemClassic.Create(Self);
  FProlog := TJclSimpleXMLElemsProlog.Create(Self);
  FOptions := [sxoAutoIndent, sxoAutoEncodeValue, sxoAutoEncodeEntity];
  FIndentString := '  ';
end;

destructor TJclSimpleXML.Destroy;
begin
  FreeAndNil(FRoot);
  FreeAndNil(FProlog);
  inherited Destroy;
end;

procedure TJclSimpleXML.DoDecodeValue(var Value: string);
begin
  if sxoAutoEncodeValue in Options then
    SimpleXMLDecode(Value, False)
  else
  if sxoAutoEncodeEntity in Options then
    Value := EntityDecode(Value);
  if Assigned(FOnDecodeValue) then
    FOnDecodeValue(Self, Value);
end;

procedure TJclSimpleXML.DoEncodeValue(var Value: string);
begin
  if Assigned(FOnEncodeValue) then
    FOnEncodeValue(Self, Value);
  if sxoAutoEncodeValue in Options then
    Value := SimpleXMLEncode(Value)
  else
  if sxoAutoEncodeEntity in Options then
    Value := EntityEncode(Value);
end;

procedure TJclSimpleXML.DoLoadProgress(const APosition, ATotal: Integer);
begin
  if Assigned(FOnLoadProg) then
    FOnLoadProg(Self, APosition, ATotal);
end;

procedure TJclSimpleXML.DoSaveProgress;
begin
  if Assigned(FOnSaveProg) then
  begin
    Inc(FSaveCurrent);
    FOnSaveProg(Self, FSaveCurrent, FSaveCount);
  end;
end;

procedure TJclSimpleXML.DoTagParsed(const AName: string);
begin
  if Assigned(FOnTagParsed) then
    FOnTagParsed(Self, AName);
end;

procedure TJclSimpleXML.DoValueParsed(const AName, AValue: string);
begin
  if Assigned(FOnValue) then
    FOnValue(Self, AName, AValue);
end;

procedure TJclSimpleXML.LoadFromFile(const FileName: TFileName; Encoding: TJclStringEncoding; CodePage: Word);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(FileName);
    LoadFromStream(Stream, Encoding, CodePage);
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXML.LoadFromResourceName(Instance: THandle; const ResName: string;
  Encoding: TJclStringEncoding; CodePage: Word);
{$IFNDEF MSWINDOWS}
const
  RT_RCDATA = PChar(10);
{$ENDIF !MSWINDOWS}
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
  try
    LoadFromStream(Stream, Encoding, CodePage);
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXML.LoadFromStream(Stream: TStream; Encoding: TJclStringEncoding; CodePage: Word);
var
  AOutStream: TStream;
  AStringStream: TJclStringStream;
  DoFree: Boolean;
begin
  FRoot.Clear;
  FProlog.Clear;
  AOutStream := nil;
  DoFree := False;
  try
    if Assigned(FOnDecodeStream) then
    begin
      AOutStream := TMemoryStream.Create;
      DoFree := True;
      FOnDecodeStream(Self, Stream, AOutStream);
      AOutStream.Seek(0, soBeginning);
    end
    else
      AOutStream := Stream;

    case Encoding of
      seAnsi:
        begin
          AStringStream := TJclAnsiStream.Create(AOutStream, False);
          TJclAnsiStream(AStringStream).CodePage := CodePage;
        end;
      seUTF8:
        AStringStream := TJclUTF8Stream.Create(AOutStream, False);
      seUTF16:
        AStringStream := TJclUTF16Stream.Create(AOutStream, False);
    else
      AStringStream := TJclAutoStream.Create(AOutStream, False);
      if CodePage <> CP_ACP then
        TJclAutoStream(AStringStream).CodePage := CodePage;
    end;
    try
      AStringStream.SkipBOM;

      LoadFromStringStream(AStringStream);

      // save codepage and encoding for future saves
      if AStringStream is TJclAutoStream then
      begin
        FCodePage := TJclAutoStream(AStringStream).CodePage;
        FEncoding := TJclAutoStream(AStringStream).Encoding;
      end
      else
      if AStringStream is TJclAnsiStream then
      begin
        FCodePage := TJclAnsiStream(AStringStream).CodePage;
        FEncoding := Encoding;
      end
      else
      begin
        FCodePage := CodePage;
        FEncoding := Encoding;
      end;
    finally
      AStringStream.Free;
    end;
  finally
    if DoFree then
      AOutStream.Free;
  end;
end;

procedure TJclSimpleXML.LoadFromStringStream(StringStream: TJclStringStream);
var
  BufferSize: Integer;
begin
  if Assigned(FOnLoadProg) then
    FOnLoadProg(Self, StringStream.Stream.Position, StringStream.Stream.Size);

  BufferSize := StringStream.BufferSize;
  StringStream.BufferSize := 1;

  // Read doctype and so on
  FProlog.LoadFromStringStream(StringStream);

  StringStream.BufferSize := BufferSize;

  // Read elements
  FRoot.LoadFromStringStream(StringStream);

  if Assigned(FOnLoadProg) then
    FOnLoadProg(Self, StringStream.Stream.Position, StringStream.Stream.Size);
end;

procedure TJclSimpleXML.LoadFromString(const Value: string);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(Value {$IFDEF SUPPORTS_UNICODE}, TEncoding.Unicode{$ENDIF});
  try
    LoadFromStream(Stream {$IFDEF SUPPORTS_UNICODE}, seUTF16, CP_UTF16LE{$ENDIF});
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXML.GetEncodingFromXMLHeader(var Encoding: TJclStringEncoding; var CodePage: Word);
var
  XMLHeader: TJclSimpleXMLElemHeader;
  I: Integer;
begin
  XMLHeader := nil;
  for I := 0 to Prolog.Count - 1 do
    if Prolog.Item[I] is TJclSimpleXMLElemHeader then
    begin
      XMLHeader := TJclSimpleXMLElemHeader(Prolog.Item[I]);
      Break;
    end;
  if Assigned(XMLHeader) then
  begin
    CodePage := CodePageFromCharsetName(XMLHeader.Encoding);
    case CodePage of
      CP_UTF8:
        Encoding := seUTF8;
      CP_UTF16LE:
        Encoding := seUTF16;
    else
      Encoding := seAnsi;
    end;
  end
  else
  begin
    // restore from previous load
    Encoding := FEncoding;
    CodePage := FCodePage;
  end;
end;

procedure TJclSimpleXML.SaveToFile(const FileName: TFileName; Encoding: TJclStringEncoding; CodePage: Word);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    SaveToStream(Stream, Encoding, CodePage);
    Stream.SaveToFile(FileName);
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXML.SaveToStream(Stream: TStream; Encoding: TJclStringEncoding; CodePage: Word);
var
  AOutStream: TStream;
  AStringStream: TJclStringStream;
  DoFree: Boolean;
begin
  if Assigned(FOnEncodeStream) then
  begin
    AOutStream := TMemoryStream.Create;
    DoFree := True;
  end
  else
  begin
    AOutStream := Stream;
    DoFree := False;
  end;
  try
    if Encoding = seAuto then
      GetEncodingFromXMLHeader(Encoding, CodePage);

    case Encoding of
      seUTF8:
        begin
          AStringStream := TJclUTF8Stream.Create(AOutStream, False);
          FCodePage := CP_UTF8;
        end;
      seUTF16:
        begin
          AStringStream := TJclUTF16Stream.Create(AOutStream, False);
          FCodePage := CP_UTF16LE;
        end
    else
      AStringStream := TJclAnsiStream.Create(AOutStream);
      TJclAnsiStream(AStringStream).CodePage := CodePage;
    end;
    try
      if not (sxoDoNotSaveBOM in Options) then
        AStringStream.WriteBOM;
      SaveToStringStream(AStringStream);
      AStringStream.Flush;
    finally
      AStringStream.Free;
    end;
    if Assigned(FOnEncodeStream) then
    begin
      AOutStream.Seek(0, soBeginning);
      FOnEncodeStream(Self, AOutStream, Stream);
    end;
  finally
    if DoFree then
      AOutStream.Free;
  end;
end;

procedure TJclSimpleXML.SaveToStringStream(StringStream: TJclStringStream);
var
  lCount: Integer;
begin
  lCount := Root.ChildsCount + Prolog.Count;
  FSaveCount := lCount;
  FSaveCurrent := 0;

  if Assigned(FOnSaveProg) then
    FOnSaveProg(Self, 0, lCount);

  if not (sxoDoNotSaveProlog in FOptions) then
    Prolog.SaveToStringStream(StringStream);

  Root.SaveToStringStream(StringStream, BaseIndentString);

  if Assigned(FOnSaveProg) then
    FOnSaveProg(Self, lCount, lCount);
end;

function TJclSimpleXML.SaveToString: string;
begin
  Result := SaveToStringEncoding(seAuto, CP_ACP);
end;

function TJclSimpleXML.SaveToStringEncoding(Encoding: TJclStringEncoding; CodePage: Word): string;
var
  Stream: TStringStream;
begin
  {$IFDEF SUPPORTS_UNICODE}
  // Use the same logic for seAuto as in SaveToStream for creating the TStringStream.
  // Otherwise a Unicode-TStringStream is written to from a TJclAnsiStream proxy.
  if Encoding = seAuto then
    GetEncodingFromXMLHeader(Encoding, CodePage);

  case Encoding of
    seAnsi:
      Stream := TStringStream.Create('', TEncoding.{$IFDEF COMPILER16_UP}ANSI{$ELSE}Default{$ENDIF});
    seUTF8:
      Stream := TStringStream.Create('', TEncoding.UTF8);
  else
    //seUTF16:
    Stream := TStringStream.Create('', TEncoding.Unicode);
  end;
  {$ELSE ~SUPPORTS_UNICODE}
  Stream := TStringStream.Create('');
  {$ENDIF ~SUPPORTS_UNICODE}
  try
    SaveToStream(Stream, Encoding, CodePage);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXML.SetBaseIndentString(const Value: string);
begin
  // test if the new value is only made of spaces or tabs
  if not StrContainsChars(Value, CharIsWhiteSpace, True) then
    Exit;

  FBaseIndentString := Value;
end;

procedure TJclSimpleXML.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  LoadFromFile(Value);
end;

//=== { TJclSimpleXMLElem } ==================================================

procedure TJclSimpleXMLElem.Assign(Value: TJclSimpleXMLElem);
var
  Elems: TJclSimpleXMLElem;
  SrcElem, DestElem: TJclSimpleXMLElem;
  I: Integer;
  SrcProps, DestProps: TJclSimpleXMLProps;
  SrcProp: TJclSimpleXMLProp;
  SrcElems, DestElems: TJclSimpleXMLElems;
begin
  Clear;
  if Value = nil then
    Exit;
  Elems := TJclSimpleXMLElem(Value);
  Name := Elems.Name;
  Self.Value := Elems.Value;
  SrcProps := Elems.FProps;
  if Assigned(SrcProps) then
  begin
    DestProps := Properties;
    for I := 0 to SrcProps.Count - 1 do
    begin
      SrcProp := SrcProps.Item[I];
      DestProps.Add(SrcProp.Name, SrcProp.Value);
    end;
  end;

  SrcElems := Elems.FItems;
  if Assigned(SrcElems) then
  begin
    DestElems := Items;
    for I := 0 to SrcElems.Count - 1 do
    begin
      // Create from the class type, so that the virtual constructor is called
      // creating an element of the correct class type.
      SrcElem := SrcElems.Item[I];
      DestElem := TJclSimpleXMLElemClass(SrcElem.ClassType).Create(SrcElem.Name, SrcElem.Value);
      DestElem.Assign(SrcElem);
      DestElems.Add(DestElem);
    end;
  end;
end;

procedure TJclSimpleXMLElem.Clear;
begin
  if FItems <> nil then
    FItems.Clear;
  if FProps <> nil then
    FProps.Clear;
end;

constructor TJclSimpleXMLElem.Create(ASimpleXML: TJclSimpleXML);
begin
  Create;
  FSimpleXML := ASimpleXML;
end;

destructor TJclSimpleXMLElem.Destroy;
begin
  FSimpleXML := nil;
  FParent := nil;
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(FProps);
  inherited Destroy;
end;

procedure TJclSimpleXMLElem.Error(const S: string);
begin
  raise EJclSimpleXMLError.Create(S);
end;

procedure TJclSimpleXMLElem.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S, Args));
end;

procedure TJclSimpleXMLElem.GetBinaryValue(Stream: TStream; BufferSize: Integer = 0);
var
  I, J, ValueLength, RequiredStreamSize: Integer;
  Buf: array of Byte;
  N1, N2: Byte;

  function NibbleCharToNibble(const AChar: Char): Byte;
  begin
    case AChar of
      '0': Result := 0;
      '1': Result := 1;
      '2': Result := 2;
      '3': Result := 3;
      '4': Result := 4;
      '5': Result := 5;
      '6': Result := 6;
      '7': Result := 7;
      '8': Result := 8;
      '9': Result := 9;
      'a', 'A': Result := 10;
      'b', 'B': Result := 11;
      'c', 'C': Result := 12;
      'd', 'D': Result := 13;
      'e', 'E': Result := 14;
      'f', 'F': Result := 15;
      else
        Result := 16;
    end;
  end;

  procedure PrepareNibbleCharMapping;
  var
    C: Char;
  begin
    if not PreparedNibbleCharMapping then
    begin
      for C := Low(Char) to High(Char) do
        NibbleCharMapping[C] := NibbleCharToNibble(C);
      PreparedNibbleCharMapping := True;
    end;
  end;

var
  CurrentStreamPosition: Integer;
begin
  if BufferSize = 0 then
    BufferSize := cBufferSize;

  SetLength(Buf, BufferSize);
  PrepareNibbleCharMapping;
  I := 1;
  J := 0;
  ValueLength := Length(Value);
  RequiredStreamSize := Stream.Position + ValueLength div 2;
  if Stream.Size < RequiredStreamSize then
  begin
    CurrentStreamPosition := Stream.Position;
    Stream.Size := RequiredStreamSize;
    Stream.Seek(CurrentStreamPosition, soBeginning);
  end;
  while I < ValueLength do
  begin
    //faster replacement for St := '$' + Value[I] + Value[I + 1]; Buf[J] := StrToIntDef(St, 0);
    N1 := NibbleCharMapping[Value[I]];
    N2 := NibbleCharMapping[Value[I + 1]];
    Inc(I, 2);
    if (N1 > 15) or (N2 > 15) then
      Buf[J] := 0
    else
      Buf[J] := (N1 shl 4) or N2;
    Inc(J);
    if J = Length(Buf) - 1 then //Buffered write to speed up the process a little
    begin
      Stream.Write(Buf[0], J);
      J := 0;
    end;
  end;
  Stream.Write(Buf[0], J);
end;

function TJclSimpleXMLElem.GetChildIndex(const AChild: TJclSimpleXMLElem): Integer;
begin
  if FItems = nil then
    Result := -1
  else
    Result := FItems.FElems.IndexOfSimpleItem(AChild);
end;

function TJclSimpleXMLElem.GetChildsCount: Integer;
var
  I: Integer;
begin
  Result := 1;
  if FItems <> nil then
    for I := 0 to FItems.Count - 1 do
      Result := Result + FItems[I].ChildsCount;
end;

function TJclSimpleXMLElem.GetHasItems: Boolean;
begin
  Result := Assigned(FItems) and (FItems.Count > 0);
end;

function TJclSimpleXMLElem.GetHasProperties: Boolean;
begin
  Result := Assigned(FProps) and (FProps.Count > 0);
end;

function TJclSimpleXMLElem.GetItemCount: Integer;
begin
  Result := 0;
  if Assigned(FItems) then
    Result := FItems.Count;
end;

function TJclSimpleXMLElem.GetItems: TJclSimpleXMLElems;
begin
  if FItems = nil then
    FItems := TJclSimpleXMLElems.Create(Self);
  Result := FItems;
end;

function TJclSimpleXMLElem.GetNamedIndex(const AChild: TJclSimpleXMLElem): Integer;
begin
  Result := Items.NamedElems[AChild.Name].IndexOf(AChild);
end;

function TJclSimpleXMLElem.GetPropertyCount: Integer;
begin
  Result := 0;
  if Assigned(FProps) then
    Result := FProps.Count;
end;

function TJclSimpleXMLElem.GetProps: TJclSimpleXMLProps;
begin
  if FProps = nil then
    FProps := TJclSimpleXMLProps.Create(Self);
  Result := FProps;
end;

procedure TJclSimpleXMLElem.LoadFromString(const Value: string);
var
  Stream: TJclStringStream;
  StrStream: TStringStream;
begin
  StrStream := TStringStream.Create(Value);
  try
    Stream := TJclAutoStream.Create(StrStream);
    try
      LoadFromStringStream(Stream);
    finally
      Stream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

function TJclSimpleXMLElem.SaveToString: string;
var
  Stream: TJclStringStream;
  StrStream: TStringStream;
begin
  StrStream := TStringStream.Create('');
  try
    Stream := TJclAutoStream.Create(StrStream);
    try
      SaveToStringStream(Stream);
      Stream.Flush;
    finally
      Stream.Free;
    end;
    Result := StrStream.DataString;
  finally
    StrStream.Free;
  end;
end;

procedure TJclSimpleXMLElem.SetName(const Value: string);
begin
  if (Value <> Name) and (Value <> '') then
  begin
    if (Parent <> nil) and (Name <> '') then
      Parent.Items.DoItemRename(Self, Value);
    inherited SetName(Value);
  end;
end;

//=== { TJclSimpleXMLNamedElemsEnumerator } ==================================

{$IFDEF SUPPORTS_FOR_IN}
constructor TJclSimpleXMLNamedElemsEnumerator.Create(AList: TJclSimpleXMLNamedElems);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

function TJclSimpleXMLNamedElemsEnumerator.GetCurrent: TJclSimpleXMLElem;
begin
  Result := FList[FIndex];
end;

function TJclSimpleXMLNamedElemsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;
{$ENDIF SUPPORTS_FOR_IN}

//=== { TJclSimpleXMLNamedElems } ============================================

constructor TJclSimpleXMLNamedElems.Create(AElems: TJclSimpleXMLElems; const AName: string);
begin
  inherited Create;
  FElems := AElems;
  FName := AName;
  FItems := TList.Create;
end;

destructor TJclSimpleXMLNamedElems.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJclSimpleXMLNamedElems.Add(const Value: Int64): TJclSimpleXMLElemClassic;
begin
  Result := Elems.Add(Name, Value);
end;

function TJclSimpleXMLNamedElems.Add(Value: TStream): TJclSimpleXMLElemClassic;
begin
  Result := Elems.Add(Name, Value);
end;

function TJclSimpleXMLNamedElems.Add(const Value: Boolean): TJclSimpleXMLElemClassic;
begin
  Result := Elems.Add(Name, Value);
end;

function TJclSimpleXMLNamedElems.Add: TJclSimpleXMLElemClassic;
begin
  Result := Elems.Add(Name);
end;

function TJclSimpleXMLNamedElems.Add(const Value: string): TJclSimpleXMLElemClassic;
begin
  Result := Elems.Add(Name, Value);
end;

function TJclSimpleXMLNamedElems.AddCData(const Value: string): TJclSimpleXMLElemCData;
begin
  Result := Elems.AddCData(Name, Value);
end;

function TJclSimpleXMLNamedElems.AddComment(const Value: string): TJclSimpleXMLElemComment;
begin
  Result := Elems.AddComment(Name, Value);
end;

function TJclSimpleXMLNamedElems.AddFirst: TJclSimpleXMLElemClassic;
begin
  Result := Elems.AddFirst(Name);
end;

function TJclSimpleXMLNamedElems.AddText(const Value: string): TJclSimpleXMLElemText;
begin
  Result := Elems.AddText(Name, Value);
end;

procedure TJclSimpleXMLNamedElems.Clear;
var
  Index: Integer;
begin
  for Index := FItems.Count - 1 downto 0 do
    Elems.Remove(TJclSimpleXMLElem(FItems.Items[Index]));
end;

procedure TJclSimpleXMLNamedElems.Delete(const Index: Integer);
begin
  if (Index >= 0) and (Index < FItems.Count) then
    Elems.Remove(TJclSimpleXMLElem(FItems.Items[Index]));
end;

function TJclSimpleXMLNamedElems.GetCount: Integer;
begin
  Result := FItems.Count;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSimpleXMLNamedElems.GetEnumerator: TJclSimpleXMLNamedElemsEnumerator;
begin
  Result := TJclSimpleXMLNamedElemsEnumerator.Create(Self);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSimpleXMLNamedElems.GetItem(const Index: Integer): TJclSimpleXMLElem;
begin
  if (Index >= 0) then
  begin
    While (Index >= Count) do
      if Assigned(Elems.Parent) and Assigned(Elems.Parent.SimpleXML) and
         (sxoAutoCreate in Elems.Parent.SimpleXML.Options) then
        Add
      else
        break;
    if Index < Count then
      Result := TJclSimpleXMLElem(FItems.Items[Index])
    else
      Result := nil;
  end
  else
    Result := nil;
end;

function TJclSimpleXMLNamedElems.IndexOf(const Value: TJclSimpleXMLElem): Integer;
begin
  Result := FItems.IndexOf(Value);
end;

function TJclSimpleXMLNamedElems.IndexOf(const Value: string): Integer;
var
  Index: Integer;
  NewItem: TJclSimpleXMLElem;
begin
  Result := -1;
  for Index := 0 to FItems.Count - 1 do
    if TJclSimpleXMLElem(FItems.Items[Index]).Value = Value then
  begin
    Result := Index;
    Break;
  end;
  if (Result = -1) and (sxoAutoCreate in Elems.Parent.SimpleXML.Options) then
  begin
    NewItem := Elems.Add(Name, Value);
    Result := FItems.IndexOf(NewItem);
  end;
end;

procedure TJclSimpleXMLNamedElems.Move(const CurIndex, NewIndex: Integer);
var
  ElemsCurIndex, ElemsNewIndex: Integer;
begin
  ElemsCurIndex := Elems.IndexOf(TJclSimpleXMLElem(FItems.Items[CurIndex]));
  ElemsNewIndex := Elems.IndexOf(TJclSimpleXMLElem(FItems.Items[NewIndex]));
  Elems.Move(ElemsCurIndex, ElemsNewIndex);
  FItems.Move(CurIndex, NewIndex);
end;

procedure TJclSimpleXMLNamedElems.SetName(const Value: string);
begin
  raise EJclSimpleXMLError.CreateRes(@SReadOnlyProperty);
end;

//=== { TJclSimpleXMLElemsEnumerator } =======================================

{$IFDEF SUPPORTS_FOR_IN}
constructor TJclSimpleXMLElemsEnumerator.Create(AList: TJclSimpleXMLElems);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

function TJclSimpleXMLElemsEnumerator.GetCurrent: TJclSimpleXMLElem;
begin
  Result := FList[FIndex];
end;

function TJclSimpleXMLElemsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;
{$ENDIF SUPPORTS_FOR_IN}

//=== { TJclSimpleXMLElems } =================================================

function TJclSimpleXMLElems.Add(const Name: string): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Name);
  AddChild(Result);
end;

function TJclSimpleXMLElems.Add(const Name, Value: string): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Name, Value);
  AddChild(Result);
end;

function TJclSimpleXMLElems.Add(const Name: string; const Value: Int64): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Name, IntToStr(Value));
  AddChild(Result);
end;

function TJclSimpleXMLElems.Add(Value: TJclSimpleXMLElem): TJclSimpleXMLElem;
begin
  if Value <> nil then
    AddChild(Value);
  Result := Value;
end;

function TJclSimpleXMLElems.Add(const Name: string; const Value: Boolean): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Name, BoolToStr(Value));
  AddChild(Result);
end;

function TJclSimpleXMLElems.Add(const Name: string; Value: TStream; BufferSize: Integer): TJclSimpleXMLElemClassic;
var
  Stream: TStringStream;
  Buf: array of Byte;
  St: string;
  I, Count: Integer;
begin
  Stream := TStringStream.Create('');
  try
    if BufferSize = 0 then
      BufferSize := cBufferSize;

    SetLength(Buf, BufferSize);
    Buf[0] := 0;
    repeat
      Count := Value.Read(Buf[0], Length(Buf));
      St := '';
      for I := 0 to Count - 1 do
        St := St + IntToHex(Buf[I], 2);
      Stream.WriteString(St);
    until Count = 0;
    Result := TJclSimpleXMLElemClassic.Create(Name, Stream.DataString);
    AddChild(Result);
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXMLElems.AddChild(const Value: TJclSimpleXMLElem);
var
  NamedIndex: Integer;
begin
  CreateElems;

  // If there already is a container, notify it to remove the element
  if Assigned(Value.Parent) then
    Value.Parent.Items.Notify(Value, opRemove);

  FElems.Add(Value);

  if FNamedElems <> nil then
  begin
    NamedIndex := FNamedElems.IndexOfName(Value.Name);
    if NamedIndex >= 0 then
      TJclSimpleXMLNamedElems(FNamedElems.SimpleItems[NamedIndex]).FItems.Add(Value);
  end;

  Notify(Value, opInsert);
end;

procedure TJclSimpleXMLElems.AddChildFirst(const Value: TJclSimpleXMLElem);
var
  NamedIndex: Integer;
begin
  CreateElems;

  // If there already is a container, notify it to remove the element
  if Assigned(Value.Parent) then
    Value.Parent.Items.Notify(Value, opRemove);

  FElems.Insert(0, Value);

  if FNamedElems <> nil then
  begin
    NamedIndex := FNamedElems.IndexOfName(Value.Name);
    if NamedIndex >= 0 then
      TJclSimpleXMLNamedElems(FNamedElems.SimpleItems[NamedIndex]).FItems.Insert(0, Value);
  end;

  Notify(Value, opInsert);
end;

function TJclSimpleXMLElems.AddFirst(const Name: string): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Name);
  AddChildFirst(Result);
end;

function TJclSimpleXMLElems.AddFirst(Value: TJclSimpleXMLElem): TJclSimpleXMLElem;
begin
  if Value <> nil then
    AddChildFirst(Value);
  Result := Value;
end;

function TJclSimpleXMLElems.AddComment(const Name,
  Value: string): TJclSimpleXMLElemComment;
begin
  Result := TJclSimpleXMLElemComment.Create(Name, Value);
  AddChild(Result);
end;

function TJclSimpleXMLElems.AddCData(const Name, Value: string): TJclSimpleXMLElemCData;
begin
  Result := TJclSimpleXMLElemCData.Create(Name, Value);
  AddChild(Result);
end;

function TJclSimpleXMLElems.AddText(const Name, Value: string): TJclSimpleXMLElemText;
begin
  Result := TJclSimpleXMLElemText.Create(Name, Value);
  AddChild(Result);
end;

procedure TJclSimpleXMLElems.BinaryValue(const Name: string; Stream: TStream);
var
  Elem: TJclSimpleXMLElem;
begin
  Elem := GetItemNamed(Name);
  if Elem <> nil then
    Elem.GetBinaryValue(Stream);
end;

function TJclSimpleXMLElems.BoolValue(const Name: string; Default: Boolean): Boolean;
var
  Elem: TJclSimpleXMLElem;
begin
  try
    Elem := GetItemNamedDefault(Name, BoolToStr(Default));
    if (Elem = nil) or (Elem.Value = '') then
      Result := Default
    else
      Result := Elem.BoolValue;
  except
    Result := Default;
  end;
end;

procedure TJclSimpleXMLElems.Clear;
begin
  if FElems <> nil then
    FElems.Clear;
  if FNamedElems <> nil then
    FNamedElems.Clear;
end;

constructor TJclSimpleXMLElems.Create(AParent: TJclSimpleXMLElem);
begin
  inherited Create;
  FParent := AParent;
end;

procedure TJclSimpleXMLElems.CreateElems;
var
  CaseSensitive: Boolean;
begin
  if FElems = nil then
  begin
    CaseSensitive := Assigned(Parent) and Assigned(Parent.SimpleXML)
      and (sxoCaseSensitive in Parent.SimpleXML.Options);
    FElems := TJclSimpleItemHashedList.Create(CaseSensitive);
  end;
end;

procedure TJclSimpleXMLElems.Delete(const Index: Integer);
var
  Elem: TJclSimpleXMLElem;
  NamedIndex: Integer;
begin
  if (FElems <> nil) and (Index >= 0) and (Index < FElems.Count) then
  begin
    Elem := TJclSimpleXMLElem(FElems.SimpleItems[Index]);
    if FNamedElems <> nil then
    begin
      NamedIndex := FNamedElems.IndexOfName(Elem.Name);
      if NamedIndex >= 0 then
        TJclSimpleXMLNamedElems(FNamedElems.SimpleItems[NamedIndex]).FItems.Remove(Elem);
    end;
    FElems.Delete(Index);
  end;
end;

procedure TJclSimpleXMLElems.Delete(const Name: string);
begin
  if FElems <> nil then
    Delete(FElems.IndexOfName(Name));
end;

destructor TJclSimpleXMLElems.Destroy;
begin
  FParent := nil;
  Clear;
  FreeAndNil(FElems);
  FreeAndNil(FNamedElems);
  inherited Destroy;
end;

procedure TJclSimpleXMLElems.DoItemRename(Value: TJclSimpleXMLElem; const Name: string);
var
  NamedIndex: Integer;
begin
  if FNamedElems <> nil then
  begin
    NamedIndex := FNamedElems.IndexOfName(Value.Name);
    if NamedIndex >= 0 then
      TJclSimpleXMLNamedElems(FNamedElems.SimpleItems[NamedIndex]).FItems.Remove(Value);

    NamedIndex := FNamedElems.IndexOfName(Name);
    if NamedIndex >= 0 then
      TJclSimpleXMLNamedElems(FNamedElems.SimpleItems[NamedIndex]).FItems.Add(Value);
  end;
  FElems.InvalidateHash;
end;

function TJclSimpleXMLElems.FloatValue(const Name: string;
  const Default: Extended): Extended;
var
  Elem: TJclSimpleXMLElem;
begin
  Elem := GetItemNamedDefault(Name, FloatToStr(Default));
  if Elem = nil then
    Result := Default
  else
    Result := Elem.FloatValue;
end;

function TJclSimpleXMLElems.GetCount: Integer;
begin
  if FElems = nil then
    Result := 0
  else
    Result := FElems.Count;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSimpleXMLElems.GetEnumerator: TJclSimpleXMLElemsEnumerator;
begin
  Result := TJclSimpleXMLElemsEnumerator.Create(Self);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSimpleXMLElems.GetItem(const Index: Integer): TJclSimpleXMLElem;
begin
  if (FElems = nil) or (Index > FElems.Count) then
    Result := nil
  else
    Result := TJclSimpleXMLElem(FElems.SimpleItems[Index]);
end;

function TJclSimpleXMLElems.GetItemNamedDefault(const Name, Default: string): TJclSimpleXMLElem;
var
  I: Integer;
begin
  Result := nil;
  if FElems <> nil then
  begin
    I := FElems.IndexOfName(Name);
    if I <> -1 then
      Result := TJclSimpleXMLElem(FElems.SimpleItems[I])
    else
    if Assigned(Parent) and Assigned(Parent.SimpleXML) and (sxoAutoCreate in Parent.SimpleXML.Options) then
      Result := Add(Name, Default);
  end
  else
  if Assigned(Parent) and Assigned(Parent.SimpleXML) and (sxoAutoCreate in Parent.SimpleXML.Options) then
    Result := Add(Name, Default);
end;

function TJclSimpleXMLElems.GetNamedElems(const Name: string): TJclSimpleXMLNamedElems;
var
  NamedIndex: Integer;
  CaseSensitive: Boolean;
begin
  if FNamedElems = nil then
  begin
    CaseSensitive := Assigned(Parent) and Assigned(Parent.SimpleXML)
      and (sxoCaseSensitive in Parent.SimpleXML.Options);
    FNamedElems := TJclSimpleItemHashedList.Create(CaseSensitive);
  end;
  NamedIndex := FNamedElems.IndexOfName(Name);
  if NamedIndex = -1 then
  begin
    Result := TJclSimpleXMLNamedElems.Create(Self, Name);
    FNamedElems.Add(Result);
    if FElems <> nil then
      for NamedIndex := 0 to FElems.Count - 1 do
        if FElems.SimpleItems[NamedIndex].Name = Name then
          Result.FItems.Add(FElems.SimpleItems[NamedIndex]);
  end
  else
    Result := TJclSimpleXMLNamedElems(FNamedElems.SimpleItems[NamedIndex]);
end;

function TJclSimpleXMLElems.GetItemNamed(const Name: string): TJclSimpleXMLElem;
begin
  Result := GetItemNamedDefault(Name, '');
end;

function TJclSimpleXMLElems.IntValue(const Name: string; const Default: Int64): Int64;
var
  Elem: TJclSimpleXMLElem;
begin
  Elem := GetItemNamedDefault(Name, IntToStr(Default));
  if Elem = nil then
    Result := Default
  else
    Result := Elem.IntValue;
end;

procedure TJclSimpleXMLElems.LoadFromStringStream(StringStream: TJclStringStream);
type
  TReadStatus = (rsWaitingTag, rsReadingTagKind);
var
  lPos: TReadStatus;
  St: TUCS4Array;
  lElem: TJclSimpleXMLElem;
  Ch: UCS4;
  ContainsText, ContainsWhiteSpace, KeepWhiteSpace: Boolean;
  SimpleXML: TJclSimpleXML;
begin
  SetLength(St, 0);
  lPos := rsWaitingTag;
  SimpleXML := Parent.SimpleXML;
  KeepWhiteSpace := (SimpleXML <> nil) and (sxoKeepWhitespace in SimpleXML.Options);
  ContainsText := False;
  ContainsWhiteSpace := False;

  // We read from a stream, thus replacing the existing items
  Clear;

  if SimpleXML <> nil then
    SimpleXML.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.PeekUCS4(Ch) do
  begin
    case lPos of
      rsWaitingTag: //We are waiting for a tag and thus avoiding spaces
        begin
          if Ch = Ord('<') then
          begin
            lPos := rsReadingTagKind;
            St := UCS4Array(Ch);
          end
          else
          if UnicodeIsWhiteSpace(Ch) then
            ContainsWhiteSpace := True
          else
            ContainsText := True;
        end;

      rsReadingTagKind: //We are trying to determine the kind of the tag
        begin
          lElem := nil;
          case Ch of
            Ord('/'):
              if UCS4ArrayEquals(St, '<') then
              begin // "</"
                // We have reached an end tag. If whitespace was found while
                // waiting for the end tag, and the user told us to keep it
                // then we have to create a text element.
                // But it must only be created if there are no other elements
                // in the list. If we did not check this, we would create a
                // text element for whitespace found between two adjacent end
                // tags.
                if ContainsText or (ContainsWhiteSpace and KeepWhiteSpace) then
                begin
                  lElem := TJclSimpleXMLElemText.Create;
                  CreateElems;
                  FElems.Add(lElem);
                  Notify(lElem, opInsert);
                  lElem.LoadFromStringStream(StringStream);
                end;
                Break;
              end
              else
              begin
                lElem := TJclSimpleXMLElemClassic.Create;
                UCS4ArrayConcat(St, Ch); // "<name/"
                lPos := rsWaitingTag;
              end;

            Ord(NativeSpace), Ord('>'), Ord(':'): //This should be a classic tag
              begin    // "<XXX " or "<XXX:" or "<XXX>
                lElem := TJclSimpleXMLElemClassic.Create;
                SetLength(St, 0);
                lPos := rsWaitingTag;
              end;
          else
            if ContainsText or (ContainsWhiteSpace and KeepWhiteSpace) then
            begin
              // inner text
              lElem := TJclSimpleXMLElemText.Create;
              lPos := rsReadingTagKind;
              ContainsText := False;
              ContainsWhiteSpace := False;
            end
            else
            begin
              if not UCS4ArrayEquals(St, '<![CDATA') or not UnicodeIsWhiteSpace(Ch) then
                UCS4ArrayConcat(St, Ch);
              if UCS4ArrayEquals(St, '<![CDATA[') then
              begin
                lElem := TJclSimpleXMLElemCData.Create;
                lPos := rsWaitingTag;
                SetLength(St, 0);
              end
              else
              if UCS4ArrayEquals(St, '<!--') then
              begin
                lElem := TJclSimpleXMLElemComment.Create;
                lPos := rsWaitingTag;
                SetLength(St, 0);
              end
              else
              if UCS4ArrayEquals(St, '<?') then
              begin
                lElem := TJclSimpleXMLElemProcessingInstruction.Create;
                lPos := rsWaitingTag;
                SetLength(St, 0);
              end;
            end;
          end;

          if lElem <> nil then
          begin
            CreateElems;
            FElems.Add(lElem);
            Notify(lElem, opInsert);
            lElem.LoadFromStringStream(StringStream);
          end;
        end;
    end;
  end;
end;

procedure TJclSimpleXMLElems.Notify(Value: TJclSimpleXMLElem; Operation: TOperation);
var
  NamedIndex: Integer;
begin
  case Operation of
    opRemove:
      if Value.Parent = Parent then  // Only remove if we have it
      begin
        if FNamedElems <> nil then
        begin
          NamedIndex := FNamedElems.IndexOfName(Value.Name);
          if NamedIndex >= 0 then
            TJclSimpleXMLNamedElems(FNamedElems.SimpleItems[NamedIndex]).FItems.Remove(Value);
        end;
        FElems.Remove(Value);
      end;
    opInsert:
      begin
        Value.FParent := Parent;
        Value.FSimpleXML := Parent.SimpleXML;
      end;
  end;
end;

function TJclSimpleXMLElems.Remove(Value: TJclSimpleXMLElem): Integer;
begin
  if FElems = nil
     then Result := -1 // like TList.IndexOf(alien)
     else begin
        Result := FElems.IndexOfSimpleItem(Value);
        Notify(Value, opRemove);
     end;
end;

procedure TJclSimpleXMLElems.SaveToStringStream(StringStream: TJclStringStream;
  const Level: string);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Item[I].SaveToStringStream(StringStream, Level);
end;

function TJclSimpleXMLElems.SimpleCompare(Elems: TJclSimpleXMLElems; Index1,
  Index2: Integer): Integer;
begin
  Result := CompareText(Elems.Item[Index1].Name, Elems.Item[Index2].Name);
end;

function TJclSimpleXMLElems.Value(const Name, Default: string): string;
var
  Elem: TJclSimpleXMLElem;
begin
  Result := '';
  Elem := GetItemNamedDefault(Name, Default);
  if Elem = nil then
    Result := Default
  else
    Result := Elem.Value;
end;

procedure TJclSimpleXMLElems.Move(const CurIndex, NewIndex: Integer);
begin
  if FElems <> nil then
    FElems.Move(CurIndex, NewIndex);
end;

function TJclSimpleXMLElems.IndexOf(const Value: TJclSimpleXMLElem): Integer;
begin
  if FElems = nil then
    Result := -1
  else
    Result := FElems.IndexOfSimpleItem(Value);
end;

function TJclSimpleXMLElems.IndexOf(const Name: string): Integer;
begin
  if FElems = nil then
    Result := -1
  else
    Result := FElems.IndexOfName(Name);
end;

procedure TJclSimpleXMLElems.InsertChild(const Value: TJclSimpleXMLElem; Index: Integer);
var
  NamedIndex: Integer;
begin
  CreateElems;

  // If there already is a container, notify it to remove the element
  if Assigned(Value.Parent) then begin
    if (value.parent<>FParent) then begin
      if FNamedElems <> nil then begin
        NamedIndex := FNamedElems.IndexOfName(Value.Name);
        if NamedIndex >= 0 then
           TJclSimpleXMLNamedElems(FNamedElems.SimpleItems[NamedIndex]).FItems.Remove(Value);
      end;
      Value.FParent.items.FElems.Extract(Value); //EW here is the real difference
      Value.FParent := nil;
      Value.FSimpleXML := nil;
    end
    else
    begin
      Value.Parent.Items.Notify(Value, opRemove);
    end;
  end;

  FElems.Insert(Index, Value);

  if FNamedElems <> nil then
  begin
    NamedIndex := FNamedElems.IndexOfName(Value.Name);
    if NamedIndex >= 0 then
      TJclSimpleXMLNamedElems(FNamedElems.SimpleItems[NamedIndex]).FItems.Add(Value);
  end;

  Notify(Value, opInsert);
end;

function TJclSimpleXMLElems.Insert(Value: TJclSimpleXMLElem;
  Index: Integer): TJclSimpleXMLElem;
begin
  if Value <> nil then
    InsertChild(Value, Index);
  Result := Value;
end;

function TJclSimpleXMLElems.Insert(const Name: string;
  Index: Integer): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Name);
  InsertChild(Result, Index);
end;

procedure QuickSort(Elems: TJclSimpleXMLElems; List: TList; L, R: Integer;
  AFunction: TJclSimpleXMLElemCompare);
var
  I, J, M: Integer;
begin
  repeat
    I := L;
    J := R;
    M := (L + R) shr 1;
    repeat
      while AFunction(Elems, I, M) < 0 do
        Inc(I);
      while AFunction(Elems, J, M) > 0 do
        Dec(J);
      if I < J then
      begin
        List.Exchange(I, J);
        Inc(I);
        Dec(J);
      end
      else
      if I = J then
      begin
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(Elems, List, L, J, AFunction);
    L := I;
  until I >= R;
end;

procedure TJclSimpleXMLElems.CustomSort(AFunction: TJclSimpleXMLElemCompare);
begin
  if FElems <> nil then
    QuickSort(Self, FElems, 0, FElems.Count - 1, AFunction);
end;

procedure TJclSimpleXMLElems.Sort;
begin
  CustomSort(SimpleCompare);
end;

//=== { TJclSimpleXMLPropsEnumerator } =======================================

{$IFDEF SUPPORTS_FOR_IN}
constructor TJclSimpleXMLPropsEnumerator.Create(AList: TJclSimpleXMLProps);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

function TJclSimpleXMLPropsEnumerator.GetCurrent: TJclSimpleXMLProp;
begin
  Result := FList[FIndex];
end;

function TJclSimpleXMLPropsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;
{$ENDIF SUPPORTS_FOR_IN}

//=== { TJclSimpleXMLProps } =================================================

function TJclSimpleXMLProps.Add(const Name, Value: string): TJclSimpleXMLProp;
begin
  if FProperties = nil then
    FProperties := TStringList.Create;
  Result := TJclSimpleXMLProp.Create(Parent, Name, Value);
  FProperties.AddObject(Name, Result);
end;

function TJclSimpleXMLProps.Add(const Name: string; const Value: Int64): TJclSimpleXMLProp;
begin
  Result := Add(Name, IntToStr(Value));
end;

function TJclSimpleXMLProps.Add(const Name: string; const Value: Boolean): TJclSimpleXMLProp;
begin
  Result := Add(Name, BoolToStr(Value));
end;

{$IFDEF SUPPORTS_UNICODE}
function TJclSimpleXMLProps.Add(const Name: string;
  const Value: AnsiString): TJclSimpleXMLProp;
begin
  Result := Add(Name, string(Value));
end;
{$ENDIF SUPPORTS_UNICODE}

function TJclSimpleXMLProps.Insert(const Index: Integer; const Name, Value: string): TJclSimpleXMLProp;
begin
  if FProperties = nil then
    FProperties := TStringList.Create;
  Result := TJclSimpleXMLProp.Create(Parent, Name, Value);
  FProperties.InsertObject(Index, Name, Result);
end;

function TJclSimpleXMLProps.Insert(const Index: Integer; const Name: string; const Value: Int64): TJclSimpleXMLProp;
begin
  Result := Insert(Index, Name, IntToStr(Value));
end;

function TJclSimpleXMLProps.Insert(const Index: Integer; const Name: string; const Value: Boolean): TJclSimpleXMLProp;
begin
  Result := Insert(Index, Name, BoolToStr(Value));
end;

function TJclSimpleXMLProps.BoolValue(const Name: string; Default: Boolean): Boolean;
var
  Prop: TJclSimpleXMLProp;
begin
  try
    Prop := GetItemNamedDefault(Name, BoolToStr(Default));
    if (Prop = nil) or (Prop.Value = '') then
      Result := Default
    else
      Result := Prop.BoolValue;
  except
    Result := Default;
  end;
end;

procedure TJclSimpleXMLProps.Clear;
var
  I: Integer;
begin
  if FProperties <> nil then
  begin
    for I := 0 to FProperties.Count - 1 do
    begin
      TJclSimpleXMLProp(FProperties.Objects[I]).Free;
      FProperties.Objects[I] := nil;
    end;
    FProperties.Clear;
  end;
end;

procedure TJclSimpleXMLProps.Delete(const Index: Integer);
begin
  if (FProperties <> nil) and (Index >= 0) and (Index < FProperties.Count) then
  begin
    TObject(FProperties.Objects[Index]).Free;
    FProperties.Delete(Index);
  end;
end;

constructor TJclSimpleXMLProps.Create(AParent: TJclSimpleXMLElem);
begin
  inherited Create;
  FParent := AParent;
end;

procedure TJclSimpleXMLProps.Delete(const Name: string);
begin
  if FProperties <> nil then
    Delete(FProperties.IndexOf(Name));
end;

destructor TJclSimpleXMLProps.Destroy;
begin
  FParent := nil;
  Clear;
  FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TJclSimpleXMLProps.DoItemRename(Value: TJclSimpleXMLProp; const Name: string);
var
  I: Integer;
begin
  if FProperties = nil then
    Exit;
  I := FProperties.IndexOfObject(Value);
  if I <> -1 then
    FProperties[I] := Name;
end;

procedure TJclSimpleXMLProps.Error(const S: string);
begin
  raise EJclSimpleXMLError.Create(S);
end;

function TJclSimpleXMLProps.FloatValue(const Name: string;
  const Default: Extended): Extended;
var
  Prop: TJclSimpleXMLProp;
begin
  Prop := GetItemNamedDefault(Name, FloatToStr(Default));
  if Prop = nil then
    Result := Default
  else
    Result := Prop.FloatValue;
end;

procedure TJclSimpleXMLProps.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S, Args));
end;

function TJclSimpleXMLProps.GetCount: Integer;
begin
  if FProperties = nil then
    Result := 0
  else
    Result := FProperties.Count;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSimpleXMLProps.GetEnumerator: TJclSimpleXMLPropsEnumerator;
begin
  Result := TJclSimpleXMLPropsEnumerator.Create(Self);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSimpleXMLProps.GetItem(const Index: Integer): TJclSimpleXMLProp;
begin
  if FProperties <> nil then
    Result := TJclSimpleXMLProp(FProperties.Objects[Index])
  else
    Result := nil;
end;

function TJclSimpleXMLProps.GetItemNamedDefault(const Name, Default: string): TJclSimpleXMLProp;
var
  I: Integer;
begin
  Result := nil;
  if FProperties <> nil then
  begin
    I := FProperties.IndexOf(Name);
    if I <> -1 then
      Result := TJclSimpleXMLProp(FProperties.Objects[I])
    else
    if Assigned(FParent) and Assigned(FParent.SimpleXML) and (sxoAutoCreate in FParent.SimpleXML.Options) then
      Result := Add(Name, Default);
  end
  else
  if Assigned(FParent) and Assigned(FParent.SimpleXML) and (sxoAutoCreate in FParent.SimpleXML.Options) then
  begin
    Result := Add(Name, Default);
  end;
end;

function TJclSimpleXMLProps.GetItemNamed(const Name: string): TJclSimpleXMLProp;
begin
  Result := GetItemNamedDefault(Name, '');
end;

function TJclSimpleXMLProps.GetSimpleXML: TJclSimpleXML;
begin
  if FParent <> nil then
    Result := FParent.SimpleXML
  else
    Result := nil;
end;

function TJclSimpleXMLProps.IntValue(const Name: string; const Default: Int64): Int64;
var
  Prop: TJclSimpleXMLProp;
begin
  Prop := GetItemNamedDefault(Name, IntToStr(Default));
  if Prop = nil then
    Result := Default
  else
    Result := Prop.IntValue;
end;

procedure TJclSimpleXMLProps.LoadFromStringStream(StringStream: TJclStringStream);
//<element Prop="foo" Prop='bar' foo:bar="beuh"/>
//Stop on / or ? or >
type
  TPosType = (
    ptWaiting,
    ptReadingName,
    ptStartingContent,
    ptReadingValue,
    ptSpaceBeforeEqual
    );
var
  lPos: TPosType;
  lName, lValue, lNameSpace: TUCS4Array;
  sValue: string;
  lPropStart: UCS4;
  Ch: UCS4;
begin
  SetLength(lValue, 0);
  SetLength(lNameSpace, 0);
  SetLength(lName, 0);
  lPropStart := Ord(NativeSpace);
  lPos := ptWaiting;

  // We read from a stream, thus replacing the existing properties
  Clear;

  while StringStream.PeekUCS4(Ch) do
  begin
    case lPos of
      ptWaiting: //We are waiting for a property
        begin
          if UnicodeIsWhiteSpace(Ch) then
            StringStream.ReadUCS4(Ch)
          else
          if UnicodeIsIdentifierStart(Ch) or (Ch = Ord('-')) or (Ch = Ord('.')) or (Ch = Ord('_')) then
          begin
            StringStream.ReadUCS4(Ch);
            lName := UCS4Array(Ch);
            SetLength(lNameSpace, 0);
            lPos := ptReadingName;
          end
          else
          if (Ch = Ord('/')) or (Ch = Ord('>')) or (Ch = Ord('?')) then
            // end of properties
            Break
          else
            FmtError(LoadResString(@RsEInvalidXMLElementUnexpectedCharacte), [UCS4ToChar(Ch), StringStream.PeekPosition]);
        end;

      ptReadingName: //We are reading a property name
        begin
          StringStream.ReadUCS4(Ch);
          if UnicodeIsIdentifierPart(Ch) or (Ch = Ord('-')) or (Ch = Ord('.')) then
          begin
            UCS4ArrayConcat(lName, Ch);
          end
          else
          if Ch = Ord(':') then
          begin
            lNameSpace := lName;
            SetLength(lName, 0);
          end
          else
          if Ch = Ord('=') then
            lPos := ptStartingContent
          else
          if UnicodeIsWhiteSpace(Ch) then
            lPos := ptSpaceBeforeEqual
          else
            FmtError(LoadResString(@RsEInvalidXMLElementUnexpectedCharacte), [UCS4ToChar(Ch), StringStream.PeekPosition]);
        end;

      ptStartingContent: //We are going to start a property content
        begin
          StringStream.ReadUCS4(Ch);
          if UnicodeIsWhiteSpace(Ch) then
            // ignore white space
          else
          if (Ch = Ord('''')) or (Ch = Ord('"')) then
          begin
            lPropStart := Ch;
            SetLength(lValue, 0);
            lPos := ptReadingValue;
          end
          else
            FmtError(LoadResString(@RsEInvalidXMLElementUnexpectedCharacte_), [UCS4ToChar(Ch), StringStream.PeekPosition]);
        end;

      ptReadingValue: //We are reading a property
        begin
          StringStream.ReadUCS4(Ch);
          if Ch = lPropStart then
          begin
            sValue := UCS4ToString(lValue);
            if GetSimpleXML <> nil then
              GetSimpleXML.DoDecodeValue(sValue);
            with Add(UCS4ToString(lName), sValue) do
              NameSpace := UCS4ToString(lNameSpace);
            lPos := ptWaiting;
          end
          else
            UCS4ArrayConcat(lValue, Ch);
        end;

      ptSpaceBeforeEqual: // We are reading the white space between a property name and the = sign
        begin
          StringStream.ReadUCS4(Ch);
          if UnicodeIsWhiteSpace(Ch) then
            // more white space, stay in this state and ignore
          else
          if Ch = Ord('=') then
            lPos := ptStartingContent
          else
            FmtError(LoadResString(@RsEInvalidXMLElementUnexpectedCharacte), [UCS4ToChar(Ch), StringStream.PeekPosition]);
        end;
    else
      Assert(False, RsEUnexpectedValueForLPos);
    end;
  end;
end;

procedure TJclSimpleXMLProps.SaveToStringStream(StringStream: TJclStringStream);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Item[I].SaveToStringStream(StringStream);
end;

procedure TJclSimpleXMLProps.SortProperties(const Order: array of string);
var
  I, Index, InsertIndex: Integer;
begin
  InsertIndex := 0;
  for I := 0 to High(Order) do
  begin
    Index := FProperties.IndexOf(Order[I]);
    if Index <> -1 then
    begin
      FProperties.Move(Index, InsertIndex);
      Inc(InsertIndex);
    end;
  end;
end;

function TJclSimpleXMLProps.Value(const Name, Default: string): string;
var
  Prop: TJclSimpleXMLProp;
begin
  Result := '';
  Prop := GetItemNamedDefault(Name, Default);
  if Prop = nil then
    Result := Default
  else
    Result := Prop.Value;
end;

//=== { TJclSimpleXMLProp } ==================================================

constructor TJclSimpleXMLProp.Create(AParent: TJclSimpleXMLElem; const AName, AValue: string);
begin
  inherited Create(AName, AValue);
  FParent := AParent;
end;

function TJclSimpleXMLProp.GetSimpleXML: TJclSimpleXML;
begin
  if FParent <> nil then
    Result := FParent.SimpleXML
  else
    Result := nil;
end;

procedure TJclSimpleXMLProp.SaveToStringStream(StringStream: TJclStringStream);
var
  AEncoder: TJclSimpleXML;
  Tmp: string;
begin
  AEncoder := GetSimpleXML;
  Tmp := Value;
  if AEncoder <> nil then
    AEncoder.DoEncodeValue(Tmp);
  if NameSpace <> '' then
    Tmp := Format(' %s:%s="%s"', [NameSpace, Name, Tmp])
  else
    Tmp := Format(' %s="%s"', [Name, tmp]);
  StringStream.WriteString(Tmp, 1, Length(Tmp));
end;

procedure TJclSimpleXMLProp.SetName(const Value: string);
begin
  if (Value <> Name) and (Value <> '') then
  begin
    if (Parent <> nil) and (Name <> '') then
      FParent.Properties.DoItemRename(Self, Value);
    inherited SetName(Value);
  end;
end;

//=== { TJclSimpleXMLElemClassic } ===========================================

procedure TJclSimpleXMLElemClassic.LoadFromStringStream(StringStream: TJclStringStream);
//<element Prop="foo" Prop='bar'/>
//<element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
//<xml:element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
type
  TReadStatus = (rsWaitingOpeningTag, rsOpeningName, rsTypeOpeningTag, rsEndSingleTag,
    rsWaitingClosingTag1, rsWaitingClosingTag2, rsClosingName);
var
  lPos: TReadStatus;
  St, lName, lNameSpace: TUCS4Array;
  sValue: string;
  Ch: UCS4;
begin
  SetLength(St, 0);
  SetLength(lName, 0);
  SetLength(lNameSpace, 0);
  sValue := '';
  lPos := rsWaitingOpeningTag;

  if SimpleXML <> nil then
    SimpleXML.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.ReadUCS4(Ch) do
  begin
    case lPos of
      rsWaitingOpeningTag: // wait beginning of tag
        if Ch = Ord('<') then
          lPos := rsOpeningName // read name
        else
        if not UnicodeIsWhiteSpace(Ch) then
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedBeginningO), [UCS4ToChar(Ch), StringStream.PeekPosition]);

      rsOpeningName:
        if UnicodeIsIdentifierPart(Ch) or (Ch = Ord('-')) or (Ch = Ord('.')) then
          UCS4ArrayConcat(St, Ch)
        else
        if (Ch = Ord(':')) and (Length(lNameSpace) = 0) then
        begin
          lNameSpace := St;
          SetLength(st, 0);
        end
        else
        if UnicodeIsWhiteSpace(Ch) and (Length(St) = 0) then
          // whitespace after "<" (no name)
          FmtError(LoadResString(@RsEInvalidXMLElementMalformedTagFoundn), [StringStream.PeekPosition])
        else
        if UnicodeIsWhiteSpace(Ch) then
        begin
          lName := St;
          SetLength(St, 0);
          Properties.LoadFromStringStream(StringStream);
          lPos := rsTypeOpeningTag;
        end
        else
        if Ch = Ord('/') then // single tag
        begin
          lName := St;
          lPos := rsEndSingleTag
        end
        else
        if Ch = Ord('>') then // 2 tags
        begin
          lName := St;
          SetLength(St, 0);
          //Load elements
          Items.LoadFromStringStream(StringStream);
          lPos := rsWaitingClosingTag1;
        end
        else
          // other invalid characters
          FmtError(LoadResString(@RsEInvalidXMLElementMalformedTagFoundn), [StringStream.PeekPosition]);

      rsTypeOpeningTag:
        if UnicodeIsWhiteSpace(Ch) then
          // nothing, spaces after name or properties
        else
        if Ch = Ord('/') then
          lPos := rsEndSingleTag // single tag
        else
        if Ch = Ord('>') then // 2 tags
        begin
          //Load elements
          Items.LoadFromStringStream(StringStream);
          lPos := rsWaitingClosingTag1;
        end
        else
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedEndOfTagBu), [UCS4ToChar(Ch), StringStream.PeekPosition]);

      rsEndSingleTag:
        if Ch = Ord('>') then
          Break
        else
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedEndOfTagBu), [UCS4ToChar(Ch), StringStream.PeekPosition]);

      rsWaitingClosingTag1:
        if UnicodeIsWhiteSpace(Ch) then
          // nothing, spaces before closing tag
        else
        if Ch = Ord('<') then
          lPos := rsWaitingClosingTag2
        else
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedEndOfTagBu), [UCS4ToChar(Ch), StringStream.PeekPosition]);

      rsWaitingClosingTag2:
        if Ch = Ord('/') then
          lPos := rsClosingName
        else
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedEndOfTagBu), [UCS4ToChar(Ch), StringStream.PeekPosition]);

      rsClosingName:
        if UnicodeIsWhiteSpace(Ch) or (Ch = Ord('>')) then
        begin
          if Length(lNameSpace) > 0 then
          begin
            if not StrSame(UCS4ToString(lNameSpace) + ':' + UCS4ToString(lName), UCS4ToString(St)) then
              FmtError(LoadResString(@RsEInvalidXMLElementErroneousEndOfTagE), [UCS4ToString(lName), UCS4ToString(St), StringStream.PeekPosition]);
          end
          else
            if not UCS4ArrayEquals(lName, St) then
              FmtError(LoadResString(@RsEInvalidXMLElementErroneousEndOfTagE), [UCS4ToString(lName), UCS4ToString(St), StringStream.PeekPosition]);
          //Set value if only one sub element
          //This might reduce speed, but this is for compatibility issues
          if (Items.Count = 1) and (Items[0] is TJclSimpleXMLElemText) then
          begin
            sValue := Items[0].Value;
            Items.Clear;
            // free some memory
            FreeAndNil(FItems);
          end;
          Break;
        end
        else
        if UnicodeIsIdentifierPart(Ch) or (Ch = Ord('-')) or (Ch = Ord('.')) or (Ch = Ord(':')) then
          UCS4ArrayConcat(St, Ch)
        else
          // other invalid characters
          FmtError(LoadResString(@RsEInvalidXMLElementMalformedTagFoundn), [StringStream.PeekPosition]);
    end;
  end;

  Name := UCS4ToString(lName);
  if SimpleXML <> nil then
    SimpleXML.DoDecodeValue(sValue);
  Value := sValue;
  NameSpace := UCS4ToString(lNameSpace);

  if SimpleXML <> nil then
  begin
    SimpleXML.DoTagParsed(Name);
    SimpleXML.DoValueParsed(Name, sValue);
  end;
end;

procedure TJclSimpleXMLElemClassic.SaveToStringStream(StringStream: TJclStringStream; const Level: string);
var
  St, AName, tmp: string;
  LevelAdd: string;
  AutoIndent: Boolean;
begin
  if(NameSpace <> '') then
    AName := NameSpace + ':' + Name
  else
    AName := Name;

  if Name <> '' then
  begin
    if SimpleXML <> nil then
       SimpleXML.DoEncodeValue(AName);
    St := Level + '<' + AName;

    StringStream.WriteString(St, 1, Length(St));
    if Assigned(FProps) then
      FProps.SaveToStringStream(StringStream);
  end;

  AutoIndent := (SimpleXML <> nil) and (sxoAutoIndent in SimpleXML.Options);

  if (ItemCount = 0) then
  begin
    tmp := Value;
    if (Name <> '') then
    begin
      if Value = '' then
      begin
        if AutoIndent then
          St := '/>' + sLineBreak
        else
          St := '/>';
      end
      else
      begin
        if SimpleXML <> nil then
          SimpleXML.DoEncodeValue(tmp);
        if AutoIndent then
          St := '>' + tmp + '</' + AName + '>' + sLineBreak
        else
          St := '>' + tmp + '</' + AName + '>';
      end;
      StringStream.WriteString(St, 1, Length(St));
    end;
  end
  else
  begin
    if (Name <> '') then
    begin
      if AutoIndent then
        St := '>' + sLineBreak
      else
        St := '>';
      StringStream.WriteString(St, 1, Length(St));
    end;
    if AutoIndent then
    begin
      LevelAdd := SimpleXML.IndentString;
    end;
    FItems.SaveToStringStream(StringStream, Level + LevelAdd);
    if Name <> '' then
    begin
      if AutoIndent then
        St := Level + '</' + AName + '>' + sLineBreak
      else
        St := Level + '</' + AName + '>';
      StringStream.WriteString(St, 1, Length(St));
    end;
  end;
  if SimpleXML <> nil then
    SimpleXML.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemComment } ===========================================

procedure TJclSimpleXMLElemComment.LoadFromStringStream(StringStream: TJclStringStream);
//<!-- declarations for <head> & <body> -->
const
  CS_START_COMMENT = '<!--';
  CS_STOP_COMMENT  = '    -->';
var
  lPos: Integer;
  St: TUCS4Array;
  Ch: UCS4;
  lOk: Boolean;
begin
  SetLength(St, 0);
  lPos := 1;
  lOk := False;

  if SimpleXML <> nil then
    SimpleXML.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.ReadUCS4(Ch) do
  begin
    case lPos of
      1..4: //<!--
        if Ch = Ord(CS_START_COMMENT[lPos]) then
          Inc(lPos)
        else
        if not UnicodeIsWhiteSpace(Ch) then
          FmtError(LoadResString(@RsEInvalidCommentExpectedsButFounds), [CS_START_COMMENT[lPos], UCS4ToChar(Ch), StringStream.PeekPosition]);
      5:
        if Ch = Ord(CS_STOP_COMMENT[lPos]) then
          Inc(lPos)
        else
          UCS4ArrayConcat(St, Ch);
      6: //-
        if Ch = Ord(CS_STOP_COMMENT[lPos]) then
          Inc(lPos)
        else
        begin
          UCS4ArrayConcat(St, Ord('-'));
          UCS4ArrayConcat(St, Ch);
          Dec(lPos);
        end;
      7: //>
        if Ch = Ord(CS_STOP_COMMENT[lPos]) then
        begin
          lOk := True;
          Break; //End if
        end
        else // -- is not authorized in comments
          FmtError(LoadResString(@RsEInvalidCommentNotAllowedInsideComme), [StringStream.PeekPosition]);
    end;
  end;

  if not lOk then
    FmtError(LoadResString(@RsEInvalidCommentUnexpectedEndOfData), [StringStream.PeekPosition]);

  Value := UCS4ToString(St);
  Name := '';

  if SimpleXML <> nil then
    SimpleXML.DoValueParsed('', Value);
end;

procedure TJclSimpleXMLElemComment.SaveToStringStream(StringStream: TJclStringStream; const Level: string);
var
  St: string;
begin
  St := Level + '<!--';
  StringStream.WriteString(St, 1, Length(St));
  if Value <> '' then
    StringStream.WriteString(Value, 1, Length(Value));
  if (SimpleXML <> nil) and (sxoAutoIndent in SimpleXML.Options) then
    St := '-->' + sLineBreak
  else
    St := '-->';
  StringStream.WriteString(St, 1, Length(St));
  if SimpleXML <> nil then
    SimpleXML.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemCData } =============================================

procedure TJclSimpleXMLElemCData.LoadFromStringStream(StringStream: TJclStringStream);
//<![CDATA[<greeting>Hello, world!</greeting>]]>
const
  CS_START_CDATA = '<![CDATA[';
  CS_STOP_CDATA  = '         ]]>';
var
  lPos: Integer;
  St: TUCS4Array;
  Ch: UCS4;
  lOk: Boolean;
begin
  SetLength(St, 0);
  lPos := 1;
  lOk := False;

  if SimpleXML <> nil then
    SimpleXML.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.ReadUCS4(Ch) do
  begin
    case lPos of
      1..9: //<![CDATA[
        if Ch = Ord(CS_START_CDATA[lPos]) then
          Inc(lPos)
        else
        if not UnicodeIsWhiteSpace(Ch) then
          FmtError(LoadResString(@RsEInvalidCDATAExpectedsButFounds), [CS_START_CDATA[lPos], UCS4ToChar(Ch), StringStream.PeekPosition]);
      10: // ]
        if Ch = Ord(CS_STOP_CDATA[lPos]) then
          Inc(lPos)
        else
          UCS4ArrayConcat(St, Ch);
      11: // ]
        if Ch = Ord(CS_STOP_CDATA[lPos]) then
          Inc(lPos)
        else
        begin
          UCS4ArrayConcat(St, Ord(']'));
          UCS4ArrayConcat(St, Ch);
          Dec(lPos);
        end;
      12: //>
        if Ch = Ord(CS_STOP_CDATA[lPos]) then
        begin
          lOk := True;
          Break; //End if
        end
        else
        // ]]]
        if Ch = Ord(CS_STOP_CDATA[lPos-1]) then
          UCS4ArrayConcat(St, Ord(']'))
        else
        begin
          UCS4ArrayConcat(St, Ord(']'));
          UCS4ArrayConcat(St, Ord(']'));
          UCS4ArrayConcat(St, Ch);
          Dec(lPos, 2);
        end;
    end;
  end;

  if not lOk then
    FmtError(LoadResString(@RsEInvalidCDATAUnexpectedEndOfData), [StringStream.PeekPosition]);

  Value := UCS4ToString(St);
  Name := '';

  if SimpleXML <> nil then
    SimpleXML.DoValueParsed('', Value);
end;

procedure TJclSimpleXMLElemCData.SaveToStringStream(StringStream: TJclStringStream; const Level: string);
var
  St: string;
begin
  St := Level + '<![CDATA[';
  StringStream.WriteString(St, 1, Length(St));
  if Value <> '' then
    StringStream.WriteString(Value, 1, Length(Value));
  if (SimpleXML <> nil) and (sxoAutoIndent in SimpleXML.Options) then
    St := ']]>' + sLineBreak
  else
    St := ']]>';
  StringStream.WriteString(St, 1, Length(St));
  if SimpleXML <> nil then
    SimpleXML.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemText } ==============================================

procedure TJclSimpleXMLElemText.LoadFromStringStream(StringStream: TJclStringStream);
var
  Ch: UCS4;
  USt: TUCS4Array;
  St, TrimValue: string;
begin
  SetLength(USt, 0);
  St := '';

  if SimpleXML <> nil then
    SimpleXML.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.PeekUCS4(Ch) do
  begin
    case Ch of
      Ord('<'):
        //Quit text
        Break;
    else
      begin
        StringStream.ReadUCS4(Ch);
        UCS4ArrayConcat(USt, Ch);
      end;
    end;
  end;

  St := UCS4ToString(USt);

  if Assigned(SimpleXML) then
  begin
    SimpleXML.DoDecodeValue(St);

    TrimValue := St;
    if sxoTrimPrecedingTextWhitespace in SimpleXML.Options then
      TrimValue := TrimLeft(TrimValue);
    if sxoTrimFollowingTextWhitespace in SimpleXML.Options then
      TrimValue := TrimRight(TrimValue);
    if (TrimValue <> '') or not (sxoKeepWhitespace in SimpleXML.Options) then
      St := TrimValue;
  end;

  Value := St;
  Name := '';

  if SimpleXML <> nil then
    SimpleXML.DoValueParsed('', St);
end;

procedure TJclSimpleXMLElemText.SaveToStringStream(StringStream: TJclStringStream; const Level: string);
var
  St, tmp: string;
begin
  // should never be used
  if Value <> '' then
  begin
    tmp := Value;
    if SimpleXML <> nil then
      SimpleXML.DoEncodeValue(tmp);
    if (SimpleXML <> nil) and (sxoAutoIndent in SimpleXML.Options) then
      St := Level + tmp + sLineBreak
    else
      St := Level + tmp;
    StringStream.WriteString(St, 1, Length(St));
  end;
  if SimpleXML <> nil then
    SimpleXML.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemProcessingInstruction } =============================

procedure TJclSimpleXMLElemProcessingInstruction.LoadFromStringStream(
  StringStream: TJclStringStream);
type
  TReadStatus = (rsWaitingOpeningTag, rsOpeningTag, rsOpeningName, rsEndTag1, rsEndTag2);
var
  lPos: TReadStatus;
  lOk: Boolean;
  St, lName, lNameSpace: TUCS4Array;
  Ch: UCS4;
begin
  SetLength(St, 0);
  SetLength(lName, 0);
  SetLength(lNameSpace, 0);
  lPos := rsWaitingOpeningTag;
  lOk := False;

  if SimpleXML <> nil then
    SimpleXML.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.ReadUCS4(Ch) do
  begin
    case lPos of
      rsWaitingOpeningTag: // wait beginning of tag
        if Ch = Ord('<') then
          lPos := rsOpeningTag
        else
        if not UnicodeIsWhiteSpace(Ch) then
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedBeginningO), [UCS4ToChar(Ch), StringStream.PeekPosition]);

      rsOpeningTag:
        if Ch = Ord('?') then
          lPos := rsOpeningName // read name
        else
          FmtError(LoadResString(@RsEInvalidXMLElementMalformedTagFoundn), [StringStream.PeekPosition]);

      rsOpeningName:
        if UnicodeIsIdentifierPart(Ch) or (Ch = Ord('-')) or (Ch = Ord('.')) then
          UCS4ArrayConcat(St, Ch)
        else
        if (Ch = Ord(':')) and (Length(lNameSpace) = 0) then
        begin
          lNameSpace := St;
          SetLength(St, 0);
        end
        else
        if UnicodeIsWhiteSpace(Ch) and (Length(St) = 0) then
          // whitespace after "<" (no name)
          FmtError(LoadResString(@RsEInvalidXMLElementMalformedTagFoundn), [StringStream.PeekPosition])
        else
        if UnicodeIsWhiteSpace(Ch) then
        begin
          lName := St;
          SetLength(St, 0);
          Properties.LoadFromStringStream(StringStream);
          lPos := rsEndTag1;
        end
        else
        if Ch = Ord('?') then
        begin
          lName := St;
          lPos := rsEndTag2;
        end
        else
          // other invalid characters
          FmtError(LoadResString(@RsEInvalidXMLElementMalformedTagFoundn), [StringStream.PeekPosition]);

      rsEndTag1:
        if Ch = Ord('?') then
          lPos := rsEndTag2
        else
        if not UnicodeIsWhiteSpace(Ch) then
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedEndOfTagBu), [UCS4ToChar(Ch), StringStream.PeekPosition]);

      rsEndTag2:
        if Ch = Ord('>') then
        begin
          lOk := True;
          Break;
        end
        else
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedEndOfTagBu), [UCS4ToChar(Ch), StringStream.PeekPosition]);
    end;
  end;

  if not lOk then
    FmtError(LoadResString(@RsEInvalidCommentUnexpectedEndOfData), [StringStream.PeekPosition]);

  Name := UCS4ToString(lName);
  NameSpace := UCS4ToString(lNameSpace);
end;

procedure TJclSimpleXMLElemProcessingInstruction.SaveToStringStream(
  StringStream: TJclStringStream; const Level: string);
var
  St: string;
begin
  St := Level + '<?';
  if NameSpace <> '' then
    St := St + NameSpace + ':' + Name
  else
    St := St + Name;
  StringStream.WriteString(St, 1, Length(St));
  if Assigned(FProps) then
    FProps.SaveToStringStream(StringStream);
  if (SimpleXML <> nil) and (sxoAutoIndent in SimpleXML.Options) then
    St := '?>' + sLineBreak
  else
    St := '?>';
  StringStream.WriteString(St, 1, Length(St));
  if SimpleXML <> nil then
    SimpleXML.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemHeader } ============================================

constructor TJclSimpleXMLElemHeader.Create;
begin
  inherited Create;

  Name := 'xml';
end;

function TJclSimpleXMLElemHeader.GetEncoding: string;
var
  ASimpleXML: TJclSimpleXML;
  DefaultCodePage: Word;
begin
  ASimpleXML := SimpleXML;
  if Assigned(ASimpleXML) then
  begin
    DefaultCodePage := ASimpleXML.CodePage;
    {$IFDEF MSWINDOWS}
    if DefaultCodePage = CP_ACP then
      DefaultCodePage := GetAcp;
    {$ENDIF MSWINDOWS}
  end
  else
    {$IFDEF UNICODE}
    DefaultCodePage := CP_UTF16LE;
    {$ELSE ~UNICODE}
    {$IFDEF MSWINDOWS}
    DefaultCodePage := GetACP;
    {$ELSE ~MSWINDOWS}
    DefaultCodePage := 1252;
    {$ENDIF ~MSWINDOWS}
    {$ENDIF ~UNICODE}
  Result := Properties.Value('encoding', CharsetNameFromCodePage(DefaultCodePage));
end;

function TJclSimpleXMLElemHeader.GetStandalone: Boolean;
begin
  Result := Properties.Value('standalone') = 'yes';
end;

function TJclSimpleXMLElemHeader.GetVersion: string;
begin
  Result := Properties.Value('version', '1.0');
end;

procedure TJclSimpleXMLElemHeader.LoadFromStringStream(StringStream: TJclStringStream);
//<?xml version="1.0" encoding="iso-xyzxx" standalone="yes"?>
var
  CodePage: Word;
  EncodingProp: TJclSimpleXMLProp;
begin
  inherited LoadFromStringStream(StringStream);

  if Assigned(FProps) then
    EncodingProp := FProps.ItemNamed['encoding']
  else
    EncodingProp := nil;
  if Assigned(EncodingProp) and (EncodingProp.Value <> '') then
    CodePage := CodePageFromCharsetName(EncodingProp.Value)
  else
    CodePage := CP_ACP;

  // set current stringstream codepage
  if StringStream is TJclAutoStream then
    TJclAutoStream(StringStream).CodePage := CodePage
  else
  if StringStream is TJclAnsiStream then
    TJclAnsiStream(StringStream).CodePage := CodePage
  else
  if not (StringStream is TJclUTF8Stream) and not (StringStream is TJclUTF16Stream) then
    Error(LoadResString(@RsENoCharset));
end;

procedure TJclSimpleXMLElemHeader.SaveToStringStream(
  StringStream: TJclStringStream; const Level: string);
begin
  SetVersion(GetVersion);
  SetEncoding(GetEncoding);
  SetStandalone(GetStandalone);
  Properties.SortProperties(['version', 'encoding', 'standalone']);

  inherited SaveToStringStream(StringStream, Level);
end;

procedure TJclSimpleXMLElemHeader.SetEncoding(const Value: string);
var
  Prop: TJclSimpleXMLProp;
begin
  Prop := Properties.ItemNamed['encoding'];
  if Assigned(Prop) then
    Prop.Value := Value
  else
    Properties.Add('encoding', Value);
end;

procedure TJclSimpleXMLElemHeader.SetStandalone(const Value: Boolean);
var
  Prop: TJclSimpleXMLProp;
const
  BooleanValues: array [Boolean] of string = ('no', 'yes');
begin
  Prop := Properties.ItemNamed['standalone'];
  if Assigned(Prop) then
    Prop.Value := BooleanValues[Value]
  else
    Properties.Add('standalone', BooleanValues[Value]);
end;

procedure TJclSimpleXMLElemHeader.SetVersion(const Value: string);
var
  Prop: TJclSimpleXMLProp;
begin
  Prop := Properties.ItemNamed['version'];
  if Assigned(Prop) then
    Prop.Value := Value
  else
    // Various XML parsers (including MSIE, Firefox) require the "version" to be the first
    Properties.Insert(0, 'version', Value);
end;

//=== { TJclSimpleXMLElemDocType } ===========================================

procedure TJclSimpleXMLElemDocType.LoadFromStringStream(StringStream: TJclStringStream);
{
<!DOCTYPE test [
<!ELEMENT test (#PCDATA) >
<!ENTITY % xx '&#37;zz;'>
<!ENTITY % zz '&#60;!ENTITY tricky "error-prone" >' >
%xx;
]>

<!DOCTYPE greeting SYSTEM "hello.dtd">
}
const
  CS_START_DOCTYPE = '<!DOCTYPE';
var
  lPos: Integer;
  lOk: Boolean;
  Ch, lChar: UCS4;
  St: TUCS4Array;
begin
  lPos := 1;
  lOk := False;
  lChar := Ord('>');
  SetLength(St, 0);

  if SimpleXML <> nil then
    SimpleXML.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.ReadUCS4(Ch) do
  begin
    case lPos of
      1..9: //<!DOCTYPE
        if Ch = Ord(CS_START_DOCTYPE[lPos]) then
          Inc(lPos)
        else
        if not UnicodeIsWhiteSpace(Ch) then
          FmtError(LoadResString(@RsEInvalidHeaderExpectedsButFounds), [CS_START_DOCTYPE[lPos], UCS4ToChar(Ch), StringStream.PeekPosition]);
      10: //]> or >
        if lChar = Ch then
        begin
          if lChar = Ord('>') then
          begin
            lOk := True;
            Break; //This is the end
          end
          else
          begin
            UCS4ArrayConcat(St, Ch);
            lChar := Ord('>');
          end;
        end
        else
        begin
          UCS4ArrayConcat(St, Ch);
          if Ch = Ord('[') then
            lChar := Ord(']');
        end;
    end;
  end;

  if not lOk then
    FmtError(LoadResString(@RsEInvalidCommentUnexpectedEndOfData), [StringStream.PeekPosition]);

  Name := '';
  Value := StrTrimCharsLeft(UCS4ToString(St), CharIsWhiteSpace);

  if SimpleXML <> nil then
    SimpleXML.DoValueParsed('', Value);
end;

procedure TJclSimpleXMLElemDocType.SaveToStringStream(StringStream: TJclStringStream;
  const Level: string);
var
  St: string;
begin
  if (SimpleXML <> nil) and (sxoAutoIndent in SimpleXML.Options) then
    St := Level + '<!DOCTYPE ' + Value + '>' + sLineBreak
  else
    St := Level + '<!DOCTYPE ' + Value + '>';
  StringStream.WriteString(St, 1, Length(St));
  if SimpleXML <> nil then
    SimpleXML.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemsPrologEnumerator } =================================

{$IFDEF SUPPORTS_FOR_IN}
constructor TJclSimpleXMLElemsPrologEnumerator.Create(AList: TJclSimpleXMLElemsProlog);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

function TJclSimpleXMLElemsPrologEnumerator.GetCurrent: TJclSimpleXMLElem;
begin
  Result := FList[FIndex];
end;

function TJclSimpleXMLElemsPrologEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;
{$ENDIF SUPPORTS_FOR_IN}

//=== { TJclSimpleXMLElemsProlog } ===========================================

constructor TJclSimpleXMLElemsProlog.Create(ASimpleXML: TJclSimpleXML);
var
  CaseSensitive: Boolean;
begin
  inherited Create;
  FSimpleXML := ASimpleXML;
  CaseSensitive := Assigned(ASimpleXML) and (sxoCaseSensitive in ASimpleXML.Options);
  FElems := TJclSimpleItemHashedList.Create(CaseSensitive);
end;

destructor TJclSimpleXMLElemsProlog.Destroy;
begin
  Clear;
  FreeAndNil(FElems);
  inherited Destroy;
end;

procedure TJclSimpleXMLElemsProlog.Clear;
begin
  FElems.Clear;
end;

function TJclSimpleXMLElemsProlog.GetCount: Integer;
begin
  Result := FElems.Count;
end;

function TJclSimpleXMLElemsProlog.GetItem(const Index: Integer): TJclSimpleXMLElem;
begin
  Result := TJclSimpleXMLElem(FElems.SimpleItems[Index]);
end;

procedure TJclSimpleXMLElemsProlog.LoadFromStringStream(StringStream: TJclStringStream);
{<?xml version="1.0" encoding="UTF-8" ?>
<!-- Test -->
<!DOCTYPE greeting [
  <!ELEMENT greeting (#PCDATA)>
]>
<greeting>Hello, world!</greeting>

<?xml version="1.0"?> <!DOCTYPE greeting SYSTEM "hello.dtd"> <greeting>Hello, world!</greeting>
}
var
  lPos: Integer;
  St: TUCS4Array;
  lEnd: Boolean;
  lElem: TJclSimpleXMLElem;
  Ch: UCS4;
begin
  SetLength(St, 0);
  lPos := 0;

  if SimpleXML <> nil then
    SimpleXML.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.PeekUCS4(Ch) do
  begin
    case lPos of
      0: //We are waiting for a tag and thus avoiding spaces and any BOM
        begin
          if UnicodeIsWhiteSpace(Ch) then
            // still waiting
          else
          if Ch = Ord('<') then
          begin
            lPos := 1;
            St := UCS4Array(Ch);
          end
          else
            FmtError(LoadResString(@RsEInvalidDocumentUnexpectedTextInFile), [StringStream.PeekPosition]);
        end;
      1: //We are trying to determine the kind of the tag
        begin
          lElem := nil;
          lEnd := False;

          if not UCS4ArrayEquals(St, '<![CDATA') or not UnicodeIsWhiteSpace(Ch) then
            UCS4ArrayConcat(St, Ch);
          if UCS4ArrayEquals(St, '<![CDATA[') then
            lEnd := True
          else
          if UCS4ArrayEquals(St, '<!--') then
            lElem := TJclSimpleXMLElemComment.Create(SimpleXML)
          else
          if UCS4ArrayEquals(St, '<?xml-stylesheet') then
            lElem := TJclSimpleXMLElemSheet.Create(SimpleXML)
          else
          if UCS4ArrayEquals(St, '<?xml ') then
            lElem := TJclSimpleXMLElemHeader.Create(SimpleXML)
          else
          if UCS4ArrayEquals(St, '<!DOCTYPE') then
            lElem := TJclSimpleXMLElemDocType.Create(SimpleXML)
          else
          if UCS4ArrayEquals(St, '<?mso-application') then
            lElem := TJclSimpleXMLElemMSOApplication.Create(SimpleXML)
          else
          if (Length(St) > 3) and (St[1] = Ord('?')) and UnicodeIsWhiteSpace(St[High(St)]) then
            lElem := TJclSimpleXMLElemProcessingInstruction.Create(SimpleXML)
          else
          if (Length(St) > 1) and (St[1] <> Ord('!')) and (St[1] <> Ord('?')) then
            lEnd := True;

          if lEnd then
            Break
          else
          if lElem <> nil then
          begin
            FElems.Add(lElem);
            lElem.LoadFromStringStream(StringStream);
            SetLength(St, 0);
            lPos := 0;
          end;
        end;
    end;
  end;
end;

procedure TJclSimpleXMLElemsProlog.SaveToStringStream(StringStream: TJclStringStream);
var
  I: Integer;
begin
  FindHeader;
  for I := 0 to Count - 1 do
    Item[I].SaveToStringStream(StringStream, '');
end;

function VarXML: TVarType;
begin
  Result := XMLVariant.VarType;
end;

procedure XMLCreateInto(var ADest: Variant; const AXML: TJclSimpleXMLElem);
begin
  TVarData(ADest).vType := VarXML;
  TVarData(ADest).vAny := AXML;
end;

function XMLCreate(const AXML: TJclSimpleXMLElem): Variant;
begin
  XMLCreateInto(Result, AXML);
end;

function XMLCreate: Variant;
begin
  XMLCreateInto(Result, TJclSimpleXMLElemClassic.Create(nil));
end;

//=== { TXMLVariant } ========================================================

procedure TXMLVariant.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
var
  StorageStream: TStringStream;
  ConversionString: TJclStringStream;
begin
  if Source.vType = VarType then
  begin
    case AVarType of
      varOleStr:
        begin
          StorageStream := TStringStream.Create('');
          try
            ConversionString := TJclUTF16Stream.Create(StorageStream, False);
            try
              ConversionString.WriteBOM;
              TJclSimpleXMLElem(Source.vAny).SaveToStringStream(ConversionString, '');
              ConversionString.Flush;
            finally
              ConversionString.Free;
            end;
            VarDataFromOleStr(Dest, StorageStream.DataString);
          finally
            StorageStream.Free;
          end;
        end;
      varString:
        begin
          StorageStream := TStringStream.Create('');
          try
            {$IFDEF SUPPORTS_UNICODE}
            ConversionString := TJclUTF16Stream.Create(StorageStream, False);
            {$ELSE ~SUPPORTS_UNICODE}
            ConversionString := TJclAnsiStream.Create(StorageStream, False);
            {$ENDIF ~SUPPORTS_UNICODE}
            try
              ConversionString.WriteBOM;
              TJclSimpleXMLElem(Source.vAny).SaveToStringStream(ConversionString, '');
              ConversionString.Flush;
            finally
              ConversionString.Free;
            end;
            VarDataFromStr(Dest, StorageStream.DataString);
          finally
            StorageStream.Free;
          end;
        end;
      {$IFDEF SUPPORTS_UNICODE_STRING}
      varUString:
        begin
          StorageStream := TStringStream.Create('');
          try
            ConversionString := TJclUTF16Stream.Create(StorageStream, False);
            try
              ConversionString.WriteBOM;
              TJclSimpleXMLElem(Source.vAny).SaveToStringStream(ConversionString, '');
              ConversionString.Flush;
            finally
              ConversionString.Free;
            end;
            VarDataClear(Dest);
            Dest.VUString := nil;
            Dest.VType := varUString;
            UnicodeString(Dest.VUString) := UnicodeString(StorageStream.DataString);
          finally
            StorageStream.Free;
          end;
        end;
      {$ENDIF SUPPORTS_UNICODE_STRING}
    else
      RaiseCastError;
    end;
  end
  else
    inherited CastTo(Dest, Source, AVarType);
end;

procedure TXMLVariant.Clear(var V: TVarData);
begin
  V.vType := varEmpty;
  V.vAny := nil;
end;

procedure TXMLVariant.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
  begin
    Dest.vType := Source.vType;
    Dest.vAny := Source.vAny;
  end;
end;

function TXMLVariant.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
var
  VXML, LXML: TJclSimpleXMLElem;
  VElems: TJclSimpleXMLElems;
  I, J, K: Integer;
begin
  Result := False;
  if (Length(Arguments) = 1) and (Arguments[0].vType in [vtInteger, vtExtended]) then
  begin
    VXML := TJclSimpleXMLElem(V.VAny);
    K := Arguments[0].vInteger;
    J := 0;

    if (K > 0) and VXML.HasItems then
    begin
      VElems := VXML.Items;
      for I := 0 to VElems.Count - 1 do
        if UpperCase(VElems.Item[I].Name) = Name then
        begin
          Inc(J);
          if J = K then
            Break;
        end;
    end;

    if (J = K) and (J < VXML.ItemCount) then
    begin
      LXML := VXML.Items[J];
      if LXML <> nil then
      begin
        Dest.vType := VarXML;
        Dest.vAny := Pointer(LXML);
        Result := True;
      end
    end;
  end
end;

function TXMLVariant.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
var
  VXML, LXML: TJclSimpleXMLElem;
  lProp: TJclSimpleXMLProp;
begin
  Result := False;
  VXML := TJclSimpleXMLElem(V.VAny);
  if VXML.HasItems then
  begin
    LXML := VXML.Items.ItemNamed[Name];
    if LXML <> nil then
    begin
      Dest.vType := VarXML;
      Dest.vAny := Pointer(LXML);
      Result := True;
    end;
  end;
  if (not Result) and VXML.HasProperties then
  begin
    lProp := VXML.Properties.ItemNamed[Name];
    if lProp <> nil then
    begin
      VarDataFromOleStr(Dest, lProp.Value);
      Result := True;
    end;
  end;
end;

function TXMLVariant.IsClear(const V: TVarData): Boolean;
var
  VXML: TJclSimpleXMLElem;
begin
  VXML := TJclSimpleXMLElem(V.VAny);
  Result := (VXML = nil) or (not VXML.HasItems);
end;

function TXMLVariant.SetProperty(const V: TVarData; const Name: string;
  const Value: TVarData): Boolean;

  function GetStrValue: string;
  begin
    try
      Result := Value.VOleStr;
    except
      Result := '';
    end;
  end;

var
  VXML, LXML: TJclSimpleXMLElem;
  lProp: TJclSimpleXMLProp;
begin
  Result := False;
  VXML := TJclSimpleXMLElem(V.VAny);
  if VXML.HasItems then
  begin
    LXML := VXML.Items.ItemNamed[Name];
    if LXML <> nil then
    begin
      LXML.Value := GetStrValue;
      Result := True;
    end;
  end;
  if (not Result) and VXML.HasProperties then
  begin
    lProp := VXML.Properties.ItemNamed[Name];
    if lProp <> nil then
    begin
      lProp.Value := GetStrValue;
      Result := True;
    end;
  end;
end;

procedure TJclSimpleXMLElemsProlog.Error(const S: string);
begin
  raise EJclSimpleXMLError.Create(S);
end;

procedure TJclSimpleXMLElemsProlog.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S, Args));
end;

procedure TJclSimpleXML.SetIndentString(const Value: string);
begin
  // test if the new value is only made of spaces or tabs
  if not StrContainsChars(Value, CharIsWhiteSpace, True) then
    Exit;
  FIndentString := Value;
end;

procedure TJclSimpleXML.SetRoot(const Value: TJclSimpleXMLElemClassic);
begin
  if Value <> FRoot then
  begin
//    FRoot.FSimpleXML := nil;
    FRoot := Value;
//    FRoot.FSimpleXML := Self;
  end;
end;

function TJclSimpleXMLElemsProlog.GetEncoding: string;
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Result := Elem.Encoding
  else
    Result := 'UTF-8';
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSimpleXMLElemsProlog.GetEnumerator: TJclSimpleXMLElemsPrologEnumerator;
begin
  Result := TJclSimpleXMLElemsPrologEnumerator.Create(Self);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSimpleXMLElemsProlog.GetStandalone: Boolean;
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Result := Elem.Standalone
  else
    Result := False;
end;

function TJclSimpleXMLElemsProlog.GetVersion: string;
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Result := Elem.Version
  else
    Result := '1.0';
end;

procedure TJclSimpleXMLElemsProlog.SetEncoding(const Value: string);
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Elem.Encoding := Value;
end;

procedure TJclSimpleXMLElemsProlog.SetStandalone(const Value: Boolean);
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Elem.Standalone := Value;
end;

procedure TJclSimpleXMLElemsProlog.SetVersion(const Value: string);
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Elem.Version := Value;
end;

function TJclSimpleXMLElemsProlog.FindHeader: TJclSimpleXMLElem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Item[I] is TJclSimpleXMLElemHeader then
    begin
      Result := Item[I];
      Exit;
    end;
  // (p3) if we get here, an xml header was not found
  Result := TJclSimpleXMLElemHeader.Create(SimpleXML);
  FElems.Add(Result);
end;

function TJclSimpleXMLElemsProlog.AddStyleSheet(const AType, AHRef: string): TJclSimpleXMLElemSheet;
begin
  // make sure there is an xml header
  FindHeader;
  Result := TJclSimpleXMLElemSheet.Create('xml-stylesheet');
  Result.Properties.Add('type',AType);
  Result.Properties.Add('href',AHRef);
  FElems.Add(Result);
end;

function TJclSimpleXMLElemsProlog.AddMSOApplication(const AProgId : string): TJclSimpleXMLElemMSOApplication;
begin
  // make sure there is an xml header
  FindHeader;
  Result := TJclSimpleXMLElemMSOApplication.Create('mso-application');
  Result.Properties.Add('progid',AProgId);
  FElems.Add(Result);
end;

function TJclSimpleXMLElemsProlog.AddComment(const AValue: string): TJclSimpleXMLElemComment;
begin
  // make sure there is an xml header
  FindHeader;
  Result := TJclSimpleXMLElemComment.Create('', AValue);
  FElems.Add(Result);
end;

function TJclSimpleXMLElemsProlog.AddDocType(const AValue: string): TJclSimpleXMLElemDocType;
begin
  // make sure there is an xml header
  FindHeader;
  Result := TJclSimpleXMLElemDocType.Create('', AValue);
  FElems.Add(Result);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  FreeAndNil(GlobalXMLVariant);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
