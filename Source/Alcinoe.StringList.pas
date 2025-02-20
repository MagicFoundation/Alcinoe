{*******************************************************************************
TALStringListA
TALStringListA Work the same as Delphi TstringList except that it's allow to
search a name=value using a quicksort algorithm when the list is sorted. Also
TALStringListA use a locale independant algorithme (based on the 8-bit ordinal
value of each character) instead of the AnsiCompareText and AnsiCompareStr used
by the Delphi TstringList. at the end the sort in TALStringListA is up to 10x
more faster than in Delphi TstringList. Also TALStringListA is not an unicode
TstringList but an 100% Ansi StringList

TALStringListW
TALStringListW is the same as TALStringListA but with unicode String

TALNVStringListA
TALNVStringListA (NV for NameValue) is same as TALStringListA (use also a
quicksort algorithme) except that here optimisation is oriented for name/value
list instead of string list.

TALNVStringListW
TALNVStringListA is the same as TALNVStringListW but with unicode String

TALAVLStringListA
TALAVLStringListA is same as TALStringListA except that it's use internally a
self-balancing binary Tree instead of a quicksort algorithm

TALHashedStringListA
TALHashedStringListA is same as TALStringListA except that it's use an internal
hash table instead of a quicksort algorithm. By using TALHashedStringListA
instead of TALStringListA, you can improve performance when the list contains a
large number of strings (else if your list don't contain a lot of strings the
performance is lower than TALStringListA because of the cost to calculate the
hash)
*******************************************************************************}
unit Alcinoe.StringList;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.classes.TStringsEnumerator didn''t change and adjust the IFDEF'}
{$ENDIF}

{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.classes.TStrings didn''t change and adjust the IFDEF'}
{$ENDIF}

{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.classes.TStringList didn''t change and adjust the IFDEF'}
{$ENDIF}

Uses
  System.Classes,
  System.Sysutils,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Math,
  System.Contnrs,
  Alcinoe.AVLBinaryTree,
  Alcinoe.QuickSortList;

Type

  {------------------}
  TALStringsA = class;

  {---------------------------}
  TALStringsEnumeratorA = class
  private
    FIndex: Integer;
    FStrings: TALStringsA;
  public
    constructor Create(AStrings: TALStringsA);
    function GetCurrent: AnsiString;
    function MoveNext: Boolean; inline;
    property Current: AnsiString read GetCurrent;
  end;

  {------------------------------}
  TALStringsA = class(TPersistent)
  private
    //[deleted from Tstrings] FEncoding: TEncoding;
    //[deleted from Tstrings] FDefaultEncoding: TEncoding;
    //[deleted from Tstrings] FWriteBOM: Boolean;
    //[deleted from Tstrings] procedure SetDefaultEncoding(const Value: TEncoding);
    //[deleted from Tstrings] procedure SetStringsAdapter(const Value: IStringsAdapter);
    //[deleted from Tstrings] FAdapter: IStringsAdapter;
    //[deleted from Tstrings] procedure ReadData(Reader: TReader);
    //[deleted from Tstrings] procedure WriteData(Writer: TWriter);
    FDelimiter: AnsiChar;
    FLineBreak: AnsiString;
    FQuoteChar: AnsiChar;
    FNameValueSeparator: AnsiChar;
    FStrictDelimiter: Boolean;
    FUpdateCount: Integer;
    FProtectedSave: Boolean;
    function GetCommaText: AnsiString;
    function GetDelimitedText: AnsiString;
    procedure SetCommaText(const Value: AnsiString);
    procedure SetDelimitedText(const Value: AnsiString);
    function GetIsEmpty: Boolean; inline;
  protected
    //[deleted from Tstrings] procedure SetEncoding(const Value: TEncoding);
    //[deleted from Tstrings] procedure DefineProperties(Filer: TFiler); override;
    function GetName(Index: Integer): AnsiString; virtual;
    function GetStrictName(Index: Integer): AnsiString; virtual; // [added from Tstrings]
    function GetValue(const Name: AnsiString): AnsiString; virtual;
    procedure SetValue(const Name, Value: AnsiString); virtual;
    function GetValueFromIndex(Index: Integer): AnsiString; virtual;
    procedure SetValueFromIndex(Index: Integer; const Value: AnsiString); virtual;
    procedure SetPersistentValue(const Name, Value: AnsiString); virtual; // [added from Tstrings]
    procedure SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString); virtual; // [added from Tstrings]
    procedure Error(const Msg: String; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure IndexError(AIndex, AMaxIndex: Integer);
    function ExtractName(const S: AnsiString): AnsiString;
    function Get(Index: Integer): AnsiString; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: AnsiString; virtual;
    procedure Put(Index: Integer; const S: AnsiString); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: AnsiString); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
    function CompareStrings(const S1, S2: AnsiString): Integer; virtual;
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
  public
    //[deleted from Tstrings] procedure LoadFromFile(const FileName: string; Encoding: TEncoding); overload; virtual;
    //[deleted from Tstrings] procedure LoadFromStream(Stream: TStream; Encoding: TEncoding); overload; virtual;
    //[deleted from Tstrings] procedure SaveToFile(const FileName: string; Encoding: TEncoding); overload; virtual;
    //[deleted from Tstrings] procedure SaveToStream(Stream: TStream; Encoding: TEncoding); overload; virtual;
    //[deleted from Tstrings] property DefaultEncoding: TEncoding read FDefaultEncoding write SetDefaultEncoding;
    //[deleted from Tstrings] property Encoding: TEncoding read FEncoding;
    //[deleted from Tstrings] property WriteBOM: Boolean read FWriteBOM write FWriteBOM;
    //[deleted from Tstrings] property StringsAdapter: IStringsAdapter read FAdapter write SetStringsAdapter;
    //[deleted from Tstrings] destructor Destroy; override;
    constructor Create; virtual;
    function Add(const S: AnsiString): Integer; virtual;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; virtual;
    function AddNameValue(const Name, Value: AnsiString): Integer; virtual; // [added from Tstrings]
    function AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer; virtual; // [added from Tstrings]
    procedure Append(const S: AnsiString);
    procedure AddStrings(Strings: TALStringsA); overload; virtual;
    procedure AddStrings(const Strings: array of AnsiString); overload;
    procedure AddStrings(const Strings: array of AnsiString; const Objects: array of TObject); overload;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    function Contains(const S: AnsiString): Boolean; inline;
    function ContainsName(const Name: AnsiString): Boolean; inline;
    function ContainsObject(const AObject: TObject): Boolean; inline;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TALStringsA): Boolean; reintroduce;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetEnumerator: TALStringsEnumeratorA; inline;
    function GetText: PAnsiChar; virtual;
    function IndexOf(const S: AnsiString): Integer; virtual;
    function IndexOfName(const Name: AnsiString): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: AnsiString); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: AnsiString; AObject: TObject); virtual;
    procedure InsertNameValue(Index: Integer; const Name, Value: AnsiString); virtual; // [added from Tstrings]
    procedure InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject); virtual; // [added from Tstrings]
    procedure LoadFromFile(const FileName: AnsiString); overload; virtual;
    procedure LoadFromFile(const FileName: String); overload; virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: AnsiString); overload; virtual;
    procedure SaveToFile(const FileName: String); overload; virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(Text: PAnsiChar); virtual;
    function ToStringArray: TArray<AnsiString>;
    function ToObjectArray: TArray<TObject>;
    function ToNameValueArray: TArray<TPair<AnsiString, AnsiString>>;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: AnsiString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property Delimiter: AnsiChar read fDelimiter write fDelimiter;
    property DelimitedText: AnsiString read GetDelimitedText write SetDelimitedText;
    property LineBreak: AnsiString read fLineBreak write fLineBreak;
    property Names[Index: Integer]: AnsiString read GetName;
    property StrictNames[Index: Integer]: AnsiString read GetStrictName; // [added from Tstrings]
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: AnsiChar read fQuoteChar write fQuoteChar;
    property Values[const Name: AnsiString]: AnsiString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: AnsiString read GetValueFromIndex write SetValueFromIndex;
    property PersistentValues[const Name: AnsiString]: AnsiString read GetValue write SetPersistentValue; // [added from Tstrings]
    property PersistentValueFromIndex[Index: Integer]: AnsiString read GetValueFromIndex write SetPersistentValueFromIndex; // [added from Tstrings]
    property NameValueSeparator: AnsiChar read fNameValueSeparator write fNameValueSeparator;
    property StrictDelimiter: Boolean read fStrictDelimiter write fStrictDelimiter;
    property Strings[Index: Integer]: AnsiString read Get write Put; default;
    property Text: AnsiString read GetTextStr write SetTextStr;
    property ProtectedSave: Boolean read fProtectedSave write fProtectedSave;
  end;

  {---------------------}
  TALStringListA = class;

  {-------------------------------}
  PALStringItemA = ^TALStringItemA;
  TALStringItemA = record
    FString: AnsiString;
    FObject: TObject;
  end;

  {-------------------------------------------}
  TALStringItemListA = array of TALStringItemA;
  TALStringListSortCompareA = reference to function(List: TALStringListA; Index1, Index2: Integer): Integer;

  {---------------------------------}
  TALStringListA = class(TALStringsA)
  private
    FList: TALStringItemListA;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    FNameValueOptimization: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; ACompare: TALStringListSortCompareA);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): AnsiString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: AnsiString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: AnsiString): Integer; override;
    procedure InsertItem(Index: Integer; const S: AnsiString; AObject: TObject); virtual;
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
    procedure init(OwnsObjects: Boolean); virtual; //[added from TStringList]
    function Find(const S: AnsiString; var Index: Integer): Boolean; virtual;
  public
    constructor Create; overload; override;
    constructor Create(OwnsObjects: Boolean); reintroduce; overload;
    destructor Destroy; override;
    function Add(const S: AnsiString): Integer; override;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function  ExtractObject(Index: Integer): TObject; overload; virtual;
    procedure Exchange(Index1, Index2: Integer); override;
    function FindName(const S: AnsiString; var Index: Integer): Boolean; // [added from TStringList]
    function IndexOf(const S: AnsiString): Integer; override;
    function IndexOfName(const Name: AnsiString): Integer; override; // [added from TStringList]
    procedure Insert(Index: Integer; const S: AnsiString); override;
    procedure InsertObject(Index: Integer; const S: AnsiString; AObject: TObject); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TALStringListSortCompareA); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    property NameValueOptimization: Boolean read FNameValueOptimization write FNameValueOptimization;
  end;

  {-----------------------}
  TALNVStringListA = class;

  {-----------------------------------}
  PALNVStringItemA = ^TALNVStringItemA;
  TALNVStringItemA = record
    FName: AnsiString;
    FValue: AnsiString;
    FNVS: Boolean;
    FObject: TObject;
  end;

  {-----------------------------------------------}
  TALNVStringItemListA = array of TALNVStringItemA;
  TALNVStringListSortCompareA = reference to function(List: TALNVStringListA; Index1, Index2: Integer): Integer;

  {-----------------------------------}
  TALNVStringListA = class(TALStringsA)
  private
    FList: TALNVStringItemListA;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; ACompare: TALNVStringListSortCompareA);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
    Function ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean; //[added from TStringList]
  protected
    function GetName(Index: Integer): AnsiString; override;
    function GetStrictName(Index: Integer): AnsiString; override; // [added from Tstrings]
    function GetValue(const Name: AnsiString): AnsiString; override;
    procedure SetValue(const Name, Value: AnsiString); override;
    function GetValueFromIndex(Index: Integer): AnsiString; override;
    procedure SetValueFromIndex(Index: Integer; const Value: AnsiString); override;
    procedure SetPersistentValue(const Name, Value: AnsiString); override; // [added from Tstrings]
    procedure SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString); override; // [added from Tstrings]
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): AnsiString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    function GetTextStr: AnsiString; override;
    procedure Put(Index: Integer; const S: AnsiString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: AnsiString): Integer; override;
    procedure InsertItem(Index: Integer; const S: AnsiString; AObject: TObject); overload; virtual;
    procedure InsertItem(Index: Integer; const Name: AnsiString; WithNvS: boolean; AObject: TObject); overload; virtual; //[added from TStringList]
    procedure InsertItem(Index: Integer; const Name, Value: AnsiString; AObject: TObject); overload; virtual; //[added from TStringList]
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
    procedure init(OwnsObjects: Boolean); virtual; //[added from TStringList]
    function Find(const S: AnsiString; var Index: Integer): Boolean; virtual;
  public
    constructor Create; overload; override;
    constructor Create(OwnsObjects: Boolean); reintroduce; overload;
    destructor Destroy; override;
    function Add(const S: AnsiString): Integer; override;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; override;
    function AddNameValue(const Name, Value: AnsiString): Integer; override; // [added from Tstrings]
    function AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer; override; // [added from Tstrings]
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function  ExtractObject(Index: Integer): TObject; overload; virtual;
    procedure Exchange(Index1, Index2: Integer); override;
    function FindName(const Name: AnsiString; var Index: Integer): Boolean; overload; // [added from TStringList]
    function FindName(const Name: AnsiString; WithNvS: boolean; var Index: Integer): Boolean; overload; // [added from TStringList]
    function FindNameValue(const Name, Value: AnsiString; var Index: Integer): Boolean;  // [added from TStringList]
    function IndexOf(const S: AnsiString): Integer; override;
    function IndexOfName(const Name: AnsiString): Integer; override; // [added from TStringList]
    procedure Insert(Index: Integer; const S: AnsiString); override;
    procedure InsertObject(Index: Integer; const S: AnsiString; AObject: TObject); override;
    procedure InsertNameValue(Index: Integer; const Name, Value: AnsiString); override; // [added from Tstrings]
    procedure InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject); override; // [added from Tstrings]
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TALNVStringListSortCompareA); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

  {------------------------}
  TALAVLStringListA = class;
  TALAVLStringListSortCompareA = function(List: TALAVLStringListA; Index1, Index2: Integer): Integer;

  {--------------------------------------------------------------------}
  TALAVLStringListBinaryTreeNodeA = class(TALStringKeyAVLBinaryTreeNode)
  Private
  Protected
  Public
    Val: AnsiString;  // Value
    Obj: Tobject; // Object
    Idx: integer; // Index in the NodeList
    Nvs: Boolean; // if NameValueSeparator was present
  end;

  {-------------------------------}
  //when duplicate is set to ignore
  //it's take care only about the "name" part
  //and ignore the value part. exemple
  //if name1=value2 is in the list and
  //we add name1=value3 then if ignore duplicates
  //no error will be raise and the add command
  //will be silently ignored
  TALAVLStringListA = class(TALStringsA)
  private
    FNodeList: TObjectList;
    FAVLBinTree: TALStringKeyAVLBinaryTree;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure QuickSort(L, R: Integer; ACompare: TALAVLStringListSortCompareA);
    procedure SetCaseSensitive(const Value: Boolean);
    function GetCaseSensitive: Boolean;
    Function ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean;
    procedure SetDuplicates(const Value: TDuplicates);
  protected
    function GetName(Index: Integer): AnsiString; override;
    function GetStrictName(Index: Integer): AnsiString; override; // [added from Tstrings]
    function GetValue(const Name: AnsiString): AnsiString; override;
    procedure SetValue(const Name, Value: AnsiString); override;
    function GetValueFromIndex(Index: Integer): AnsiString; override;
    procedure SetValueFromIndex(Index: Integer; const Value: AnsiString); override;
    procedure SetPersistentValue(const Name, Value: AnsiString); override; // [added from Tstrings]
    procedure SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString); override; // [added from Tstrings]
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): AnsiString; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    function GetTextStr: AnsiString; override;
    procedure Put(Index: Integer; const S: AnsiString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure InsertItem(Index: Integer; const Name, Value: AnsiString; AObject: TObject); overload; virtual;
    procedure InsertItem(Index: Integer; const S: AnsiString; AObject: TObject); overload; virtual;
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
    procedure init(OwnsObjects: Boolean); virtual; //[added from TStringList]
  public
    constructor Create; overload; override;
    constructor Create(OwnsObjects: Boolean); reintroduce; overload;
    destructor Destroy; override;
    function Add(const S: AnsiString): Integer; override;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; override;
    function AddNameValue(const Name, Value: AnsiString): Integer; override; // [added from Tstrings]
    function AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer; override; // [added from Tstrings]
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function  ExtractObject(Index: Integer): TObject; overload; virtual;
    procedure Exchange(Index1, Index2: Integer); override;
    function IndexOf(const S: AnsiString): Integer; override;
    function IndexOfName(const Name: AnsiString): Integer; override; // [added from TStringList]
    procedure Insert(Index: Integer; const S: AnsiString); override;
    procedure InsertObject(Index: Integer; const S: AnsiString; AObject: TObject); override;
    procedure InsertNameValue(Index: Integer; const Name, Value: AnsiString); override; // [added from Tstrings]
    procedure InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject); override; // [added from Tstrings]
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure CustomSort(Compare: TALAVLStringListSortCompareA); virtual;
    property Duplicates: TDuplicates read FDuplicates write SetDuplicates;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

  {---------------------------}
  TALHashedStringListA = class;
  TALHashedStringListSortCompareA = function(List: TALHashedStringListA; Index1, Index2: Integer): Integer;

  {-------------------------------------------------}
  TALHashedStringListDictionaryNodeA = class(Tobject)
  Private
  Protected
  Public
    ID: AnsiString; // Name
    Val: AnsiString;  // Value
    Obj: Tobject; // Object
    Idx: integer; // Index in the NodeList
    Nvs: Boolean; // if NameValueSeparator was present
  end;

  {-------------------------------}
  //when duplicate is set to ignore
  //it's take care only about the "name" part
  //and ignore the value part. exemple
  //if name1=value2 is in the list and
  //we add name1=value3 then if ignore duplicates
  //no error will be raise and the add command
  //will be silently ignored
  TALHashedStringListA = class(TALStringsA)
  private
    FNodeList: TObjectList<TALHashedStringListDictionaryNodeA>;
    FDictionary: TObjectDictionary<ansiString, TALHashedStringListDictionaryNodeA>;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    FCaseSensitive: boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure QuickSort(L, R: Integer; ACompare: TALHashedStringListSortCompareA);
    procedure SetCaseSensitive(const Value: Boolean);
    function GetCaseSensitive: Boolean;
    Function ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean;
    procedure SetDuplicates(const Value: TDuplicates);
    function CreateDictionary(ACapacity: integer; aCaseSensitive: boolean): TObjectDictionary<ansiString, TALHashedStringListDictionaryNodeA>;
  protected
    function GetName(Index: Integer): AnsiString; override;
    function GetStrictName(Index: Integer): AnsiString; override; // [added from Tstrings]
    function GetValue(const Name: AnsiString): AnsiString; override;
    procedure SetValue(const Name, Value: AnsiString); override;
    function GetValueFromIndex(Index: Integer): AnsiString; override;
    procedure SetValueFromIndex(Index: Integer; const Value: AnsiString); override;
    procedure SetPersistentValue(const Name, Value: AnsiString); override; // [added from Tstrings]
    procedure SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString); override; // [added from Tstrings]
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): AnsiString; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    function GetTextStr: AnsiString; override;
    procedure Put(Index: Integer; const S: AnsiString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure InsertItem(Index: Integer; const Name, Value: AnsiString; AObject: TObject); overload; virtual;
    procedure InsertItem(Index: Integer; const S: AnsiString; AObject: TObject); overload; virtual;
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
    procedure init(OwnsObjects: Boolean; ACapacity: Integer); virtual; //[added from TStringList]
  public
    constructor Create; overload; override;
    constructor Create(OwnsObjects: Boolean); reintroduce; overload;
    constructor Create(ACapacity: Integer); reintroduce; overload; //[added from Tstrings]
    constructor Create(OwnsObjects: Boolean; ACapacity: Integer); reintroduce; overload; //[added from Tstrings]
    destructor Destroy; override;
    function Add(const S: AnsiString): Integer; override;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; override;
    function AddNameValue(const Name, Value: AnsiString): Integer; override; // [added from Tstrings]
    function AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer; override; // [added from Tstrings]
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function  ExtractObject(Index: Integer): TObject; overload; virtual;
    procedure Exchange(Index1, Index2: Integer); override;
    function IndexOf(const S: AnsiString): Integer; override;
    function IndexOfName(const Name: AnsiString): Integer; override; // [added from TStringList]
    procedure Insert(Index: Integer; const S: AnsiString); override;
    procedure InsertObject(Index: Integer; const S: AnsiString; AObject: TObject); override;
    procedure InsertNameValue(Index: Integer; const Name, Value: AnsiString); override; // [added from Tstrings]
    procedure InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject); override; // [added from Tstrings]
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure CustomSort(Compare: TALHashedStringListSortCompareA); virtual;
    property Duplicates: TDuplicates read FDuplicates write SetDuplicates;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

  {------------------}
  TALStringsW = class;

  {---------------------------}
  TALStringsEnumeratorW = class
  private
    FIndex: Integer;
    FStrings: TALStringsW;
  public
    constructor Create(AStrings: TALStringsW);
    function GetCurrent: String;
    function MoveNext: Boolean; inline;
    property Current: String read GetCurrent;
  end;

  {------------------------------}
  TALStringsW = class(TPersistent)
  private
    //[deleted from Tstrings] procedure SetStringsAdapter(const Value: IStringsAdapter);
    //[deleted from Tstrings] FAdapter: IStringsAdapter;
    //[deleted from Tstrings] procedure ReadData(Reader: TReader);
    //[deleted from Tstrings] procedure WriteData(Writer: TWriter);
    FEncoding: TEncoding;
    FDefaultEncoding: TEncoding;
    FDelimiter: Char;
    FLineBreak: String;
    FQuoteChar: Char;
    FNameValueSeparator: Char;
    FStrictDelimiter: Boolean;
    FUpdateCount: Integer;
    FWriteBOM: Boolean;
    FProtectedSave: Boolean;
    function GetCommaText: String;
    function GetDelimitedText: String;
    procedure SetCommaText(const Value: String);
    procedure SetDelimitedText(const Value: String);
    procedure SetDefaultEncoding(const Value: TEncoding);
    function GetIsEmpty: Boolean; inline;
  protected
    //[deleted from Tstrings] procedure DefineProperties(Filer: TFiler); override;
    function GetName(Index: Integer): String; virtual;
    function GetStrictName(Index: Integer): String; virtual; // [added from Tstrings]
    function GetValue(const Name: String): String; virtual;
    procedure SetValue(const Name, Value: String); virtual;
    function GetValueFromIndex(Index: Integer): String; virtual;
    procedure SetValueFromIndex(Index: Integer; const Value: String); virtual;
    procedure SetPersistentValue(const Name, Value: String); virtual; // [added from Tstrings]
    procedure SetPersistentValueFromIndex(Index: Integer; const Value: String); virtual; // [added from Tstrings]
    procedure Error(const Msg: String; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure IndexError(AIndex, AMaxIndex: Integer);
    function ExtractName(const S: String): String;
    function Get(Index: Integer): String; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: String; virtual;
    procedure Put(Index: Integer; const S: String); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetEncoding(const Value: TEncoding); virtual;
    procedure SetTextStr(const Value: String); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
    function CompareStrings(const S1, S2: String): Integer; virtual;
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
  public
    //[deleted from Tstrings] property StringsAdapter: IStringsAdapter read FAdapter write SetStringsAdapter;
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const S: String): Integer; virtual;
    function AddObject(const S: String; AObject: TObject): Integer; virtual;
    function AddNameValue(const Name, Value: String): Integer; virtual; // [added from Tstrings]
    function AddNameValueObject(const Name, Value: String; AObject: TObject): Integer; virtual; // [added from Tstrings]
    procedure Append(const S: String);
    procedure AddStrings(Strings: TALStringsW); overload; virtual;
    procedure AddStrings(const Strings: array of string); overload;
    procedure AddStrings(const Strings: array of string; const Objects: array of TObject); overload;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    function Contains(const S: string): Boolean; inline;
    function ContainsName(const Name: string): Boolean; inline;
    function ContainsObject(const AObject: TObject): Boolean; inline;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TALStringsW): Boolean; reintroduce;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetEnumerator: TALStringsEnumeratorW; inline;
    function GetText: PChar; virtual;
    function IndexOf(const S: String): Integer; virtual;
    function IndexOfName(const Name: String): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: String); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: String; AObject: TObject); virtual;
    procedure InsertNameValue(Index: Integer; const Name, Value: String); virtual; // [added from Tstrings]
    procedure InsertNameValueObject(Index: Integer; const Name, Value: String; AObject: TObject); virtual; // [added from Tstrings]
    procedure LoadFromFile(const FileName: String); overload; virtual;
    procedure LoadFromFile(const FileName: string; Encoding: TEncoding); overload; virtual;
    procedure LoadFromStream(Stream: TStream); overload; virtual;
    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding); overload; virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: String); overload; virtual;
    procedure SaveToFile(const FileName: string; Encoding: TEncoding); overload; virtual;
    procedure SaveToStream(Stream: TStream); overload; virtual;
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding); overload; virtual;
    procedure SetText(Text: PChar); virtual;
    function ToStringArray: TArray<String>;
    function ToObjectArray: TArray<TObject>;
    function ToNameValueArray: TArray<TPair<String, String>>;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: String read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property DefaultEncoding: TEncoding read FDefaultEncoding write SetDefaultEncoding;
    property Delimiter: Char read fDelimiter write fDelimiter;
    property DelimitedText: String read GetDelimitedText write SetDelimitedText;
    property Encoding: TEncoding read FEncoding;
    property LineBreak: String read fLineBreak write fLineBreak;
    property Names[Index: Integer]: String read GetName;
    property StrictNames[Index: Integer]: String read GetStrictName; // [added from Tstrings]
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: Char read fQuoteChar write fQuoteChar;
    property Values[const Name: String]: String read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: String read GetValueFromIndex write SetValueFromIndex;
    property PersistentValues[const Name: String]: String read GetValue write SetPersistentValue; // [added from Tstrings]
    property PersistentValueFromIndex[Index: Integer]: String read GetValueFromIndex write SetPersistentValueFromIndex; // [added from Tstrings]
    property NameValueSeparator: Char read fNameValueSeparator write fNameValueSeparator;
    property StrictDelimiter: Boolean read fStrictDelimiter write fStrictDelimiter;
    property Strings[Index: Integer]: String read Get write Put; default;
    property Text: String read GetTextStr write SetTextStr;
    property WriteBOM: Boolean read FWriteBOM write FWriteBOM;
    property ProtectedSave: Boolean read fProtectedSave write fProtectedSave;
  end;

  {---------------------}
  TALStringListW = class;

  {-------------------------------}
  PALStringItemW = ^TALStringItemW;
  TALStringItemW = record
    FString: String;
    FObject: TObject;
  end;

  {-------------------------------------------}
  TALStringItemListW = array of TALStringItemW;
  TALStringListSortCompareW = reference to function(List: TALStringListW; Index1, Index2: Integer): Integer;

  {---------------------------------}
  TALStringListW = class(TALStringsW)
  private
    FList: TALStringItemListW;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    FNameValueOptimization: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; ACompare: TALStringListSortCompareW);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): String; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: String); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: String): Integer; override;
    procedure InsertItem(Index: Integer; const S: String; AObject: TObject); virtual;
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
    procedure init(OwnsObjects: Boolean); virtual; //[added from TStringList]
    function Find(const S: String; var Index: Integer): Boolean; virtual;
  public
    constructor Create; overload; override;
    constructor Create(OwnsObjects: Boolean); reintroduce; overload;
    destructor Destroy; override;
    function Add(const S: String): Integer; override;
    function AddObject(const S: String; AObject: TObject): Integer; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function  ExtractObject(Index: Integer): TObject; overload; virtual;
    procedure Exchange(Index1, Index2: Integer); override;
    function FindName(const S: String; var Index: Integer): Boolean; // [added from TStringList]
    function IndexOf(const S: String): Integer; override;
    function IndexOfName(const Name: String): Integer; override; // [added from TStringList]
    procedure Insert(Index: Integer; const S: String); override;
    procedure InsertObject(Index: Integer; const S: String; AObject: TObject); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TALStringListSortCompareW); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    property NameValueOptimization: Boolean read FNameValueOptimization write FNameValueOptimization;
  end;

  {-----------------------}
  TALNVStringListW = class;

  {-----------------------------------}
  PALNVStringItemW = ^TALNVStringItemW;
  TALNVStringItemW = record
    FName: String;
    FValue: String;
    FNVS: Boolean;
    FObject: TObject;
  end;

  {-----------------------------------------------}
  TALNVStringItemListW = array of TALNVStringItemW;
  TALNVStringListSortCompareW = reference to function(List: TALNVStringListW; Index1, Index2: Integer): Integer;

  {-----------------------------------}
  TALNVStringListW = class(TALStringsW)
  private
    FList: TALNVStringItemListW;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; ACompare: TALNVStringListSortCompareW);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
    Function ExtractNameValue(const S: String; var Name, Value: String): Boolean; //[added from TStringList]
  protected
    function GetName(Index: Integer): String; override;
    function GetStrictName(Index: Integer): String; override; // [added from Tstrings]
    function GetValue(const Name: String): String; override;
    procedure SetValue(const Name, Value: String); override;
    function GetValueFromIndex(Index: Integer): String; override;
    procedure SetValueFromIndex(Index: Integer; const Value: String); override;
    procedure SetPersistentValue(const Name, Value: String); override; // [added from Tstrings]
    procedure SetPersistentValueFromIndex(Index: Integer; const Value: String); override; // [added from Tstrings]
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): String; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    function GetTextStr: String; override;
    procedure Put(Index: Integer; const S: String); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: String): Integer; override;
    procedure InsertItem(Index: Integer; const S: String; AObject: TObject); overload; virtual;
    procedure InsertItem(Index: Integer; const Name: String; WithNvS: boolean; AObject: TObject); overload; virtual; //[added from TStringList]
    procedure InsertItem(Index: Integer; const Name, Value: String; AObject: TObject); overload; virtual; //[added from TStringList]
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
    procedure init(OwnsObjects: Boolean); virtual; //[added from TStringList]
    function Find(const S: String; var Index: Integer): Boolean; virtual;
  public
    constructor Create; overload; override;
    constructor Create(OwnsObjects: Boolean); reintroduce; overload;
    destructor Destroy; override;
    function Add(const S: String): Integer; override;
    function AddObject(const S: String; AObject: TObject): Integer; override;
    function AddNameValue(const Name, Value: String): Integer; override; // [added from Tstrings]
    function AddNameValueObject(const Name, Value: String; AObject: TObject): Integer; override; // [added from Tstrings]
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function  ExtractObject(Index: Integer): TObject; overload; virtual;
    procedure Exchange(Index1, Index2: Integer); override;
    function FindName(const Name: String; var Index: Integer): Boolean; overload; // [added from TStringList]
    function FindName(const Name: String; WithNvS: boolean; var Index: Integer): Boolean; overload; // [added from TStringList]
    function FindNameValue(const Name, Value: String; var Index: Integer): Boolean;  // [added from TStringList]
    function IndexOf(const S: String): Integer; override;
    function IndexOfName(const Name: String): Integer; override; // [added from TStringList]
    procedure Insert(Index: Integer; const S: String); override;
    procedure InsertObject(Index: Integer; const S: String; AObject: TObject); override;
    procedure InsertNameValue(Index: Integer; const Name, Value: String); override; // [added from Tstrings]
    procedure InsertNameValueObject(Index: Integer; const Name, Value: String; AObject: TObject); override; // [added from Tstrings]
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TALNVStringListSortCompareW); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

implementation

Uses
  System.RTLConsts,
  system.IOUtils,
  System.Ansistrings,
  System.Hash,
  Alcinoe.StringUtils,
  Alcinoe.Common;

{**************************************************************}
constructor TALStringsEnumeratorA.Create(AStrings: TALStringsA);
begin
  inherited Create;
  FIndex := -1;
  FStrings := AStrings;
end;

{****************************************************}
function TALStringsEnumeratorA.GetCurrent: AnsiString;
begin
  Result := FStrings[FIndex];
end;

{***********************************************}
function TALStringsEnumeratorA.MoveNext: Boolean;
begin
  Result := FIndex < FStrings.Count - 1;
  if Result then
    Inc(FIndex);
end;

{*****************************}
constructor TALStringsA.Create;
begin
  inherited Create;
  FDelimiter := ',';
  FLineBreak := sLineBreak;
  FQuoteChar := '"';
  FNameValueSeparator := '=';
  FStrictDelimiter:= False;
  FUpdateCount:= 0;
  fProtectedSave := False;
end;

{*****************************************************}
function TALStringsA.Add(const S: AnsiString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

{*****************************************************************************}
function TALStringsA.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

{************************************************************************}
function TALStringsA.AddNameValue(const Name, Value: AnsiString): Integer;
begin
  result := add(name + NameValueSeparator + Value);
end;

{************************************************************************************************}
function TALStringsA.AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer;
begin
  result := addObject(name + NameValueSeparator + Value, AObject);
end;

{************************************************}
procedure TALStringsA.Append(const S: AnsiString);
begin
  Add(S);
end;

{*****************************************************}
procedure TALStringsA.AddStrings(Strings: TALStringsA);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

{*******************************************************************}
procedure TALStringsA.AddStrings(const Strings: array of AnsiString);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := Low(Strings) to High(Strings) do
      Add(Strings[I]);
  finally
    EndUpdate;
  end;
end;

{****************************************************************************************************}
procedure TALStringsA.AddStrings(const Strings: array of AnsiString; const Objects: array of TObject);
var
  I: Integer;
begin
  if Length(Strings) <> Length(Objects) then
    raise EArgumentOutOfRangeException.CreateRes(@System.RTLConsts.sInvalidStringAndObjectArrays);
  BeginUpdate;
  try
    for I := Low(Strings) to High(Strings) do
      AddObject(Strings[I], Objects[I]);
  finally
    EndUpdate;
  end;
end;

{************************************************}
procedure TALStringsA.Assign(Source: TPersistent);
var I: integer;
begin
  if Source is TALStringsA then
  begin
    BeginUpdate;
    try
      Clear;
      NameValueSeparator := TALStringsA(Source).NameValueSeparator;
      QuoteChar := TALStringsA(Source).QuoteChar;
      Delimiter := TALStringsA(Source).Delimiter;
      LineBreak := TALStringsA(Source).LineBreak;
      StrictDelimiter := TALStringsA(Source).StrictDelimiter;
      AddStrings(TALStringsA(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end;
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      Clear;
      NameValueSeparator := AnsiChar(TStrings(Source).NameValueSeparator);
      QuoteChar := AnsiChar(TStrings(Source).QuoteChar);
      Delimiter := AnsiChar(TStrings(Source).Delimiter);
      LineBreak := AnsiString(TStrings(Source).LineBreak);
      StrictDelimiter := TStrings(Source).StrictDelimiter;
      for I := 0 to Tstrings(Source).Count - 1 do
        AddObject(Ansistring(Tstrings(Source)[I]), Tstrings(Source).Objects[I]);
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

{************************************************}
procedure TALStringsA.AssignTo(Dest: TPersistent);
var I: integer;
begin
  if Dest is TStrings then
  begin
    Tstrings(Dest).BeginUpdate;
    try
      Tstrings(Dest).Clear;
      Tstrings(Dest).NameValueSeparator := char(NameValueSeparator);
      Tstrings(Dest).QuoteChar := char(QuoteChar);
      Tstrings(Dest).Delimiter := char(Delimiter);
      Tstrings(Dest).LineBreak := String(LineBreak);
      Tstrings(Dest).StrictDelimiter := StrictDelimiter;
      for I := 0 to Count - 1 do
        Tstrings(Dest).AddObject(string(get(I)), Objects[I]);
    finally
      Tstrings(Dest).EndUpdate;
    end;
    Exit;
  end;
  inherited AssignTo(Dest);
end;

{********************************}
procedure TALStringsA.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

{******************************}
procedure TALStringsA.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

{*********************************************************}
function TALStringsA.Equals(Strings: TALStringsA): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

{$IFOPT O+}
  // Turn off optimizations to force creating a EBP stack frame and
  // place params on the stack.
  {$DEFINE OPTIMIZATIONSON}
  {$O-}
{$ENDIF O+}

{************************************************************}
procedure TALStringsA.Error(const Msg: String; Data: Integer);
begin
  raise EStringListError.CreateFmt(Msg, [Data]) at
    PPointer(PByte(@Msg) + SizeOf(Msg) + SizeOf(Self) + SizeOf(Pointer))^;
end;

{*************************************************************}
procedure TALStringsA.Error(Msg: PResStringRec; Data: Integer);
begin
  raise EStringListError.CreateFmt(LoadResString(Msg), [Data]) at
    PPointer(PByte(@Msg) + SizeOf(Msg) + SizeOf(Self) + SizeOf(Pointer))^;
end;

{***********************************************************}
procedure TALStringsA.IndexError(AIndex, AMaxIndex: Integer);
begin
  raise EStringListError.Create(ListIndexErrorMsg(AIndex, AMaxIndex, Self)) at ReturnAddress;
end;

{$IFDEF OPTIMIZATIONSON}
  {$UNDEF OPTIMIZATIONSON}
  {$O+}
{$ENDIF OPTIMIZATIONSON}

{******************************************************}
procedure TALStringsA.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: AnsiString;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

{****************************************************************}
function TALStringsA.ExtractName(const S: AnsiString): AnsiString;
var
  P: Integer;
begin
  Result := S;
  P := ALPosA(NameValueSeparator, Result);

  // change behavior from original Tstring
  // i thing that if a Tstring have an item
  //
  // item1
  //
  // then set MyTstrings.values[item1] := Value1
  // must do
  //
  // item1=Value1
  //
  // instead of what he actually do
  //
  // item1
  // item1=Value1
  //
  // also when MyTStrings contain
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4
  // item5=Value5
  //
  // then doing
  // MyTStrings.valueFromIndex[4] := 'value4'
  // must result in
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4=Value4
  // item5=Value5
  //
  // instead of the current behavior of Tstrings
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // =Value4
  // item5=Value5
  //
  //
  // Original function:
  //
  // if P <> 0 then
  //   SetLength(Result, P-1) else
  //   SetLength(Result, 0);

  if P <> 0 then SetLength(Result, P-1);
end;

{****************************************}
function TALStringsA.GetCapacity: Integer;
begin  // descendents may optionally override/replace this default implementation
  Result := Count;
end;

{********************************************}
function TALStringsA.GetCommaText: AnsiString;
var
  LOldDelimiter: AnsiChar;
  LOldQuoteChar: AnsiChar;
begin
  LOldDelimiter := Delimiter;
  LOldQuoteChar := QuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    Result := GetDelimitedText;
  finally
    Delimiter := LOldDelimiter;
    QuoteChar := LOldQuoteChar;
  end;
end;

{************************************************}
function TALStringsA.GetDelimitedText: AnsiString;
var
  S: AnsiString;
  P: PAnsiChar;
  I, Count: Integer;
  LDelimiters: TSysCharSet;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := QuoteChar + QuoteChar
  else
  begin
    Result := '';
    LDelimiters := [AnsiChar(#0), AnsiChar(QuoteChar), AnsiChar(Delimiter)];
    if not StrictDelimiter then
      LDelimiters := LDelimiters + [AnsiChar(#1)..AnsiChar(' ')];
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PAnsiChar(S);
      while not (P^ in LDelimiters) do
        Inc(P);
      if (P^ <> #0) then S := ALQuotedStr(S, QuoteChar);
      Result := Result + S + Delimiter;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

{********************************************************}
function TALStringsA.GetEnumerator: TALStringsEnumeratorA;
begin
  Result := TALStringsEnumeratorA.Create(Self);
end;

{***************************************}
function TALStringsA.GetIsEmpty: Boolean;
begin
  Result := GetCount = 0;
end;

{*******************************************************}
function TALStringsA.GetName(Index: Integer): AnsiString;
begin
  Result := ExtractName(Get(Index));
end;

{*************************************************************}
function TALStringsA.GetStrictName(Index: Integer): AnsiString;
var P: Integer;
begin
  Result := Get(Index);
  P := ALPosA(NameValueSeparator, Result);
  if P <> 0 then SetLength(Result, P-1)
  else SetLength(Result, 0);
end;

{******************************************************}
function TALStringsA.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

{**************************************}
function TALStringsA.GetText: PAnsiChar;
begin
  Result := System.Ansistrings.StrNew(PAnsiChar(GetTextStr));
end;

{******************************************}
function TALStringsA.GetTextStr: AnsiString;
var
  I, L, Size, Count: Integer;
  P: PAnsiChar;
  S, LB: AnsiString;
begin
  Count := GetCount;
  Size := 0;
  LB := LineBreak;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      ALMove(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      ALMove(Pointer(LB)^, P^, L);
      Inc(P, L);
    end;
  end;
end;

{****************************************************************}
function TALStringsA.GetValue(const Name: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := ALCopyStr(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

{*********************************************************}
function TALStringsA.IndexOf(const S: AnsiString): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

{****************************************************************}
function TALStringsA.IndexOfName(const Name: AnsiString): Integer;
var
  P: Integer;
  S: AnsiString;
begin

  // change behavior from original Tstring
  // i thing that if a Tstring have an item
  //
  // item1
  //
  // then set MyTstrings.values[item1] := Value1
  // must do
  //
  // item1=Value1
  //
  // instead of what he actually do
  //
  // item1
  // item1=Value1
  //
  // also when MyTStrings contain
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4
  // item5=Value5
  //
  // then doing
  // MyTStrings.valueFromIndex[4] := 'value4'
  // must result in
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4=Value4
  // item5=Value5
  //
  // instead of the current behavior of Tstrings
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // =Value4
  // item5=Value5
  //
  //
  // Original function:
  //
  // for Result := 0 to GetCount - 1 do
  // begin
  //   S := Get(Result);
  //   P := ALPosA(NameValueSeparator, S);
  //   if (P <> 0) and (CompareStrings(ALCopyStr(S, 1, P - 1), Name) = 0) then Exit;
  // end;
  // Result := -1;

  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := ALPosA(NameValueSeparator, S);
    if ((P <> 0) and (CompareStrings(ALCopyStr(S, 1, P - 1), Name) = 0)) or
       ((P = 0) and (CompareStrings(S, Name) = 0)) then Exit;
  end;
  Result := -1;
end;

{************************************************************}
function TALStringsA.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{**********************************************************}
function TALStringsA.Contains(const S: AnsiString): Boolean;
begin
  Result := IndexOf(S) >= 0;
end;

{*****************************************************************}
function TALStringsA.ContainsName(const Name: AnsiString): Boolean;
begin
  Result := IndexOfName(Name) >= 0;
end;

{*******************************************************************}
function TALStringsA.ContainsObject(const AObject: TObject): Boolean;
begin
  Result := IndexOfObject(AObject) >= 0;
end;

{****************************************************************************************}
procedure TALStringsA.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

{***********************************************************************************}
procedure TALStringsA.InsertNameValue(Index: Integer; const Name, Value: AnsiString);
begin
  Insert(Index, name + NameValueSeparator + Value);
end;

{***********************************************************************************************************}
procedure TALStringsA.InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
begin
  InsertObject(Index, name + NameValueSeparator + Value, AObject);
end;

{*************************************************************}
procedure TALStringsA.LoadFromFile(const FileName: AnsiString);
begin
  LoadFromFile(String(FileName));
end;

{*********************************************************}
procedure TALStringsA.LoadFromFile(const FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(String(FileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    ALFreeAndNil(Stream);
  end;
end;

{****************************************************}
procedure TALStringsA.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  S: AnsiString;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size);
    Stream.ReadBuffer(Pointer(S)^, Size);
    SetTextStr(S);
  finally
    EndUpdate;
  end;
end;

{******************************************************}
procedure TALStringsA.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: AnsiString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      PutObject(CurIndex, nil);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

{*************************************************************}
procedure TALStringsA.Put(Index: Integer; const S: AnsiString);
begin
end;

{****************************************************************}
procedure TALStringsA.PutObject(Index: Integer; AObject: TObject);
begin
end;

{***********************************************************}
procedure TALStringsA.SaveToFile(const FileName: AnsiString);
begin
  SaveToFile(String(FileName));
end;

{*******************************************************}
procedure TALStringsA.SaveToFile(const FileName: String);
Var LFileStream: TfileStream;
    LTmpFilename: String;
begin
  if ProtectedSave then LTmpFilename := FileName + '.~tmp'
  else LTmpFilename := FileName;
  try

    LFileStream := TfileStream.Create(LTmpFilename,fmCreate);
    Try
      SaveToStream(LFileStream);
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

{**************************************************}
procedure TALStringsA.SaveToStream(Stream: TStream);
var
  S: AnsiString;
begin
  S := GetTextStr;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

{******************************************************}
procedure TALStringsA.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendents may optionally implement this method
end;

{**********************************************************}
procedure TALStringsA.SetCommaText(const Value: AnsiString);
var
  LOldDelimiter: AnsiChar;
  LOldQuoteChar: AnsiChar;
begin
  LOldDelimiter := Delimiter;
  LOldQuoteChar := QuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    SetDelimitedText(Value);
  finally
    Delimiter := LOldDelimiter;
    QuoteChar := LOldQuoteChar;
  end;
end;

{*********************************************}
procedure TALStringsA.SetText(Text: PAnsiChar);
begin
  SetTextStr(Text);
end;

{********************************************************}
procedure TALStringsA.SetTextStr(const Value: AnsiString);
var
  P, PCurVal, PCurLB, PStartVal, PEndVal, PStartLB, PEndLB: PAnsiChar;
  S: AnsiString;
  LineBreakLen: Integer;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P = nil then
      Exit;

    LineBreakLen := Length(LineBreak);
    if LineBreakLen = 0 then
    begin
      Add(Value);
      Exit;
    end;

    PEndVal := P + Length(Value);

    // When LineBreak is:
    // * sLineBreak - for compatibility with Windows, Posix and old macOS platforms,
    //   we handle #13#10, #10 and #13 as it would be #13#10.
    // * NOT sLineBreak - we use strict checking for LineBreak.
    if ALCompareStrA(LineBreak, sLineBreak) = 0 then
    begin
      while P < PEndVal do
      begin
        PStartVal := P;
        while (P < PEndVal) and not (P^ in [#10, #13]) do Inc(P);
        SetString(S, PStartVal, P - PStartVal);
        Add(S);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
      Exit;
    end;

    PStartLB := Pointer(LineBreak);
    PEndLB := PStartLB + LineBreakLen;
    PCurLB := PStartLB;
    PStartVal := P;

    while P < PEndVal do
    begin
      while (P < PEndVal) and (P^ <> PStartLB^) do
        Inc(P);
      if P < PEndVal then
      begin
        PCurVal := P + 1;
        Inc(PCurLB);
        while (PCurLB < PEndLB) and (PCurVal < PEndVal) and (PCurVal^ = PCurLB^) do
        begin
          Inc(PCurVal);
          Inc(PCurLB)
        end;
        if PCurLB = PEndLB then
        begin
          SetString(S, PStartVal, P - PStartVal);
          Add(S);
          P := PCurVal;
          PStartVal := P;
        end
        else
          Inc(P);
        PCurLB := PStartLB;
      end;
    end;

    if P > PStartVal then
    begin
      SetString(S, PStartVal, P - PStartVal);
      Add(S);
    end;
  finally
    EndUpdate;
  end;
end;

{******************************************************}
procedure TALStringsA.SetUpdateState(Updating: Boolean);
begin
end;

{************************************************************}
procedure TALStringsA.SetValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then Add(Name + NameValueSeparator + Value)
    else Put(I, Name + NameValueSeparator + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{**********************************************************************}
procedure TALStringsA.SetPersistentValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then Add(Name + NameValueSeparator + Value)
  else Put(I, Name + NameValueSeparator + Value);
end;

{**************************************************************}
procedure TALStringsA.SetDelimitedText(const Value: AnsiString);
var
  P, P1: PAnsiChar;
  S: AnsiString;
begin
  BeginUpdate;
  try
    Clear;
    P := PAnsiChar(Value);
    if not StrictDelimiter then
      while P^ in [#1..' '] do
        Inc(P);
    while P^ <> #0 do
    begin
      if P^ = QuoteChar then
        S := ALExtractQuotedStr(P, QuoteChar)
      else
      begin
        P1 := P;
        while ((not StrictDelimiter and (P^ > ' ')) or
              (StrictDelimiter and (P^ <> #0))) and (P^ <> Delimiter) do
          Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);
      if not StrictDelimiter then
        while P^ in [#1..' '] do
          Inc(P);

      if P^ = Delimiter then
      begin
        P1 := P;
        Inc(P1);
        if P1^ = #0 then
          Add('');
        repeat
          Inc(P);
        until not (not StrictDelimiter and (P^ in [#1..' ']));
      end;
    end;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************}
function TALStringsA.CompareStrings(const S1, S2: AnsiString): Integer;
begin
  Result := ALCompareTextA(S1, S2);
end;

{*****************************************************************}
function TALStringsA.GetValueFromIndex(Index: Integer): AnsiString;
var
  SepPos: Integer;
begin
  if Index >= 0 then
  begin
    Result := Get(Index);
    SepPos := ALPosA(NameValueSeparator, Result);
    if (SepPos > 0) then
      System.Delete(Result, 1, SepPos)
    else
      Result := '';
  end
  else
    Result := '';
end;

{*******************************************************************************}
procedure TALStringsA.SetValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Value <> '' then
  begin
    if Index < 0 then Add(NameValueSeparator + Value)
    else Put(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

{*****************************************************************************************}
procedure TALStringsA.SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Index < 0 then Add(NameValueSeparator + Value)
  else Put(Index, Names[Index] + NameValueSeparator + Value);
end;

{*****************************************************}
function TALStringsA.ToStringArray: TArray<AnsiString>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Strings[I];
end;

{**************************************************}
function TALStringsA.ToObjectArray: TArray<TObject>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Objects[I];
end;

{***************************************************************************}
function TALStringsA.ToNameValueArray: TArray<TPair<AnsiString, AnsiString>>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := TPair<AnsiString, AnsiString>.create(Names[i], ValueFromIndex[I]);
end;

{********************************}
destructor TALStringListA.Destroy;
var
  I: Integer;
  Temp: Array of TObject;
begin
  FOnChange := nil;
  FOnChanging := nil;

  // If the list owns the Objects gather them and free after the list is disposed
  if OwnsObjects then
  begin
    SetLength(Temp, FCount);
    for I := 0 to FCount - 1 do
      Temp[I] := FList[I].FObject;
  end;

  inherited Destroy;
  FCount := 0;
  SetCapacity(0);

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      ALFreeAndNil(Temp[I]);
end;

{********************************************************}
function TALStringListA.Add(const S: AnsiString): Integer;
begin
  Result := AddObject(S, nil);
end;

{********************************************************************************}
function TALStringListA.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0);
      end;
  InsertItem(Result, S, AObject);
end;

{***************************************************}
procedure TALStringListA.Assign(Source: TPersistent);
begin
  if Source is TALStringListA then
  begin
    Clear;
    FCaseSensitive := TALStringListA(Source).FCaseSensitive;
    FDuplicates := TALStringListA(Source).FDuplicates;
    FSorted := TALStringListA(Source).FSorted;
  end
  else if Source is TALNVStringListA then
  begin
    Clear;
    FCaseSensitive := TALNVStringListA(Source).FCaseSensitive;
    FDuplicates := TALNVStringListA(Source).FDuplicates;
    FSorted := TALNVStringListA(Source).FSorted;
  end
  else if Source is TALHashedStringListA then
  begin
    Clear;
    FCaseSensitive := TALHashedStringListA(Source).CaseSensitive;
    FDuplicates := TALHashedStringListA(Source).FDuplicates;
    FSorted := False;
  end
  else if Source is TALAVLStringListA then
  begin
    Clear;
    FCaseSensitive := TALAVLStringListA(Source).CaseSensitive;
    FDuplicates := TALAVLStringListA(Source).FDuplicates;
    FSorted := False;
  end
  else if Source is TStringList then
  begin
    Clear;
    CaseSensitive := TStringList(Source).CaseSensitive;
    Duplicates := TStringList(Source).Duplicates;
    Sorted := TStringList(Source).Sorted;
  end;
  inherited Assign(Source);
end;

{***************************************************}
procedure TALStringListA.AssignTo(Dest: TPersistent);
begin
  if Dest is TStringList then
  begin
    TStringList(Dest).clear;
    TStringList(Dest).CaseSensitive := fCaseSensitive;
    TStringList(Dest).Duplicates := fDuplicates;
    TStringList(Dest).Sorted := fSorted;
  end;
  inherited AssignTo(Dest);
end;

{*******************************}
procedure TALStringListA.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{********************************}
procedure TALStringListA.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{*****************************}
procedure TALStringListA.Clear;
var
  I: Integer;
  Temp: Array of TObject;
begin
  if FCount <> 0 then
  begin
    Changing;

    // If the list owns the Objects gather them and free after the list is disposed
    if OwnsObjects then
    begin
      SetLength(Temp, FCount);
      for I := 0 to FCount - 1 do
        Temp[I] := FList[I].FObject;
    end;

    FCount := 0;
    SetCapacity(0);

    // Free the objects that were owned by the list
    if Length(Temp) > 0 then
      for I := 0 to Length(Temp) - 1 do
        ALFreeAndNil(Temp[I]);

    Changed;
  end;
end;

{**********************************************}
procedure TALStringListA.Delete(Index: Integer);
var
  Obj: TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index, FCount - 1);
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  // Direct memory writing to managed array follows
  //  see http://dn.embarcadero.com/article/33423
  // Explicitly finalize the element we about to stomp on with move
  Finalize(FList[Index]);
  Dec(FCount);
  if Index < FCount then
  begin
    ALMove(
      FList[Index + 1],
      FList[Index],
      (FCount - Index) * SizeOf(TALStringItemA));
    // Make sure there is no danglng pointer in the last (now unused) element
    PPointer(@FList[FCount].FString)^ := nil;
    PPointer(@FList[FCount].FObject)^ := nil;
  end;
  if Obj <> nil then
    ALFreeAndNil(Obj);
  Changed;
end;

{**************************************************************}
function  TALStringListA.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index, FCount - 1);
  Changing;
  result := FList[Index].FObject;
  FList[Index].FObject := nil;
  Changed;
end;

{*********************************************************}
procedure TALStringListA.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    IndexError(Index1, FCount - 1);
  if (Index2 < 0) or (Index2 >= FCount) then
    IndexError(Index2, FCount - 1);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{**************************************************************}
procedure TALStringListA.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Pointer;
  Item1, Item2: PALStringItemA;
begin
  Item1 := @FList[Index1];
  Item2 := @FList[Index2];
  Temp := Pointer(Item1^.FString);
  Pointer(Item1^.FString) := Pointer(Item2^.FString);
  Pointer(Item2^.FString) := Temp;
  Temp := Pointer(Item1^.FObject);
  Pointer(Item1^.FObject) := Pointer(Item2^.FObject);
  Pointer(Item2^.FObject) := Temp;
end;

{*****************************************************************************}
function TALStringListA.Find(const S: AnsiString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FString, S);
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

{*********************************************************************************}
function TALStringListA.FindName(const S: AnsiString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(ExtractName(FList[I].FString), S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        //if Duplicates <> dupAccept then L := I; //because we need the last value of name in any case
                                                  //ex: a
                                                  //    aaa
                                                  //    aaa=
                                                  //    bbb
                                                  //then doing values['aaa'] := 'xxx' must result in
                                                  //    a
                                                  //    aaa
                                                  //    aaa=xxx
                                                  //    bbb
                                                  //and not in
                                                  //    a
                                                  //    aaa=xxx
                                                  //    aaa=
                                                  //    bbb
        L := I; // this mean L > H
        I := I + 1;
        while I <= FCount - 1 do begin
          if CompareStrings(ExtractName(FList[I].FString), s) = 0 then L := I
          else break;
          I := I + 1;
        end;
      end;
    end;
  end;
  Index := L;
end;

{******************************************************}
function TALStringListA.Get(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  Result := FList[Index].FString;
end;

{*******************************************}
function TALStringListA.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

{****************************************}
function TALStringListA.GetCount: Integer;
begin
  Result := FCount;
end;

{*********************************************************}
function TALStringListA.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  Result := FList[Index].FObject;
end;

{****************************}
procedure TALStringListA.Grow;
{$IF CompilerVersion <= 32}{tokyo}
var
  Delta: Integer;
{$endif}
begin
  {$IF CompilerVersion <= 32}{tokyo}
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
  {$else}
  SetCapacity(GrowCollection(FCapacity, FCount + 1));
  {$endif}
end;

{************************************************************}
function TALStringListA.IndexOf(const S: AnsiString): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

{*******************************************************************}
function TALStringListA.IndexOfName(const Name: ansistring): Integer;
begin
  if (not Sorted) or (not FNameValueOptimization) then Result := inherited IndexOfName(Name)
  else if not FindName(Name, Result) then Result := -1;
end;

{*******************************************************************}
procedure TALStringListA.Insert(Index: Integer; const S: AnsiString);
begin
  InsertObject(Index, S, nil);
end;

{*******************************************************************************************}
procedure TALStringListA.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then
    IndexError(Index, FCount);
  InsertItem(Index, S, AObject);
end;

{*********************************************************}
procedure TALStringListA.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: AnsiString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      FList[CurIndex].FObject := nil;
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
procedure TALStringListA.InsertItem(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    ALMove(
      FList[Index],
      FList[Index + 1],
      (FCount - Index) * SizeOf(TALStringItemA));
  Pointer(FList[Index].FString) := nil;
  Pointer(FList[Index].FObject) := nil;
  FList[Index].FObject := AObject;
  FList[Index].FString := S;
  Inc(FCount);
  Changed;
end;

{****************************************************************}
procedure TALStringListA.Put(Index: Integer; const S: AnsiString);
begin
  if not sorted then begin
    if Cardinal(Index) >= Cardinal(FCount) then
      IndexError(Index, FCount - 1);
    Changing;
    FList[Index].FString := S;
    Changed;
  end
  else begin
    delete(index);
    add(s);
  end;
end;

{*******************************************************************}
procedure TALStringListA.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  FList[Index].FObject := AObject;

  if Obj <> nil then
    ALFreeAndNil(Obj);

  Changed;
end;

{*************************************************************************************}
procedure TALStringListA.QuickSort(L, R: Integer; ACompare: TALStringListSortCompareA);
var
  I, J, P: Integer;
begin
  while L < R do
  begin
    if (R - L) = 1 then
    begin
      if ACompare(Self, L, R) > 0 then
        ExchangeItems(L, R);
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
          ExchangeItems(I, J);
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

{*********************************************************}
procedure TALStringListA.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{*************************************************}
procedure TALStringListA.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

{*********************************************************}
procedure TALStringListA.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{*******************************************************************************************}
function ALStringListCompareStringsA(List: TALStringListA; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(
              List.FList[Index1].FString,
              List.FList[Index2].FString);
end;

{****************************}
procedure TALStringListA.Sort;
begin
  CustomSort(ALStringListCompareStringsA);
end;

{**********************************************************************}
procedure TALStringListA.CustomSort(Compare: TALStringListSortCompareA);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

{************************************************************************}
function TALStringListA.CompareStrings(const S1, S2: AnsiString): Integer;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function internalCompareStr(const S1, S2: AnsiString): Integer;
  var
    P1, P2: PAnsiChar;
    I: Integer;
    L1, L2: Integer;
  begin
    { Length and PChar of S1 }
    L1 := Length(S1);
    P1 := PAnsiChar(S1);

    { Length and PChar of S2 }
    L2 := Length(S2);
    P2 := PAnsiChar(S2);

    { Continue the loop until the end of one string is reached. }
    I := 0;
    while (I < L1) and (I < L2) do
    begin
      if (P1^ <> P2^) then begin
        if (P1^ = NameValueSeparator) then result := -1
        else if (P2^ = NameValueSeparator) then result := 1
        else result := Ord(P1^) - Ord(P2^);
        Exit;
      end;

      Inc(P1);
      Inc(P2);
      Inc(I);
    end;

    { If chars were not different return the difference in length }
    Result := L1 - L2;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function internalCompareText(const S1, S2: AnsiString): Integer;
  var
    P1, P2: PAnsiChar;
    I: Integer;
    C1, C2: AnsiChar;
    L1, L2: Integer;
  begin
    { Length and PChar of S1 }
    L1 := Length(S1);
    P1 := PAnsiChar(S1);

    { Length and PChar of S2 }
    L2 := Length(S2);
    P2 := PAnsiChar(S2);

    { Continue the loop until the end of one string is reached. }
    I := 0;
    while (I < L1) and (I < L2) do
    begin
      if P1^ in ['a'..'z'] then
        C1 := AnsiChar(Byte(P1^) xor $20)
      else
        C1 := P1^;

      if P2^ in ['a'..'z'] then
        C2 := AnsiChar(Byte(P2^) xor $20)
      else
        C2 := P2^;

      if (C1 <> C2) then begin
        if (C1 = NameValueSeparator) then result := -1
        else if (C2 = NameValueSeparator) then result := 1
        else result := Ord(C1) - Ord(C2);
        Exit;
      end;

      Inc(P1);
      Inc(P2);
      Inc(I);
    end;

    { If chars were not different return the difference in length }
    Result := L1 - L2;
  end;

begin

  // Orignial Delphi Code
  // the difference between TALStringListA and TStringList is that
  // TstringList use ansiCompareStr or ansiCompareText that are
  // dependant from the local. I don't like this behavior because
  // as you can read
  // http://msdn.microsoft.com/en-us/library/windows/desktop/dd317759(v=vs.85).aspx
  // "Using CompareString incorrectly can compromise the security of your
  // application. Strings that are not compared correctly can produce
  // invalid input. For example, the function can raise security issues when
  // used for a non-linguistic comparison, because two strings that are
  // distinct in their binary representation can be linguistically equivalent"
  // so i prefere to use instead CompareStr and CompareText but only
  // a..z = A..Z will be handle when case insensitive is set.
  // other behavior must be handle in descendant classe
  //
  // also not the ansiCompareStr and ansiCompareText
  // are 10x more slower than CompareStr and CompareText
  //
  // if CaseSensitive then
  //   Result := AnsiCompareStr(S1, S2)
  // else
  //   Result := AnsiCompareText(S1, S2);

  // it's important that the order is not change because
  // of the ord(NameValueSeparator) this is need because of
  // function FindName
  //
  // EX the items
  //
  //   aaa0
  //   aaa=123
  //   aaaa
  //
  // must be ordered like
  //
  //   aaa=123   |     aaa
  //   aaa0      |     aaa0
  //   aaaa      |     aaaa
  //                   => OK, ordered work with findname
  //
  // but with just Result := ALCompareTextA(S1, S2)
  // it's will be ordered like
  //
  //   aaa0      |     aaa0
  //   aaa=123   |     aaa
  //   aaaa      |     aaaa
  //                   => KO, NOT ordered, break the findname
  //
  // so we need to give to the NameValueSeparator the lowest ASCII
  // number (#0). for this we must use custom CompareStr and
  // custom CompareText. the cost is that our custom implementation
  // of internalCompareStr and internalCompareText (based on the pure
  // pascal implementation of CompareStr and CompareText, will be aound
  // 50% more slower than the ASM version of CompareStr and CompareText
  // IE 50% more slower with average 50 bytes for S1 and S2, it's even
  // more slower when the number of bytes in S1 and S2 increase

  if  fNameValueOptimization then begin
    if CaseSensitive then
      Result := InternalCompareStr(S1, S2)
    else
      Result := InternalCompareText(S1, S2);
  end
  else begin
    if CaseSensitive then
      Result := ALCompareStrA(S1, S2)
    else
      Result := ALCompareTextA(S1, S2);
  end;
end;

{**************************************************}
procedure TALStringListA.init(OwnsObjects: Boolean);
begin
  setlength(FList, 0);
  FCount := 0;
  FCapacity := 0;
  FSorted := False;
  FDuplicates := dupIgnore;
  FCaseSensitive := False;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := OwnsObjects;
  FNameValueOptimization := True;
end;

{********************************}
constructor TALStringListA.Create;
begin
  inherited Create;
  init(False);
end;

{******************************************************}
constructor TALStringListA.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects);
end;

{**************************************************************}
procedure TALStringListA.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then
    begin
      // Calling Sort won't sort the list because CustomSort will
      // only sort the list if it's not already sorted
      Sorted := False;
      Sorted := True;
    end;
  end;
end;

{**********************************}
destructor TALNVStringListA.Destroy;
var
  I: Integer;
  Temp: Array of TObject;
begin
  FOnChange := nil;
  FOnChanging := nil;

  // If the list owns the Objects gather them and free after the list is disposed
  if OwnsObjects then
  begin
    SetLength(Temp, FCount);
    for I := 0 to FCount - 1 do
      Temp[I] := FList[I].FObject;
  end;

  inherited Destroy;
  FCount := 0;
  SetCapacity(0);

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      ALFreeAndNil(Temp[I]);
end;

{**********************************************************}
function TALNVStringListA.Add(const S: AnsiString): Integer;
begin
  Result := AddObject(S, nil);
end;

{**********************************************************************************}
function TALNVStringListA.AddObject(const S: AnsiString; AObject: TObject): Integer;
Var LName, LValue: AnsiString;
begin
  if not Sorted then begin
    Result := FCount;
    InsertItem(Result, S, AObject);
  end
  else begin
    if ExtractNameValue(S, LName, LValue) then begin
      if FindNameValue(LName, LValue, Result) then
        case Duplicates of
          dupIgnore: Exit;
          dupError: Error(@SDuplicateString, 0);
        end;
      InsertItem(Result, LName, LValue, AObject);
    end
    else begin
      if FindName(s, false{WithNvS}, Result) then
        case Duplicates of
          dupIgnore: Exit;
          dupError: Error(@SDuplicateString, 0);
        end;
      InsertItem(Result, s, False{WithNvS}, AObject);
    end;
  end;
end;

{*****************************************************************************}
function TALNVStringListA.AddNameValue(const Name, Value: AnsiString): Integer;
begin
  Result := AddNameValueObject(Name, Value, nil);
end;

{*****************************************************************************************************}
function TALNVStringListA.AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer;
begin
  if not Sorted then begin
    Result := FCount;
  end
  else begin
    if FindNameValue(Name, Value, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0);
      end;
  end;
  InsertItem(Result, Name, Value, AObject);
end;

{*****************************************************}
procedure TALNVStringListA.Assign(Source: TPersistent);
begin
  if Source is TALNVStringListA then
  begin
    Clear;
    FCaseSensitive := TALNVStringListA(Source).FCaseSensitive;
    FDuplicates := TALNVStringListA(Source).FDuplicates;
    FSorted := TALNVStringListA(Source).FSorted;
  end
  else if Source is TALStringListA then
  begin
    Clear;
    FCaseSensitive := TALStringListA(Source).FCaseSensitive;
    FDuplicates := TALStringListA(Source).FDuplicates;
    FSorted := TALStringListA(Source).FSorted;
  end
  else if Source is TALHashedStringListA then
  begin
    Clear;
    FCaseSensitive := TALHashedStringListA(Source).CaseSensitive;
    FDuplicates := TALHashedStringListA(Source).FDuplicates;
    FSorted := False;
  end
  else if Source is TALAVLStringListA then
  begin
    Clear;
    FCaseSensitive := TALAVLStringListA(Source).CaseSensitive;
    FDuplicates := TALAVLStringListA(Source).FDuplicates;
    FSorted := False;
  end
  else if Source is TStringList then
  begin
    Clear;
    CaseSensitive := TStringList(Source).CaseSensitive;
    Duplicates := TStringList(Source).Duplicates;
    Sorted := TStringList(Source).Sorted;
  end;
  inherited Assign(Source);
end;

{*****************************************************}
procedure TALNVStringListA.AssignTo(Dest: TPersistent);
begin
  if Dest is TStringList then
  begin
    TStringList(Dest).clear;
    TStringList(Dest).CaseSensitive := fCaseSensitive;
    TStringList(Dest).Duplicates := fDuplicates;
    TStringList(Dest).Sorted := fSorted;
  end;
  inherited AssignTo(Dest);
end;

{*********************************}
procedure TALNVStringListA.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{**********************************}
procedure TALNVStringListA.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{*******************************}
procedure TALNVStringListA.Clear;
var
  I: Integer;
  Temp: Array of TObject;
begin
  if FCount <> 0 then
  begin
    Changing;

    // If the list owns the Objects gather them and free after the list is disposed
    if OwnsObjects then
    begin
      SetLength(Temp, FCount);
      for I := 0 to FCount - 1 do
        Temp[I] := FList[I].FObject;
    end;

    FCount := 0;
    SetCapacity(0);

    // Free the objects that were owned by the list
    if Length(Temp) > 0 then
      for I := 0 to Length(Temp) - 1 do
        ALFreeAndNil(Temp[I]);

    Changed;
  end;
end;

{************************************************}
procedure TALNVStringListA.Delete(Index: Integer);
var
  Obj: TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index, FCount - 1);
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  // Direct memory writing to managed array follows
  //  see http://dn.embarcadero.com/article/33423
  // Explicitly finalize the element we about to stomp on with move
  Finalize(FList[Index]);
  Dec(FCount);
  if Index < FCount then
  begin
    ALMove(
      FList[Index + 1],
      FList[Index],
      (FCount - Index) * SizeOf(TALNVStringItemA));
    // Make sure there is no danglng pointer in the last (now unused) element
    PPointer(@FList[FCount].FName)^   := nil;
    PPointer(@FList[FCount].FValue)^  := nil;
    PPointer(@FList[FCount].FObject)^ := nil;
  end;
  if Obj <> nil then
    ALFreeAndNil(Obj);
  Changed;
end;

{****************************************************************}
function  TALNVStringListA.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index, FCount - 1);
  Changing;
  result := FList[Index].FObject;
  FList[Index].FObject := nil;
  Changed;
end;

{***********************************************************}
procedure TALNVStringListA.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    IndexError(Index1, FCount - 1);
  if (Index2 < 0) or (Index2 >= FCount) then
    IndexError(Index2, FCount - 1);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{****************************************************************}
procedure TALNVStringListA.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Pointer;
  Item1, Item2: PALNVStringItemA;
begin
  Item1 := @FList[Index1];
  Item2 := @FList[Index2];

  Temp := Pointer(Item1^.FName);
  Pointer(Item1^.FName) := Pointer(Item2^.FName);
  Pointer(Item2^.FName) := Temp;

  Temp := Pointer(Item1^.FValue);
  Pointer(Item1^.FValue) := Pointer(Item2^.FValue);
  Pointer(Item2^.FValue) := Temp;

  Temp := pointer(Item1^.FObject);
  pointer(Item1^.FObject) := pointer(Item2^.FObject);
  pointer(Item2^.FObject) := Temp;
end;

{*******************************************************************************}
function TALNVStringListA.Find(const S: AnsiString; var Index: Integer): Boolean;
var Name, Value: ansiString;
begin
  if ExtractNameValue(S, Name, Value) then result := FindNameValue(Name, Value, Index)
  else result := FindName(Name, False{WithNvS}, Index);
end;

{**************************************************************************************}
function TALNVStringListA.FindName(const Name: AnsiString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FName, Name);     // The return value is less than 0 if FList[I].FName < Name, 0 if FList[I].FName = Name, or greater than 0 if FList[I].FName > Name.
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        //if Duplicates <> dupAccept then L := I; //because we need the last value of name in any case
                                                  //ex: a
                                                  //    aaa
                                                  //    aaa=
                                                  //    bbb
                                                  //then doing values['aaa'] := 'xxx' must result in
                                                  //    a
                                                  //    aaa
                                                  //    aaa=xxx
                                                  //    bbb
                                                  //and not in
                                                  //    a
                                                  //    aaa=xxx
                                                  //    aaa=
                                                  //    bbb
        L := I; // this mean L > H
        I := I + 1;
        while I <= FCount - 1 do begin
          if CompareStrings(FList[I].FName, Name) = 0 then L := I
          else break;
          I := I + 1;
        end;
      end;
    end;
  end;
  Index := L;
end;

{********************************************************************************************************}
function TALNVStringListA.FindName(const Name: AnsiString; WithNvS: boolean; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FName, Name);     // The return value is less than 0 if FList[I].FName < Name, 0 if FList[I].FName = Name, or greater than 0 if FList[I].FName > Name.
    if (C = 0) then begin
      if (not WithNvS) and FList[I].FNVS then c := 1   // Must be ordered in this order :
                                                       // aa
                                                       // aaa
                                                       // aaa=
                                                       // aaaa
      else if (WithNvS) and (not FList[I].FNVS) then c := -1;  // Must be ordered in this order :
                                                               // aa
                                                               // aaa
                                                               // aaa=
                                                               // aaaa
    end;
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        //if Duplicates <> dupAccept then L := I; //because we need the last value of name in any case
                                                  //ex: a
                                                  //    aaa
                                                  //    aaa=
                                                  //    bbb
                                                  //then doing values['aaa'] := 'xxx' must result in
                                                  //    a
                                                  //    aaa
                                                  //    aaa=xxx
                                                  //    bbb
                                                  //and not in
                                                  //    a
                                                  //    aaa=xxx
                                                  //    aaa=
                                                  //    bbb
        L := I; // this mean L > H
        I := I + 1;
        while I <= FCount - 1 do begin
          C := CompareStrings(FList[I].FName, Name);
          if (C = 0) then begin
            if (not WithNvS) and FList[I].FNVS then c := 1   // Must be ordered in this order :
                                                             // aa
                                                             // aaa
                                                             // aaa=
                                                             // aaaa
            else if (WithNvS) and (not FList[I].FNVS) then c := -1;  // Must be ordered in this order :
                                                                     // aa
                                                                     // aaa
                                                                     // aaa=
                                                                     // aaaa
          end;
          if c = 0 then L := I
          else break;
          I := I + 1;
        end;
      end;
    end;
  end;
  Index := L;
end;

{**************************************************************************************************}
function TALNVStringListA.FindNameValue(const Name, Value: AnsiString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FName, Name);  // The return value is less than 0 if FList[I].FName < Name, 0 if FList[I].FName = Name, or greater than 0 if FList[I].FName > Name.
    if (C = 0) then begin
      if (not FList[I].FNVS) then c := -1 // Must be ordered in this order :
                                          // aa
                                          // aaa
                                          // aaa=
                                          // aaaa
      else C := CompareStrings(FList[I].FValue, Value);  // The return value is less than 0 if FList[I].FValue < Value, 0 if FList[I].FValue = Value, or greater than 0 if FList[I].FValue > Value.
    end;
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

{********************************************************}
function TALNVStringListA.Get(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  if FList[Index].fNvs then Result := FList[Index].FName + NameValueSeparator + FList[Index].fValue
  else Result := FList[Index].fname;
end;

{*********************************************}
function TALNVStringListA.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

{******************************************}
function TALNVStringListA.GetCount: Integer;
begin
  Result := FCount;
end;

{***********************************************************}
function TALNVStringListA.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  Result := FList[Index].FObject;
end;

{******************************}
procedure TALNVStringListA.Grow;
{$IF CompilerVersion <= 32}{tokyo}
var
  Delta: Integer;
{$endif}
begin
  {$IF CompilerVersion <= 32}{tokyo}
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
  {$else}
  SetCapacity(GrowCollection(FCapacity, FCount + 1));
  {$endif}
end;

{***********************************************}
function TALNVStringListA.GetTextStr: AnsiString;
var
  I, L, Size: Integer;
  P: PAnsiChar;
  S, LB: AnsiString;
  NvS: AnsiChar;
begin
  Size := 0;
  LB := LineBreak;
  NvS := nameValueSeparator;
  for I := 0 to FCount - 1 do begin
    if FList[i].fNvs then Inc(Size, Length(FList[i].fName) + 1{length(NameValueSeparator)} +  Length(FList[i].fValue) + Length(LB))
    else Inc(Size, Length(FList[i].fName) + Length(LB))
  end;
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to FCount - 1 do
  begin
    S := FList[i].fName;
    L := Length(S);
    if L <> 0 then
    begin
      ALMove(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    if FList[i].fNvs then begin
      ALMove(NvS, P^, 1);
      Inc(P, 1);
      S := FList[i].fValue;
      L := Length(S);
      if L <> 0 then
      begin
        ALMove(Pointer(S)^, P^, L);
        Inc(P, L);
      end;
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      ALMove(Pointer(LB)^, P^, L);
      Inc(P, L);
    end;
  end;
end;

{**************************************************************}
function TALNVStringListA.IndexOf(const S: AnsiString): Integer;
Var LName, LValue: AnsiString;
begin
  if ExtractNameValue(S, LName, LValue) then begin
    if not Sorted then begin
      for Result := 0 to FCount - 1 do
        if Flist[Result].FNVS and
           (CompareStrings(Flist[Result].FName, LName) = 0) and
           (CompareStrings(Flist[Result].FValue, LValue) = 0) then Exit;
      Result := -1;
    end
    else begin
      if not FindNameValue(LName, LValue, Result) then Result := -1;
    end;
  end
  else begin
    if not Sorted then begin
      for Result := 0 to FCount - 1 do
        if (not Flist[Result].FNVS) and
           (CompareStrings(Flist[Result].FName, s) = 0) then Exit;
      Result := -1;
    end
    else begin
      if not FindName(s, false{WinthNvS}, Result) then Result := -1;
    end;
  end;
end;

{*********************************************************************}
function TALNVStringListA.IndexOfName(const Name: ansistring): Integer;
begin
  if not Sorted then begin
    for Result := 0 to FCount - 1 do
      if CompareStrings(Flist[Result].FName, Name) = 0 then Exit;
    Result := -1;
  end
  else begin
    if not FindName(Name, Result) then Result := -1;
  end;
end;

{*********************************************************************}
procedure TALNVStringListA.Insert(Index: Integer; const S: AnsiString);
begin
  InsertObject(Index, S, nil);
end;

{*********************************************************************************************}
procedure TALNVStringListA.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then
    IndexError(Index, FCount);
  InsertItem(Index, S, AObject);
end;

{****************************************************************************************}
procedure TALNVStringListA.InsertNameValue(Index: Integer; const Name, Value: AnsiString);
begin
  InsertNameValueObject(Index, Name, Value, nil);
end;

{****************************************************************************************************************}
procedure TALNVStringListA.InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then
    IndexError(Index, FCount - 1);
  InsertItem(Index, Name, Value, AObject);
end;

{***********************************************************}
procedure TALNVStringListA.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempName: AnsiString;
  TempValue: AnsiString;
  TempNvS: Boolean;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempName := Flist[curIndex].FName;
      TempNvs := Flist[curIndex].FNvs;
      if TempNvs then TempValue := Flist[curIndex].FValue;
      TempObject := Flist[curIndex].FObject;
      FList[CurIndex].FObject := nil;
      Delete(CurIndex);
      if TempNvs then InsertObject(NewIndex, TempName, TempObject)
      else InsertNameValueObject(NewIndex, TempName, TempValue, TempObject)
    finally
      EndUpdate;
    end;
  end;
end;

{*******************************************************************************************}
procedure TALNVStringListA.InsertItem(Index: Integer; const S: AnsiString; AObject: TObject);
var Name, Value: ansiString;
begin
  if ExtractNameValue(S, Name, Value) then InsertItem(Index, Name, Value, AObject)
  else InsertItem(Index, s, False{WithNvS}, AObject);
end;

{*****************************************************************************************************}
procedure TALNVStringListA.InsertItem(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    ALMove(
      FList[Index],
      FList[Index + 1],
      (FCount - Index) * SizeOf(TALNVStringItemA));
  Pointer(FList[Index].FName) := nil;
  Pointer(FList[Index].FValue) := nil;
  Pointer(FList[Index].FObject) := nil;
  FList[Index].FObject := AObject;
  FList[Index].FName := Name;
  FList[Index].fNvs := true;
  FList[Index].FValue := Value;
  Inc(FCount);
  Changed;
end;

{****************************************************************************************************************}
procedure TALNVStringListA.InsertItem(Index: Integer; const Name: AnsiString; WithNvS: boolean; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    ALMove(
      FList[Index],
      FList[Index + 1],
      (FCount - Index) * SizeOf(TALNVStringItemA));
  Pointer(FList[Index].FName) := nil;
  Pointer(FList[Index].FValue) := nil;
  Pointer(FList[Index].FObject) := nil;
  FList[Index].FObject := AObject;
  FList[Index].FName := Name;
  FList[Index].fNvs := WithNvS;
  FList[Index].FValue := '';
  Inc(FCount);
  Changed;
end;

{******************************************************************}
procedure TALNVStringListA.Put(Index: Integer; const S: AnsiString);
var Name, Value: ansiString;
begin
  if not sorted then begin
    if Cardinal(Index) >= Cardinal(FCount) then
      IndexError(Index, FCount - 1);
    Changing;
    if ExtractNameValue(S, Name, Value) then begin
      FList[Index].FName := Name;
      FList[Index].FNvS := True;
      FList[Index].FValue := Value;
    end
    else begin
      FList[Index].FName := S;
      FList[Index].FNvS := False;
      FList[Index].FValue := '';
    end;
    Changed;
  end
  else begin
    delete(index);
    add(s);
  end;
end;

{*********************************************************************}
procedure TALNVStringListA.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  FList[Index].FObject := AObject;

  if Obj <> nil then
    ALFreeAndNil(Obj);

  Changed;
end;

{*****************************************************************************************}
procedure TALNVStringListA.QuickSort(L, R: Integer; ACompare: TALNVStringListSortCompareA);
var
  I, J, P: Integer;
begin
  while L < R do
  begin
    if (R - L) = 1 then
    begin
      if ACompare(Self, L, R) > 0 then
        ExchangeItems(L, R);
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
          ExchangeItems(I, J);
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

{***********************************************************}
procedure TALNVStringListA.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{***************************************************}
procedure TALNVStringListA.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

{***********************************************************}
procedure TALNVStringListA.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{***********************************************************************************************}
function ALNVStringListCompareStringsA(List: TALNVStringListA; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(
              List.FList[Index1].FName,
              List.FList[Index2].FName);  // The return value is less than 0 if List.FList[Index1].FName < List.FList[Index2].FName, 0 if List.FList[Index1].FName = List.FList[Index2].FName, or greater than 0 if List.FList[Index1].FName > List.FList[Index2].FName.
  if result = 0 then begin
    if (not List.FList[Index1].fNvS) and List.FList[Index2].FNVS then result := -1  // Must be ordered in this order :
                                                                                    // aa
                                                                                    // aaa
                                                                                    // aaa=
                                                                                    // aaaa
    else if (List.FList[Index1].fNvS) and (not List.FList[Index2].FNVS) then result := 1;  // Must be ordered in this order :
                                                                                           // aa
                                                                                           // aaa
                                                                                           // aaa=
                                                                                           // aaaa
    if (result=0) then Result := List.CompareStrings(
                                   List.FList[Index1].FValue,
                                   List.FList[Index2].FValue);  // The return value is less than 0 if List.FList[Index1].FValue < List.FList[Index2].FValue, 0 if List.FList[Index1].FValue = List.FList[Index2].FValue, or greater than 0 if List.FList[Index1].FValue > List.FList[Index2].FValue.
  end;
end;

{******************************}
procedure TALNVStringListA.Sort;
begin
  CustomSort(ALNVStringListCompareStringsA);
end;

{**************************************************************************}
procedure TALNVStringListA.CustomSort(Compare: TALNVStringListSortCompareA);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

{**************************************************************************}
function TALNVStringListA.CompareStrings(const S1, S2: AnsiString): Integer;
begin

  // Orignial Delphi Code
  // the difference between TALNVStringListA and TStringList is that
  // TstringList use ansiCompareStr or ansiCompareText that are
  // dependant from the local. I don't like this behavior because
  // as you can read
  // http://msdn.microsoft.com/en-us/library/windows/desktop/dd317759(v=vs.85).aspx
  // "Using CompareString incorrectly can compromise the security of your
  // application. Strings that are not compared correctly can produce
  // invalid input. For example, the function can raise security issues when
  // used for a non-linguistic comparison, because two strings that are
  // distinct in their binary representation can be linguistically equivalent"
  // so i prefere to use instead CompareStr and CompareText but only
  // a..z = A..Z will be handle when case insensitive is set.
  // other behavior must be handle in descendant classe
  //
  // also not the ansiCompareStr and ansiCompareText
  // are 10x more slower than CompareStr and CompareText
  //
  // if CaseSensitive then
  //   Result := AnsiCompareStr(S1, S2)
  // else
  //   Result := AnsiCompareText(S1, S2);

  // it's important that the order is not change because
  // of the ord(NameValueSeparator) this is need because of
  // function FindName
  //
  // EX the items
  //
  //   aaa0
  //   aaa=123
  //   aaaa
  //
  // must be ordered like
  //
  //   aaa=123   |     aaa
  //   aaa0      |     aaa0
  //   aaaa      |     aaaa
  //                   => OK, ordered work with findname
  //
  // but with just Result := ALCompareTextA(S1, S2)
  // it's will be ordered like
  //
  //   aaa0      |     aaa0
  //   aaa=123   |     aaa
  //   aaaa      |     aaaa
  //                   => KO, NOT ordered, break the findname
  //

  if CaseSensitive then
    Result := ALCompareStrA(S1, S2)
  else
    Result := ALCompareTextA(S1, S2);

end;

{****************************************************}
procedure TALNVStringListA.init(OwnsObjects: Boolean);
begin
  setlength(FList, 0);
  FCount := 0;
  FCapacity := 0;
  FSorted := False;
  FDuplicates := dupIgnore;
  FCaseSensitive := False;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := OwnsObjects;
end;

{**********************************}
constructor TALNVStringListA.Create;
begin
  inherited Create;
  init(False);
end;

{********************************************************}
constructor TALNVStringListA.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects);
end;

{****************************************************************}
procedure TALNVStringListA.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then
    begin
      // Calling Sort won't sort the list because CustomSort will
      // only sort the list if it's not already sorted
      Sorted := False;
      Sorted := True;
    end;
  end;
end;

{****************************************************************************************************}
Function TALNVStringListA.ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean;
Var P1: Integer;
begin
  P1 := ALPosA(NameValueSeparator,S);
  if P1 > 0 then begin
    result := True;
    Name := AlCopyStr(S,1,P1-1);
    Value := AlCopyStr(S,P1+1, maxint);
  end
  else begin
    Result := False;
    Name := S;
    Value := '';
  end;
end;

{************************************************************}
function TALNVStringListA.GetName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  Result := Flist[Index].fName;
end;

{******************************************************************}
function TALNVStringListA.GetStrictName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  if Flist[Index].fnvs then Result := Flist[Index].fName
  else result := ''
end;

{*********************************************************************}
function TALNVStringListA.GetValue(const Name: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then begin
    if Flist[i].fnvs then Result := Flist[i].fValue
    else Result := '';
  end else
    Result := '';
end;

{*****************************************************************}
procedure TALNVStringListA.SetValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then AddNameValue(Name, Value)
    else begin
      Changing;
      Flist[i].fValue := Value;
      Flist[i].fNVS := True;
      Changed;
    end
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{**********************************************************************}
function TALNVStringListA.GetValueFromIndex(Index: Integer): AnsiString;
begin
  if Index >= 0 then
  begin
    if Cardinal(Index) >= Cardinal(FCount) then
      IndexError(Index, FCount - 1);
    if (Flist[index].fNvs) then
      result := Flist[index].fValue
    else
      Result := '';
  end
  else
    Result := '';
end;

{************************************************************************************}
procedure TALNVStringListA.SetValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Value <> '' then
  begin
    if Index < 0 then AddNameValue('', Value)
    else begin
      if Cardinal(Index) >= Cardinal(FCount) then
        IndexError(Index, FCount - 1);
      Changing;
      Flist[Index].fValue := Value;
      Flist[Index].fNVS := True;
      Changed;
    end;
  end
  else
    if Index >= 0 then Delete(Index);
end;

{***************************************************************************}
procedure TALNVStringListA.SetPersistentValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then AddNameValue(Name, Value)
  else begin
    Changing;
    Flist[I].fValue := Value;
    Flist[I].fNVS := True;
    Changed;
  end
end;

{**********************************************************************************************}
procedure TALNVStringListA.SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Index < 0 then AddNameValue('', Value)
  else begin
    if Cardinal(Index) >= Cardinal(FCount) then
      IndexError(Index, FCount - 1);
    Changing;
    Flist[Index].fValue := Value;
    Flist[Index].fNVS := True;
    Changed;
  end;
end;

{***********************************}
destructor TALAVLStringListA.Destroy;
var
  I: Integer;
  Temp: Array of TObject;
begin
  FOnChange := nil;
  FOnChanging := nil;

  // If the list owns the Objects gather them and free after the list is disposed
  if OwnsObjects then
  begin
    SetLength(Temp, Count);
    for I := 0 to Count - 1 do
      Temp[I] := Objects[I];
  end;

  ALFreeAndNil(FAVLBinTree);
  ALFreeAndNil(FNodeList);
  inherited Destroy;

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      ALFreeAndNil(Temp[I]);
end;

{***********************************************************}
function TALAVLStringListA.Add(const S: AnsiString): Integer;
begin
  Result := AddObject(S, nil);
end;

{***********************************************************************************}
function TALAVLStringListA.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  Result := Count;
  InsertItem(Result, S, AObject);
end;

{******************************************************************************}
function TALAVLStringListA.AddNameValue(const Name, Value: AnsiString): Integer;
begin
  Result := AddNameValueObject(Name, Value, nil);
end;

{******************************************************************************************************}
function TALAVLStringListA.AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer;
begin
  Result := Count;
  InsertItem(Result, Name, Value, AObject);
end;

{******************************************************}
procedure TALAVLStringListA.Assign(Source: TPersistent);
begin
  if Source is TALAVLStringListA then
  begin
    Clear;
    CaseSensitive := TALAVLStringListA(Source).CaseSensitive;
    FDuplicates := TALAVLStringListA(Source).FDuplicates;
  end
  else if Source is TALStringListA then
  begin
    Clear;
    CaseSensitive := TALStringListA(Source).CaseSensitive;
    FDuplicates := TALStringListA(Source).Duplicates;
  end
  else if Source is TALNVStringListA then
  begin
    Clear;
    CaseSensitive := TALNVStringListA(Source).FCaseSensitive;
    FDuplicates := TALNVStringListA(Source).FDuplicates;
  end
  else if Source is TALHashedStringListA then
  begin
    Clear;
    CaseSensitive := TALHashedStringListA(Source).CaseSensitive;
    FDuplicates := TALHashedStringListA(Source).FDuplicates;
  end
  else if Source is TStringList then
  begin
    Clear;
    CaseSensitive := TStringList(Source).CaseSensitive;
    FDuplicates := TStringList(Source).Duplicates;
  end;
  inherited Assign(Source);
end;

{******************************************************}
procedure TALAVLStringListA.AssignTo(Dest: TPersistent);
begin
  if Dest is TStringList then
  begin
    TStringList(Dest).clear;
    TStringList(Dest).CaseSensitive := CaseSensitive;
    TStringList(Dest).Duplicates := fDuplicates;
    TStringList(Dest).Sorted := False;
  end;
  inherited AssignTo(Dest);
end;

{**********************************}
procedure TALAVLStringListA.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************}
procedure TALAVLStringListA.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{********************************}
procedure TALAVLStringListA.Clear;
var
  I: Integer;
  Temp: Array of TObject;
begin
  if Count <> 0 then
  begin
    Changing;

    // If the list owns the Objects gather them and free after the list is disposed
    if OwnsObjects then
    begin
      SetLength(Temp, Count);
      for I := 0 to Count - 1 do
        Temp[I] := Objects[I];
    end;

    FAVLBinTree.Clear;
    FnodeList.Clear;

    // Free the objects that were owned by the list
    if Length(Temp) > 0 then
      for I := 0 to Length(Temp) - 1 do
        ALFreeAndNil(Temp[I]);

    Changed;
  end;
end;

{*************************************************}
procedure TALAVLStringListA.Delete(Index: Integer);
var
  Obj: TObject;
  i: integer;
begin
  if (Index < 0) or (Index >= Count) then
    IndexError(Index, Count - 1);
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := Objects[Index]
  else
    Obj := nil;

  FAVLBinTree.DeleteNode(TALAVLStringListBinaryTreeNodeA(FNodelist[Index]).ID);
  FNodelist.Delete(Index);
  for i := Index to FNodeList.Count - 1 do
    TALAVLStringListBinaryTreeNodeA(FNodelist[i]).Idx := i;

  if Obj <> nil then
    ALFreeAndNil(Obj);
  Changed;
end;

{*****************************************************************}
function  TALAVLStringListA.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= Count) then
    IndexError(Index, Count - 1);
  Changing;
  with TALAVLStringListBinaryTreeNodeA(FNodeList[Index]) do begin
    result := Obj;
    Obj := nil;
  end;
  Changed;
end;

{************************************************************}
procedure TALAVLStringListA.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= Count) then
    IndexError(Index1, Count - 1);
  if (Index2 < 0) or (Index2 >= Count) then
    IndexError(Index2, Count - 1);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{*****************************************************************}
procedure TALAVLStringListA.ExchangeItems(Index1, Index2: Integer);
var Item1, Item2: TALAVLStringListBinaryTreeNodeA;
begin
  Item1 := TALAVLStringListBinaryTreeNodeA(FNodelist[Index1]);
  Item2 := TALAVLStringListBinaryTreeNodeA(FNodelist[Index2]);
  FNodeList.Exchange(Index1,Index2);
  Item1.idx := Index2;
  Item2.idx := Index1;
end;

{*********************************************************}
function TALAVLStringListA.Get(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    IndexError(Index, Count - 1);
  with TALAVLStringListBinaryTreeNodeA(FNodelist[Index]) do begin
    if Nvs then Result := ID + NameValueSeparator + Val
    else Result := ID;
  end;
end;

{*******************************************}
function TALAVLStringListA.GetCount: Integer;
begin
  Result := FNodeList.Count;
end;

{************************************************************}
function TALAVLStringListA.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    IndexError(Index, Count - 1);
  Result := TALAVLStringListBinaryTreeNodeA(FNodelist[Index]).Obj;
end;

{************************************************}
function TALAVLStringListA.GetTextStr: AnsiString;
var
  I, L, Size, Count: Integer;
  P: PAnsiChar;
  S, LB: AnsiString;
  NvS: AnsiChar;
begin
  Count := GetCount;
  Size := 0;
  LB := LineBreak;
  NvS := nameValueSeparator;
  for I := 0 to Count - 1 do begin
    if TALAVLStringListBinaryTreeNodeA(fNodeList[i]).Nvs then Inc(Size, Length(TALAVLStringListBinaryTreeNodeA(fNodeList[i]).ID) + 1{length(NameValueSeparator)} +  Length(TALAVLStringListBinaryTreeNodeA(fNodeList[i]).Val) + Length(LB))
    else Inc(Size, Length(TALAVLStringListBinaryTreeNodeA(fNodeList[i]).ID) + Length(LB))
  end;
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := TALAVLStringListBinaryTreeNodeA(fNodeList[i]).ID;
    L := Length(S);
    if L <> 0 then
    begin
      ALMove(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    if TALAVLStringListBinaryTreeNodeA(fNodeList[i]).Nvs then begin
      ALMove(NvS, P^, 1);
      Inc(P, 1);
      S := TALAVLStringListBinaryTreeNodeA(fNodeList[i]).Val;
      L := Length(S);
      if L <> 0 then
      begin
        ALMove(Pointer(S)^, P^, L);
        Inc(P, L);
      end;
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      ALMove(Pointer(LB)^, P^, L);
      Inc(P, L);
    end;
  end;
end;

{***************************************************************}
function TALAVLStringListA.IndexOf(const S: AnsiString): Integer;
Var LName, LValue: AnsiString;
    LNode: TALAVLStringListBinaryTreeNodeA;
begin
  if ExtractNameValue(S, LName, LValue) then begin
    LNode := TALAVLStringListBinaryTreeNodeA(FAVLBinTree.FindNode(LName));
    if (not assigned(LNode))
       or
       ((CaseSensitive) and
        (LNode.Val <> LValue))
       or
       ((not CaseSensitive) and
        (not ALSameTextA(LNode.Val, LValue)))
    then result := -1
    else result := LNode.idx;
  end
  else begin
    LNode := TALAVLStringListBinaryTreeNodeA(FAVLBinTree.FindNode(S));
    if (not assigned(LNode)) or (LNode.Nvs) then result := -1
    else result := LNode.idx;
  end;
end;

{**********************************************************************}
function TALAVLStringListA.IndexOfName(const Name: ansistring): Integer;
Var LNode: TALAVLStringListBinaryTreeNodeA;
begin
  LNode := TALAVLStringListBinaryTreeNodeA(FAVLBinTree.FindNode(Name));
  if assigned(LNode) then result := LNode.Idx
  else result := -1;
end;

{**********************************************************************}
procedure TALAVLStringListA.Insert(Index: Integer; const S: AnsiString);
begin
  InsertObject(Index, S, nil);
end;

{**********************************************************************************************}
procedure TALAVLStringListA.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  if (Index < 0) or (Index > Count) then
    IndexError(Index, Count);
  InsertItem(Index, S, AObject);
end;

{*****************************************************************************************}
procedure TALAVLStringListA.InsertNameValue(Index: Integer; const Name, Value: AnsiString);
begin
  InsertNameValueObject(Index, Name, Value, nil);
end;

{*****************************************************************************************************************}
procedure TALAVLStringListA.InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
begin
  if (Index < 0) or (Index > Count) then
    IndexError(Index, Count - 1);
  InsertItem(Index, Name, Value, AObject);
end;

{************************************************************}
procedure TALAVLStringListA.Move(CurIndex, NewIndex: Integer);
var i: integer;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      FNodeList.Move(CurIndex, NewIndex);
      for i := min(CurIndex, NewIndex) to FNodeList.Count - 1 do
        TALAVLStringListBinaryTreeNodeA(FNodelist[i]).Idx := i;
    finally
      EndUpdate;
    end;
  end;
end;

{******************************************************************************************************}
procedure TALAVLStringListA.InsertItem(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
Var LNode: TALAVLStringListBinaryTreeNodeA;
    I: integer;
begin
  Changing;

  LNode := TALAVLStringListBinaryTreeNodeA.Create;
  LNode.Idx := Index;
  LNode.ID := Name;
  LNode.Val := Value;
  LNode.Nvs := True;
  LNode.Obj := AObject;
  if not FAVLBinTree.AddNode(LNode) then begin
    ALFreeAndNil(LNode);
    case Duplicates of
      dupIgnore: Exit;
      else Error(@SDuplicateString, 0);
    end;
  end;
  FNodeList.Insert(Index, LNode);
  for i := Index + 1 to FNodeList.Count - 1 do
    TALAVLStringListBinaryTreeNodeA(FNodelist[i]).Idx := i;

  Changed;
end;

{********************************************************************************************}
procedure TALAVLStringListA.InsertItem(Index: Integer; const S: AnsiString; AObject: TObject);
Var LName, AValue: AnsiString;
    LNvs: Boolean;
    LNode: TALAVLStringListBinaryTreeNodeA;
    I: integer;
begin
  Changing;

  LNvs := ExtractNameValue(S, LName, aValue);
  LNode := TALAVLStringListBinaryTreeNodeA.Create;
  LNode.Idx := Index;
  LNode.ID := LName;
  LNode.Val := aValue;
  LNode.Nvs := LNvs;
  LNode.Obj := AObject;
  if not FAVLBinTree.AddNode(LNode) then begin
    ALFreeAndNil(LNode);
    case Duplicates of
      dupIgnore: Exit;
      else Error(@SDuplicateString, 0);
    end;
  end;
  FNodeList.Insert(Index, LNode);
  for i := Index + 1 to FNodeList.Count - 1 do
    TALAVLStringListBinaryTreeNodeA(FNodelist[i]).Idx := i;

  Changed;
end;

{*******************************************************************}
procedure TALAVLStringListA.Put(Index: Integer; const S: AnsiString);
Var LNewName, LNewValue: AnsiString;
    LNewNvs: Boolean;
    LNewNode, LOldNode: TALAVLStringListBinaryTreeNodeA;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    IndexError(Index, Count - 1);
  Changing;

  LNewNvs := ExtractNameValue(S, LNewName, LNewValue);
  LOldNode := TALAVLStringListBinaryTreeNodeA(FNodeList[index]);
  if (CaseSensitive and (LOldNode.ID <> LNewName)) or
     ((not CaseSensitive) and (not ALSameTextA(LOldNode.ID, LNewName))) then begin
    LNewNode := TALAVLStringListBinaryTreeNodeA.Create;
    LNewNode.Idx := Index;
    LNewNode.ID := LNewName;
    LNewNode.Val := LNewValue;
    LNewNode.NVS := LNewNvs;
    LNewNode.Obj := LOldNode.Obj;
    if not FAVLBinTree.AddNode(LNewNode) then begin
      ALFreeAndNil(LNewNode);
      case Duplicates of
        dupIgnore: Exit;
        else Error(@SDuplicateString, 0);
      end;
    end;
    FNodeList[Index] := LNewNode;
    FAVLBinTree.DeleteNode(LOldNode.ID);
  end
  else begin
    LOldNode.Val := LNewValue;
    LOldNode.NVS := LNewNvs;
  end;

  Changed;
end;

{**********************************************************************}
procedure TALAVLStringListA.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    IndexError(Index, Count - 1);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := TALAVLStringListBinaryTreeNodeA(FNodeList[Index]).Obj
  else
    Obj := nil;

  TALAVLStringListBinaryTreeNodeA(FNodeList[Index]).Obj := AObject;

  if Obj <> nil then
    ALFreeAndNil(Obj);

  Changed;
end;

{*******************************************************************************************}
procedure TALAVLStringListA.QuickSort(L, R: Integer; ACompare: TALAVLStringListSortCompareA);
var
  I, J, P: Integer;
begin
  while L < R do
  begin
    if (R - L) = 1 then
    begin
      if ACompare(Self, L, R) > 0 then
        ExchangeItems(L, R);
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
          ExchangeItems(I, J);
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

{************************************************************}
procedure TALAVLStringListA.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{****************************************************************************}
procedure TALAVLStringListA.CustomSort(Compare: TALAVLStringListSortCompareA);
begin
  if (Count > 1) then
  begin
    Changing;
    QuickSort(0, Count - 1, Compare);
    Changed;
  end;
end;

{*****************************************************}
procedure TALAVLStringListA.init(OwnsObjects: Boolean);
begin
  FAVLBinTree:= TALStringKeyAVLBinaryTree.Create;
  FAVLBinTree.CaseSensitive := False;
  FNodeList := TObjectList.Create(False);
  FDuplicates := dupError;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := OwnsObjects;
end;

{***********************************}
constructor TALAVLStringListA.Create;
begin
  inherited Create;
  init(False);
end;

{*********************************************************}
constructor TALAVLStringListA.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects);
end;

{*****************************************************************}
procedure TALAVLStringListA.SetCaseSensitive(const Value: Boolean);
begin
  FAVLBinTree.CaseSensitive := Value;
end;

{***************************************************}
function TALAVLStringListA.GetCaseSensitive: Boolean;
begin
  result := FAVLBinTree.CaseSensitive;
end;

{******************************************************************}
procedure TALAVLStringListA.SetDuplicates(const Value: TDuplicates);
begin
  if value = dupAccept then raise exception.Create('TALAVLStringListA does not support duplicate Names');
  FDuplicates := Value;
end;

{*****************************************************************************************************}
Function TALAVLStringListA.ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean;
Var P1: Integer;
begin
  P1 := ALPosA(NameValueSeparator,S);
  if P1 > 0 then begin
    result := True;
    Name := AlCopyStr(S,1,P1-1);
    Value := AlCopyStr(S,P1+1, maxint);
  end
  else begin
    Result := False;
    Name := S;
    Value := '';
  end;
end;

{*************************************************************}
function TALAVLStringListA.GetName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    IndexError(Index, Count - 1);
  Result := TALAVLStringListBinaryTreeNodeA(FNodelist[Index]).ID;
end;

{*******************************************************************}
function TALAVLStringListA.GetStrictName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    IndexError(Index, Count - 1);
  if TALAVLStringListBinaryTreeNodeA(FNodelist[Index]).nvs then Result := TALAVLStringListBinaryTreeNodeA(FNodelist[Index]).ID
  else result := ''
end;

{**********************************************************************}
function TALAVLStringListA.GetValue(const Name: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then begin
    if TALAVLStringListBinaryTreeNodeA(FNodelist[i]).nvs then Result := TALAVLStringListBinaryTreeNodeA(FNodelist[i]).Val
    else Result := '';
  end else
    Result := '';
end;

{******************************************************************}
procedure TALAVLStringListA.SetValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then AddNameValue(Name, Value)
    else begin
      Changing;
      TALAVLStringListBinaryTreeNodeA(FNodeList[i]).Val := Value;
      TALAVLStringListBinaryTreeNodeA(FNodeList[i]).NVS := True;
      Changed;
    end
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{***********************************************************************}
function TALAVLStringListA.GetValueFromIndex(Index: Integer): AnsiString;
begin
  if Index >= 0 then
  begin
    if Cardinal(Index) >= Cardinal(Count) then
      IndexError(Index, Count - 1);
    if (TALAVLStringListBinaryTreeNodeA(FNodeList[index]).Nvs) then
      result := TALAVLStringListBinaryTreeNodeA(FNodeList[index]).Val
    else
      Result := '';
  end
  else
    Result := '';
end;

{*************************************************************************************}
procedure TALAVLStringListA.SetValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Value <> '' then
  begin
    if Index < 0 then AddNameValue('', Value)
    else begin
      if Cardinal(Index) >= Cardinal(Count) then
        IndexError(Index, Count - 1);
      Changing;
      TALAVLStringListBinaryTreeNodeA(FNodeList[Index]).Val := Value;
      TALAVLStringListBinaryTreeNodeA(FNodeList[Index]).NVS := True;
      Changed;
    end;
  end
  else
    if Index >= 0 then Delete(Index);
end;

{****************************************************************************}
procedure TALAVLStringListA.SetPersistentValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then AddNameValue(Name, Value)
  else begin
    Changing;
    TALAVLStringListBinaryTreeNodeA(FNodeList[I]).Val := Value;
    TALAVLStringListBinaryTreeNodeA(FNodeList[I]).NVS := True;
    Changed;
  end
end;

{***********************************************************************************************}
procedure TALAVLStringListA.SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Index < 0 then AddNameValue('', Value)
  else begin
    if Cardinal(Index) >= Cardinal(Count) then
      IndexError(Index, Count - 1);
    Changing;
    TALAVLStringListBinaryTreeNodeA(FNodeList[Index]).Val := Value;
    TALAVLStringListBinaryTreeNodeA(FNodeList[Index]).NVS := True;
    Changed;
  end;
end;

{**************************************}
destructor TALHashedStringListA.Destroy;
var
  I: Integer;
  Temp: Array of TObject;
begin
  FOnChange := nil;
  FOnChanging := nil;

  // If the list owns the Objects gather them and free after the list is disposed
  if OwnsObjects then
  begin
    SetLength(Temp, Count);
    for I := 0 to Count - 1 do
      Temp[I] := Objects[I];
  end;

  ALFreeAndNil(FDictionary);
  ALFreeAndNil(FNodeList);
  inherited Destroy;

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      ALFreeAndNil(Temp[I]);
end;

{**************************************************************}
function TALHashedStringListA.Add(const S: AnsiString): Integer;
begin
  Result := AddObject(S, nil);
end;

{**************************************************************************************}
function TALHashedStringListA.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  Result := Count;
  InsertItem(Result, S, AObject);
end;

{*********************************************************************************}
function TALHashedStringListA.AddNameValue(const Name, Value: AnsiString): Integer;
begin
  Result := AddNameValueObject(Name, Value, nil);
end;

{*********************************************************************************************************}
function TALHashedStringListA.AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer;
begin
  Result := Count;
  InsertItem(Result, Name, Value, AObject);
end;

{*********************************************************}
procedure TALHashedStringListA.Assign(Source: TPersistent);
begin
  if Source is TALHashedStringListA then
  begin
    Clear;
    CaseSensitive := TALHashedStringListA(Source).CaseSensitive;
    FDuplicates := TALHashedStringListA(Source).FDuplicates;
  end
  else if Source is TALStringListA then
  begin
    Clear;
    CaseSensitive := TALStringListA(Source).CaseSensitive;
    FDuplicates := TALStringListA(Source).Duplicates;
  end
  else if Source is TALNVStringListA then
  begin
    Clear;
    FCaseSensitive := TALNVStringListA(Source).FCaseSensitive;
    FDuplicates := TALNVStringListA(Source).FDuplicates;
  end
  else if Source is TALAVLStringListA then
  begin
    Clear;
    CaseSensitive := TALAVLStringListA(Source).CaseSensitive;
    FDuplicates := TALAVLStringListA(Source).FDuplicates;
  end
  else if Source is TStringList then
  begin
    Clear;
    CaseSensitive := TStringList(Source).CaseSensitive;
    FDuplicates := TStringList(Source).Duplicates;
  end;
  inherited Assign(Source);
end;

{*********************************************************}
procedure TALHashedStringListA.AssignTo(Dest: TPersistent);
begin
  if Dest is TStringList then
  begin
    TStringList(Dest).clear;
    TStringList(Dest).CaseSensitive := CaseSensitive;
    TStringList(Dest).Duplicates := fDuplicates;
    TStringList(Dest).Sorted := False;
  end;
  inherited AssignTo(Dest);
end;

{*************************************}
procedure TALHashedStringListA.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{**************************************}
procedure TALHashedStringListA.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{***********************************}
procedure TALHashedStringListA.Clear;
var
  I: Integer;
  Temp: Array of TObject;
begin
  if Count <> 0 then
  begin
    Changing;

    // If the list owns the Objects gather them and free after the list is disposed
    if OwnsObjects then
    begin
      SetLength(Temp, Count);
      for I := 0 to Count - 1 do
        Temp[I] := Objects[I];
    end;

    FDictionary.Clear;
    FnodeList.Clear;

    // Free the objects that were owned by the list
    if Length(Temp) > 0 then
      for I := 0 to Length(Temp) - 1 do
        ALFreeAndNil(Temp[I]);

    Changed;
  end;
end;

{****************************************************}
procedure TALHashedStringListA.Delete(Index: Integer);
var
  Obj: TObject;
  i: integer;
begin
  if (Index < 0) or (Index >= Count) then
    IndexError(Index, Count - 1);
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := Objects[Index]
  else
    Obj := nil;

  Fdictionary.Remove(FNodelist[Index].ID);
  FNodelist.Delete(Index);
  for i := Index to FNodeList.Count - 1 do
    FNodelist[i].Idx := i;

  if Obj <> nil then
    ALFreeAndNil(Obj);
  Changed;
end;

{********************************************************************}
function  TALHashedStringListA.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= Count) then
    IndexError(Index, Count - 1);
  Changing;
  with FNodeList[Index] do begin
    result := Obj;
    Obj := nil;
  end;
  Changed;
end;

{***************************************************************}
procedure TALHashedStringListA.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= Count) then
    IndexError(Index1, Count - 1);
  if (Index2 < 0) or (Index2 >= Count) then
    IndexError(Index2, Count - 1);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{********************************************************************}
procedure TALHashedStringListA.ExchangeItems(Index1, Index2: Integer);
var Item1, Item2: TALHashedStringListDictionaryNodeA;
begin
  Item1 := FNodelist[Index1];
  Item2 := FNodelist[Index2];
  FNodeList.Exchange(Index1,Index2);
  Item1.idx := Index2;
  Item2.idx := Index1;
end;

{************************************************************}
function TALHashedStringListA.Get(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    IndexError(Index, Count - 1);
  with FNodelist[Index] do begin
    if Nvs then Result := ID + NameValueSeparator + Val
    else Result := ID;
  end;
end;

{**********************************************}
function TALHashedStringListA.GetCount: Integer;
begin
  Result := FNodeList.Count;
end;

{***************************************************************}
function TALHashedStringListA.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    IndexError(Index, Count - 1);
  Result := FNodelist[Index].Obj;
end;

{***************************************************}
function TALHashedStringListA.GetTextStr: AnsiString;
var
  I, L, Size, Count: Integer;
  P: PAnsiChar;
  S, LB: AnsiString;
  NvS: AnsiChar;
begin
  Count := GetCount;
  Size := 0;
  LB := LineBreak;
  NvS := nameValueSeparator;
  for I := 0 to Count - 1 do begin
    if fNodeList[i].Nvs then Inc(Size, Length(fNodeList[i].ID) + 1{length(NameValueSeparator)} +  Length(fNodeList[i].Val) + Length(LB))
    else Inc(Size, Length(fNodeList[i].ID) + Length(LB))
  end;
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := fNodeList[i].ID;
    L := Length(S);
    if L <> 0 then
    begin
      ALMove(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    if fNodeList[i].Nvs then begin
      ALMove(NvS, P^, 1);
      Inc(P, 1);
      S := fNodeList[i].Val;
      L := Length(S);
      if L <> 0 then
      begin
        ALMove(Pointer(S)^, P^, L);
        Inc(P, L);
      end;
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      ALMove(Pointer(LB)^, P^, L);
      Inc(P, L);
    end;
  end;
end;

{******************************************************************}
function TALHashedStringListA.IndexOf(const S: AnsiString): Integer;
Var LName, LValue: AnsiString;
    LNode: TALHashedStringListDictionaryNodeA;
begin
  if ExtractNameValue(S, LName, LValue) then begin
    if (not fDictionary.TryGetValue(LName,LNode))
       or
       ((CaseSensitive) and
        (LNode.Val <> LValue))
       or
       ((not CaseSensitive) and
        (not ALSameTextA(LNode.Val, LValue)))
    then result := -1
    else result := LNode.idx;
  end
  else begin
    if (not fDictionary.TryGetValue(S,LNode)) or (LNode.Nvs) then result := -1
    else result := LNode.idx;
  end;
end;

{*************************************************************************}
function TALHashedStringListA.IndexOfName(const Name: ansistring): Integer;
Var LNode: TALHashedStringListDictionaryNodeA;
begin
  if fDictionary.TryGetValue(Name,LNode) then result := LNode.Idx
  else result := -1;
end;

{*************************************************************************}
procedure TALHashedStringListA.Insert(Index: Integer; const S: AnsiString);
begin
  InsertObject(Index, S, nil);
end;

{*************************************************************************************************}
procedure TALHashedStringListA.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  if (Index < 0) or (Index > Count) then
    IndexError(Index, Count);
  InsertItem(Index, S, AObject);
end;

{********************************************************************************************}
procedure TALHashedStringListA.InsertNameValue(Index: Integer; const Name, Value: AnsiString);
begin
  InsertNameValueObject(Index, Name, Value, nil);
end;

{********************************************************************************************************************}
procedure TALHashedStringListA.InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
begin
  if (Index < 0) or (Index > Count) then
    IndexError(Index, Count - 1);
  InsertItem(Index, Name, Value, AObject);
end;

{***************************************************************}
procedure TALHashedStringListA.Move(CurIndex, NewIndex: Integer);
var I: integer;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      FNodeList.Move(CurIndex, NewIndex);
      for i := min(CurIndex, NewIndex) to FNodeList.Count - 1 do
        FNodelist[i].Idx := i;
    finally
      EndUpdate;
    end;
  end;
end;

{*********************************************************************************************************}
procedure TALHashedStringListA.InsertItem(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
Var LNode: TALHashedStringListDictionaryNodeA;
    I: integer;
begin
  Changing;

  LNode := TALHashedStringListDictionaryNodeA.Create;
  LNode.Idx := Index;
  LNode.ID := Name;
  LNode.Val := Value;
  LNode.Nvs := True;
  LNode.Obj := AObject;
  {$IF CompilerVersion <= 32} // tokyo
  if not Fdictionary.ContainsKey(Name) then Fdictionary.Add(Name,aNode)
  else begin
  {$ELSE}
  if not Fdictionary.TryAdd(Name,LNode) then begin
  {$ENDIF}
    ALFreeAndNil(LNode);
    case Duplicates of
      dupIgnore: Exit;
      else Error(@SDuplicateString, 0);
    end;
  end;
  FNodeList.Insert(Index, LNode);
  for i := Index + 1 to FNodeList.Count - 1 do
    FNodelist[i].Idx := i;

  Changed;
end;

{***********************************************************************************************}
procedure TALHashedStringListA.InsertItem(Index: Integer; const S: AnsiString; AObject: TObject);
Var LName, LValue: AnsiString;
    LNvs: Boolean;
    LNode: TALHashedStringListDictionaryNodeA;
    I: integer;
begin
  Changing;

  LNvs := ExtractNameValue(S, LName, LValue);
  LNode := TALHashedStringListDictionaryNodeA.Create;
  LNode.Idx := Index;
  LNode.ID := LName;
  LNode.Val := LValue;
  LNode.Nvs := LNvs;
  LNode.Obj := AObject;
  {$IF CompilerVersion <= 32} // tokyo
  if not Fdictionary.ContainsKey(aName) then Fdictionary.Add(aName,aNode)
  else begin
  {$ELSE}
  if not Fdictionary.TryAdd(LName,LNode) then begin
  {$ENDIF}
    ALFreeAndNil(LNode);
    case Duplicates of
      dupIgnore: Exit;
      else Error(@SDuplicateString, 0);
    end;
  end;
  FNodeList.Insert(Index, LNode);
  for i := Index + 1 to FNodeList.Count - 1 do
    FNodelist[i].Idx := i;

  Changed;
end;

{**********************************************************************}
procedure TALHashedStringListA.Put(Index: Integer; const S: AnsiString);
Var LNewName, LNewValue: AnsiString;
    LNewNvs: Boolean;
    LNewNode, LOldNode: TALHashedStringListDictionaryNodeA;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    IndexError(Index, Count - 1);
  Changing;

  LNewNvs := ExtractNameValue(S, LNewName, LNewValue);
  LOldNode := FNodeList[index];
  if (CaseSensitive and (LOldNode.ID <> LNewName)) or
     ((not CaseSensitive) and (not ALSameTextA(LOldNode.ID, LNewName))) then begin
    LNewNode := TALHashedStringListDictionaryNodeA.Create;
    LNewNode.Idx := Index;
    LNewNode.ID := LNewName;
    LNewNode.Val := LNewValue;
    LNewNode.NVS := LNewNvs;
    LNewNode.Obj := LOldNode.Obj;
    {$IF CompilerVersion <= 32} // tokyo
    if not Fdictionary.ContainsKey(aNewName) then Fdictionary.Add(aNewName, aNewNode)
    else begin
    {$ELSE}
    if not Fdictionary.TryAdd(LNewName, LNewNode) then begin
    {$ENDIF}
      ALFreeAndNil(LNewNode);
      case Duplicates of
        dupIgnore: Exit;
        else Error(@SDuplicateString, 0);
      end;
    end;
    FNodeList[Index] := LNewNode;
    Fdictionary.Remove(LOldNode.ID);
  end
  else begin
    LOldNode.Val := LNewValue;
    LOldNode.NVS := LNewNvs;
  end;

  Changed;
end;

{*************************************************************************}
procedure TALHashedStringListA.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    IndexError(Index, Count - 1);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FNodeList[Index].Obj
  else
    Obj := nil;

  FNodeList[Index].Obj := AObject;

  if Obj <> nil then
    ALFreeAndNil(Obj);

  Changed;
end;

{***************************************************************}
procedure TALHashedStringListA.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity <= FDictionary.Count then FDictionary.TrimExcess;
  FNodeList.Capacity := NewCapacity;
end;

{*************************************************************************************************}
procedure TALHashedStringListA.QuickSort(L, R: Integer; ACompare: TALHashedStringListSortCompareA);
var
  I, J, P: Integer;
begin
  while L < R do
  begin
    if (R - L) = 1 then
    begin
      if ACompare(Self, L, R) > 0 then
        ExchangeItems(L, R);
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
          ExchangeItems(I, J);
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

{***************************************************************}
procedure TALHashedStringListA.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{**********************************************************************************}
procedure TALHashedStringListA.CustomSort(Compare: TALHashedStringListSortCompareA);
begin
  if (Count > 1) then
  begin
    Changing;
    QuickSort(0, Count - 1, Compare);
    Changed;
  end;
end;

{*************************************************************************************************************************************************************}
function TALHashedStringListA.CreateDictionary(ACapacity: integer; aCaseSensitive: boolean): TObjectDictionary<ansiString, TALHashedStringListDictionaryNodeA>;
begin
  if aCaseSensitive then result := TObjectDictionary<ansiString, TALHashedStringListDictionaryNodeA>.create(
                                     [doOwnsValues],
                                     ACapacity,
                                     TDelegatedEqualityComparer<ansiString>.Create(
                                       function(const Left, Right: ansiString): Boolean
                                       begin
                                         Result := ALSameTextA(Left, Right);
                                       end,
                                       function(const Value: ansiString): Integer
                                       begin
                                         Result := THashBobJenkins.GetHashValue(PAnsiChar(Value)^, Length(Value) * SizeOf(AnsiChar));
                                       end))
  else result := TObjectDictionary<ansiString, TALHashedStringListDictionaryNodeA>.create(
                   [doOwnsValues],
                   ACapacity,
                   TDelegatedEqualityComparer<ansiString>.Create(
                     function(const Left, Right: ansiString): Boolean
                     begin
                       Result := ALSameTextA(Left, Right);
                     end,
                     function(const Value: ansiString): Integer
                     var LLowerValue: ansiString;
                     begin
                       LLowerValue := ALLowerCase(Value);
                       Result := THashBobJenkins.GetHashValue(PAnsiChar(LLowerValue)^, Length(LLowerValue) * SizeOf(AnsiChar));
                     end));
end;

{****************************************************************************}
procedure TALHashedStringListA.init(OwnsObjects: Boolean; ACapacity: Integer);
begin
  FDictionary := CreateDictionary(ACapacity, False);
  FCaseSensitive := False;
  FNodeList := TObjectList<TALHashedStringListDictionaryNodeA>.Create(False);
  FNodeList.Capacity := ACapacity;
  FDuplicates := dupError;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := OwnsObjects;
end;

{**************************************}
constructor TALHashedStringListA.Create;
begin
  inherited Create;
  init(False, 0);
end;

{************************************************************}
constructor TALHashedStringListA.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects, 0);
end;

{**********************************************************}
constructor TALHashedStringListA.Create(ACapacity: Integer);
begin
  inherited Create;
  init(False, ACapacity);
end;

{********************************************************************************}
constructor TALHashedStringListA.Create(OwnsObjects: Boolean; ACapacity: Integer);
begin
  inherited Create;
  init(OwnsObjects, ACapacity);
end;

type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if System.Generics.Collections.TObjectDictionary<K,V> was not updated and adjust the IFDEF'}
  {$ENDIF}
  _TObjectDictionaryAccessPrivate<K,V> = class(TObjectDictionary<K,V>)
  private
    FOwnerships: TDictionaryOwnerships;
  end;

{********************************************************************}
procedure TALHashedStringListA.SetCaseSensitive(const Value: Boolean);
var LTmpDictionary: TObjectDictionary<ansiString, TALHashedStringListDictionaryNodeA>;
    I: integer;
begin
  if fCaseSensitive <> Value then begin
    LTmpDictionary := CreateDictionary(count, Value);
    for I := 0 to FnodeList.Count - 1 do
      LTmpDictionary.Add(FNodeList[i].ID,FNodeList[i]);
    _TObjectDictionaryAccessPrivate<ansiString, TALHashedStringListDictionaryNodeA>(Fdictionary).fOwnerships := [];
    ALFreeAndNil(Fdictionary);
    FDictionary := LTmpDictionary;
  end;
end;

{******************************************************}
function TALHashedStringListA.GetCaseSensitive: Boolean;
begin
  result := fCaseSensitive;
end;

{*********************************************************************}
procedure TALHashedStringListA.SetDuplicates(const Value: TDuplicates);
begin
  if value = dupAccept then raise exception.Create('TALHashedStringListA does not support duplicate Names');
  FDuplicates := Value;
end;

{********************************************************************************************************}
Function TALHashedStringListA.ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean;
Var P1: Integer;
begin
  P1 := ALPosA(NameValueSeparator,S);
  if P1 > 0 then begin
    result := True;
    Name := AlCopyStr(S,1,P1-1);
    Value := AlCopyStr(S,P1+1, maxint);
  end
  else begin
    Result := False;
    Name := S;
    Value := '';
  end;
end;

{****************************************************************}
function TALHashedStringListA.GetName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    IndexError(Index, Count - 1);
  Result := FNodelist[Index].ID;
end;

{**********************************************************************}
function TALHashedStringListA.GetStrictName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    IndexError(Index, Count - 1);
  if FNodelist[Index].nvs then Result := FNodelist[Index].ID
  else result := ''
end;

{*************************************************************************}
function TALHashedStringListA.GetValue(const Name: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then begin
    if FNodelist[i].nvs then Result := FNodelist[i].Val
    else Result := '';
  end else
    Result := '';
end;

{*********************************************************************}
procedure TALHashedStringListA.SetValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then AddNameValue(Name, Value)
    else begin
      Changing;
      FNodeList[i].Val := Value;
      FNodeList[i].NVS := True;
      Changed;
    end
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{**************************************************************************}
function TALHashedStringListA.GetValueFromIndex(Index: Integer): AnsiString;
begin
  if Index >= 0 then
  begin
    if Cardinal(Index) >= Cardinal(Count) then
      IndexError(Index, Count - 1);
    if (FNodeList[index].Nvs) then
      result := FNodeList[index].Val
    else
      Result := '';
  end
  else
    Result := '';
end;

{****************************************************************************************}
procedure TALHashedStringListA.SetValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Value <> '' then
  begin
    if Index < 0 then AddNameValue('', Value)
    else begin
      if Cardinal(Index) >= Cardinal(Count) then
        IndexError(Index, Count - 1);
      Changing;
      FNodeList[Index].Val := Value;
      FNodeList[Index].NVS := True;
      Changed;
    end;
  end
  else
    if Index >= 0 then Delete(Index);
end;

{*******************************************************************************}
procedure TALHashedStringListA.SetPersistentValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then AddNameValue(Name, Value)
  else begin
    Changing;
    FNodeList[I].Val := Value;
    FNodeList[I].NVS := True;
    Changed;
  end
end;

{**************************************************************************************************}
procedure TALHashedStringListA.SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Index < 0 then AddNameValue('', Value)
  else begin
    if Cardinal(Index) >= Cardinal(Count) then
      IndexError(Index, Count - 1);
    Changing;
    FNodeList[Index].Val := Value;
    FNodeList[Index].NVS := True;
    Changed;
  end;
end;

{**************************************************************}
constructor TALStringsEnumeratorW.Create(AStrings: TALStringsW);
begin
  inherited Create;
  FIndex := -1;
  FStrings := AStrings;
end;

{************************************************}
function TALStringsEnumeratorW.GetCurrent: String;
begin
  Result := FStrings[FIndex];
end;

{***********************************************}
function TALStringsEnumeratorW.MoveNext: Boolean;
begin
  Result := FIndex < FStrings.Count - 1;
  if Result then
    Inc(FIndex);
end;

{*****************************}
constructor TALStringsW.Create;
begin
  inherited Create;
  FDefaultEncoding := TEncoding.UTF8;
  FEncoding := nil;
  FWriteBOM := True;
  FDelimiter := ',';
  FLineBreak := sLineBreak;
  FQuoteChar := '"';
  FNameValueSeparator := '=';
  FStrictDelimiter:= False;
  FUpdateCount:= 0;
  fProtectedSave := False;
end;

{*****************************}
destructor TALStringsW.Destroy;
begin
  if (FEncoding <> nil) and (not TEncoding.IsStandardEncoding(FEncoding)) then
    ALFreeAndNil(FEncoding);
  if (FDefaultEncoding <> nil) and (not TEncoding.IsStandardEncoding(FDefaultEncoding)) then
    ALFreeAndNil(FDefaultEncoding);
  inherited Destroy;
end;

{*************************************************}
function TALStringsW.Add(const S: String): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

{*************************************************************************}
function TALStringsW.AddObject(const S: String; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

{********************************************************************}
function TALStringsW.AddNameValue(const Name, Value: String): Integer;
begin
  result := add(name + NameValueSeparator + Value);
end;

{********************************************************************************************}
function TALStringsW.AddNameValueObject(const Name, Value: String; AObject: TObject): Integer;
begin
  result := addObject(name + NameValueSeparator + Value, AObject);
end;

{********************************************}
procedure TALStringsW.Append(const S: String);
begin
  Add(S);
end;

{*****************************************************}
procedure TALStringsW.AddStrings(Strings: TALStringsW);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

{***************************************************************}
procedure TALStringsW.AddStrings(const Strings: array of string);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := Low(Strings) to High(Strings) do
      Add(Strings[I]);
  finally
    EndUpdate;
  end;
end;

{************************************************************************************************}
procedure TALStringsW.AddStrings(const Strings: array of string; const Objects: array of TObject);
var
  I: Integer;
begin
  if Length(Strings) <> Length(Objects) then
    raise EArgumentOutOfRangeException.CreateRes(@System.RTLConsts.sInvalidStringAndObjectArrays);
  BeginUpdate;
  try
    for I := Low(Strings) to High(Strings) do
      AddObject(Strings[I], Objects[I]);
  finally
    EndUpdate;
  end;
end;

{************************************************}
procedure TALStringsW.Assign(Source: TPersistent);
var I: integer;
begin
  if Source is TALStringsW then
  begin
    BeginUpdate;
    try
      Clear;
      // Must use property setter for DefaultEncoding
      DefaultEncoding := TALStringsW(Source).DefaultEncoding;
      // Must use internal property setter for Encoding
      SetEncoding(TALStringsW(Source).Encoding);
      NameValueSeparator := TALStringsW(Source).NameValueSeparator;
      QuoteChar := TALStringsW(Source).QuoteChar;
      Delimiter := TALStringsW(Source).Delimiter;
      LineBreak := TALStringsW(Source).LineBreak;
      StrictDelimiter := TALStringsW(Source).StrictDelimiter;
      WriteBOM := TALStringsW(Source).WriteBOM;
      AddStrings(TALStringsW(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end;
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      Clear;
      // Must use property setter for DefaultEncoding
      DefaultEncoding := TStrings(Source).DefaultEncoding;
      // Must use internal property setter for Encoding
      SetEncoding(TStrings(Source).Encoding);
      NameValueSeparator := TStrings(Source).NameValueSeparator;
      QuoteChar := TStrings(Source).QuoteChar;
      Delimiter := TStrings(Source).Delimiter;
      LineBreak := TStrings(Source).LineBreak;
      StrictDelimiter := TStrings(Source).StrictDelimiter;
      WriteBOM := TStrings(Source).WriteBOM;
      for I := 0 to Tstrings(Source).Count - 1 do
        AddObject(Tstrings(Source)[I], Tstrings(Source).Objects[I]);
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

{************************************************}
procedure TALStringsW.AssignTo(Dest: TPersistent);
var I: integer;
begin
  if Dest is TStrings then
  begin
    Tstrings(Dest).BeginUpdate;
    try
      Tstrings(Dest).Clear;
      Tstrings(Dest).NameValueSeparator := NameValueSeparator;
      Tstrings(Dest).QuoteChar := QuoteChar;
      Tstrings(Dest).Delimiter := Delimiter;
      Tstrings(Dest).LineBreak := LineBreak;
      Tstrings(Dest).StrictDelimiter := StrictDelimiter;
      for I := 0 to Count - 1 do
        Tstrings(Dest).AddObject(get(I), Objects[I]);
    finally
      Tstrings(Dest).EndUpdate;
    end;
    Exit;
  end;
  inherited AssignTo(Dest);
end;

{********************************}
procedure TALStringsW.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

{******************************}
procedure TALStringsW.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

{*********************************************************}
function TALStringsW.Equals(Strings: TALStringsW): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

{$IFOPT O+}
  // Turn off optimizations to force creating a EBP stack frame and
  // place params on the stack.
  {$DEFINE OPTIMIZATIONSON}
  {$O-}
{$ENDIF O+}

{************************************************************}
procedure TALStringsW.Error(const Msg: String; Data: Integer);
begin
  raise EStringListError.CreateFmt(Msg, [Data]) at
    PPointer(PByte(@Msg) + SizeOf(Msg) + SizeOf(Self) + SizeOf(Pointer))^;
end;

{*************************************************************}
procedure TALStringsW.Error(Msg: PResStringRec; Data: Integer);
begin
  raise EStringListError.CreateFmt(LoadResString(Msg), [Data]) at
    PPointer(PByte(@Msg) + SizeOf(Msg) + SizeOf(Self) + SizeOf(Pointer))^;
end;

{***********************************************************}
procedure TALStringsW.IndexError(AIndex, AMaxIndex: Integer);
begin
  raise EStringListError.Create(ListIndexErrorMsg(AIndex, AMaxIndex, Self)) at ReturnAddress;
end;

{$IFDEF OPTIMIZATIONSON}
  {$UNDEF OPTIMIZATIONSON}
  {$O+}
{$ENDIF OPTIMIZATIONSON}

{******************************************************}
procedure TALStringsW.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: String;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

{********************************************************}
function TALStringsW.ExtractName(const S: String): String;
var
  P: Integer;
begin
  Result := S;
  P := ALPosW(NameValueSeparator, Result);

  // change behavior from original Tstring
  // i thing that if a Tstring have an item
  //
  // item1
  //
  // then set MyTstrings.values[item1] := Value1
  // must do
  //
  // item1=Value1
  //
  // instead of what he actually do
  //
  // item1
  // item1=Value1
  //
  // also when MyTStrings contain
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4
  // item5=Value5
  //
  // then doing
  // MyTStrings.valueFromIndex[4] := 'value4'
  // must result in
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4=Value4
  // item5=Value5
  //
  // instead of the current behavior of Tstrings
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // =Value4
  // item5=Value5
  //
  //
  // Original function:
  //
  // if P <> 0 then
  //   SetLength(Result, P-1) else
  //   SetLength(Result, 0);

  if P <> 0 then SetLength(Result, P-1);
end;

{****************************************}
function TALStringsW.GetCapacity: Integer;
begin  // descendents may optionally override/replace this default implementation
  Result := Count;
end;

{****************************************}
function TALStringsW.GetCommaText: String;
var
  LOldDelimiter: Char;
  LOldQuoteChar: Char;
begin
  LOldDelimiter := Delimiter;
  LOldQuoteChar := QuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    Result := GetDelimitedText;
  finally
    Delimiter := LOldDelimiter;
    QuoteChar := LOldQuoteChar;
  end;
end;

{**************************}
{$WARN WIDECHAR_REDUCED OFF}
function TALStringsW.GetDelimitedText: String;
var
  S: String;
  P: PChar;
  I, Count: Integer;
  LDelimiters: set of Char;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := QuoteChar + QuoteChar
  else
  begin
    Result := '';
    LDelimiters := [Char(#0), Char(QuoteChar), Char(Delimiter)];
    if not StrictDelimiter then
      LDelimiters := LDelimiters + [Char(#1)..Char(' ')];
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PChar(S);
      while not (P^ in LDelimiters) do
        Inc(P);
      if (P^ <> #0) then S := ALQuotedStr(S, QuoteChar);
      Result := Result + S + Delimiter;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;
{$WARN WIDECHAR_REDUCED ON}

{********************************************************}
function TALStringsW.GetEnumerator: TALStringsEnumeratorW;
begin
  Result := TALStringsEnumeratorW.Create(Self);
end;

{***************************************}
function TALStringsW.GetIsEmpty: Boolean;
begin
  Result := GetCount = 0;
end;

{***************************************************}
function TALStringsW.GetName(Index: Integer): String;
begin
  Result := ExtractName(Get(Index));
end;

{*********************************************************}
function TALStringsW.GetStrictName(Index: Integer): String;
var P: Integer;
begin
  Result := Get(Index);
  P := ALPosW(NameValueSeparator, Result);
  if P <> 0 then SetLength(Result, P-1)
  else SetLength(Result, 0);
end;

{******************************************************}
function TALStringsW.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

{**********************************}
function TALStringsW.GetText: PChar;
begin
  Result := StrNew(PChar(GetTextStr));
end;

{**************************************}
function TALStringsW.GetTextStr: String;
var
  I, L, Size, Count: Integer;
  P: PChar;
  S, LB: String;
begin
  Count := GetCount;
  Size := 0;
  LB := LineBreak;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      ALMove(Pointer(S)^, P^, L*SizeOf(Char));
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      ALMove(Pointer(LB)^, P^, L*SizeOf(Char));
      Inc(P, L);
    end;
  end;
end;

{********************************************************}
function TALStringsW.GetValue(const Name: String): String;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := ALCopyStr(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

{*****************************************************}
function TALStringsW.IndexOf(const S: String): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

{************************************************************}
function TALStringsW.IndexOfName(const Name: String): Integer;
var
  P: Integer;
  S: String;
begin

  // change behavior from original Tstring
  // i thing that if a Tstring have an item
  //
  // item1
  //
  // then set MyTstrings.values[item1] := Value1
  // must do
  //
  // item1=Value1
  //
  // instead of what he actually do
  //
  // item1
  // item1=Value1
  //
  // also when MyTStrings contain
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4
  // item5=Value5
  //
  // then doing
  // MyTStrings.valueFromIndex[4] := 'value4'
  // must result in
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // item4=Value4
  // item5=Value5
  //
  // instead of the current behavior of Tstrings
  //
  // item1=Value1
  // item2=Value2
  // item3=Value3
  // =Value4
  // item5=Value5
  //
  //
  // Original function:
  //
  // for Result := 0 to GetCount - 1 do
  // begin
  //   S := Get(Result);
  //   P := ALPosA(NameValueSeparator, S);
  //   if (P <> 0) and (CompareStrings(ALCopyStr(S, 1, P - 1), Name) = 0) then Exit;
  // end;
  // Result := -1;

  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := ALPosW(NameValueSeparator, S);
    if ((P <> 0) and (CompareStrings(ALCopyStr(S, 1, P - 1), Name) = 0)) or
       ((P = 0) and (CompareStrings(S, Name) = 0)) then Exit;
  end;
  Result := -1;
end;

{************************************************************}
function TALStringsW.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{******************************************************}
function TALStringsW.Contains(const S: string): Boolean;
begin
  Result := IndexOf(S) >= 0;
end;

{*************************************************************}
function TALStringsW.ContainsName(const Name: string): Boolean;
begin
  Result := IndexOfName(Name) >= 0;
end;

{*******************************************************************}
function TALStringsW.ContainsObject(const AObject: TObject): Boolean;
begin
  Result := IndexOfObject(AObject) >= 0;
end;

{************************************************************************************}
procedure TALStringsW.InsertObject(Index: Integer; const S: String; AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

{*******************************************************************************}
procedure TALStringsW.InsertNameValue(Index: Integer; const Name, Value: String);
begin
  Insert(Index, name + NameValueSeparator + Value);
end;

{*******************************************************************************************************}
procedure TALStringsW.InsertNameValueObject(Index: Integer; const Name, Value: String; AObject: TObject);
begin
  InsertObject(Index, name + NameValueSeparator + Value, AObject);
end;

{*********************************************************}
procedure TALStringsW.LoadFromFile(const FileName: String);
begin
  LoadFromFile(FileName, nil);
end;

{******************************************************************************}
procedure TALStringsW.LoadFromFile(const FileName: string; Encoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, Encoding);
  finally
    ALFreeAndNil(Stream);
  end;
end;

{****************************************************}
procedure TALStringsW.LoadFromStream(Stream: TStream);
begin
  LoadFromStream(Stream, nil);
end;

{*************************************************************************}
procedure TALStringsW.LoadFromStream(Stream: TStream; Encoding: TEncoding);
var
  Size: Integer;
  Buffer: TBytes;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetLength(Buffer, Size);
    {$IF CompilerVersion >= 30}{Delphi seattle}
    Stream.ReadBuffer(Buffer, 0, Size);
    {$else}
    Stream.Read(Buffer, 0, Size);
    {$ENDIF}
    Size := TEncoding.GetBufferEncoding(Buffer, Encoding, FDefaultEncoding);
    SetEncoding(Encoding); // Keep Encoding in case the stream is saved
    SetTextStr(Encoding.GetString(Buffer, Size, Length(Buffer) - Size));
  finally
    EndUpdate;
  end;
end;

{******************************************************}
procedure TALStringsW.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: String;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      PutObject(CurIndex, nil);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

{*********************************************************}
procedure TALStringsW.Put(Index: Integer; const S: String);
begin
end;

{****************************************************************}
procedure TALStringsW.PutObject(Index: Integer; AObject: TObject);
begin
end;

{*******************************************************}
procedure TALStringsW.SaveToFile(const FileName: string);
begin
  SaveToFile(FileName, FEncoding);
end;

{****************************************************************************}
procedure TALStringsW.SaveToFile(const FileName: string; Encoding: TEncoding);
Var LFileStream: TfileStream;
    LTmpFilename: String;
begin
  if ProtectedSave then LTmpFilename := FileName + '.~tmp'
  else LTmpFilename := FileName;
  try

    LFileStream := TfileStream.Create(LTmpFilename,fmCreate);
    Try
      SaveToStream(LFileStream, Encoding);
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

{**************************************************}
procedure TALStringsW.SaveToStream(Stream: TStream);
begin
  SaveToStream(Stream, FEncoding);
end;

{***********************************************************************}
procedure TALStringsW.SaveToStream(Stream: TStream; Encoding: TEncoding);
var
  Buffer, Preamble: TBytes;
begin
  if Encoding = nil then
    Encoding := FDefaultEncoding;
  Buffer := Encoding.GetBytes(GetTextStr);
  if WriteBOM then
  begin
    Preamble := Encoding.GetPreamble;
    if Length(Preamble) > 0 then
      Stream.WriteBuffer(Preamble, Length(Preamble));
  end;
  Stream.WriteBuffer(Buffer, Length(Buffer));
end;

{******************************************************}
procedure TALStringsW.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendents may optionally implement this method
end;

{******************************************************}
procedure TALStringsW.SetCommaText(const Value: String);
var
  LOldDelimiter: Char;
  LOldQuoteChar: Char;
begin
  LOldDelimiter := Delimiter;
  LOldQuoteChar := QuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    SetDelimitedText(Value);
  finally
    Delimiter := LOldDelimiter;
    QuoteChar := LOldQuoteChar;
  end;
end;

{*****************************************}
procedure TALStringsW.SetText(Text: PChar);
begin
  SetTextStr(Text);
end;

{**************************}
{$WARN WIDECHAR_REDUCED OFF}
procedure TALStringsW.SetTextStr(const Value: String);
var
  P, PCurVal, PCurLB, PStartVal, PEndVal, PStartLB, PEndLB: PChar;
  S: string;
  LineBreakLen: Integer;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P = nil then
      Exit;

    LineBreakLen := Length(LineBreak);
    if LineBreakLen = 0 then
    begin
      Add(Value);
      Exit;
    end;

    PEndVal := P + Length(Value);

    // When LineBreak is:
    // * sLineBreak - for compatibility with Windows, Posix and old macOS platforms,
    //   we handle #13#10, #10 and #13 as it would be #13#10.
    // * NOT sLineBreak - we use strict checking for LineBreak.
    if ALCompareStrW(LineBreak, sLineBreak) = 0 then
    begin
      while P < PEndVal do
      begin
        PStartVal := P;
        while (P < PEndVal) and not (P^ in [#10, #13]) do Inc(P);
        SetString(S, PStartVal, P - PStartVal);
        Add(S);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
      Exit;
    end;

    PStartLB := Pointer(LineBreak);
    PEndLB := PStartLB + LineBreakLen;
    PCurLB := PStartLB;
    PStartVal := P;

    while P < PEndVal do
    begin
      while (P < PEndVal) and (P^ <> PStartLB^) do
        Inc(P);
      if P < PEndVal then
      begin
        PCurVal := P + 1;
        Inc(PCurLB);
        while (PCurLB < PEndLB) and (PCurVal < PEndVal) and (PCurVal^ = PCurLB^) do
        begin
          Inc(PCurVal);
          Inc(PCurLB)
        end;
        if PCurLB = PEndLB then
        begin
          SetString(S, PStartVal, P - PStartVal);
          Add(S);
          P := PCurVal;
          PStartVal := P;
        end
        else
          Inc(P);
        PCurLB := PStartLB;
      end;
    end;

    if P > PStartVal then
    begin
      SetString(S, PStartVal, P - PStartVal);
      Add(S);
    end;
  finally
    EndUpdate;
  end;
end;
{$WARN WIDECHAR_REDUCED ON}

{******************************************************}
procedure TALStringsW.SetUpdateState(Updating: Boolean);
begin
end;

{********************************************************}
procedure TALStringsW.SetValue(const Name, Value: String);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then Add(Name + NameValueSeparator + Value)
    else Put(I, Name + NameValueSeparator + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{******************************************************************}
procedure TALStringsW.SetPersistentValue(const Name, Value: String);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then Add(Name + NameValueSeparator + Value)
  else Put(I, Name + NameValueSeparator + Value);
end;

{***************************************************************}
procedure TALStringsW.SetDefaultEncoding(const Value: TEncoding);
begin
  if (FDefaultEncoding <> nil) and (not TEncoding.IsStandardEncoding(FDefaultEncoding)) then
    ALFreeAndNil(FDefaultEncoding);
  if TEncoding.IsStandardEncoding(Value) then
    FDefaultEncoding := Value
  else if Value <> nil then
    FDefaultEncoding := Value.Clone
  else
    FDefaultEncoding := TEncoding.UTF8;
end;

{**********************************************************}
procedure TALStringsW.SetDelimitedText(const Value: String);
var
  P, P1: PChar;
  S: String;
begin
  BeginUpdate;
  try
    Clear;
    P := PChar(Value);
    if not StrictDelimiter then
      while ((P^ >= char(#1)) and (P^ <= char(' '))) do
        Inc(P);
    while P^ <> #0 do
    begin
      if P^ = QuoteChar then
        S := ALExtractQuotedStr(P, QuoteChar)
      else
      begin
        P1 := P;
        while ((not StrictDelimiter and (P^ > ' ')) or
              (StrictDelimiter and (P^ <> #0))) and (P^ <> Delimiter) do
          Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);
      if not StrictDelimiter then
        while ((P^ >= char(#1)) and (P^ <= char(' '))) do
          Inc(P);

      if P^ = Delimiter then
      begin
        P1 := P;
        Inc(P1);
        if P1^ = #0 then
          Add('');
        repeat
          Inc(P);
        until not (not StrictDelimiter and ((P^ >= char(#1)) and (P^ <= char(' '))));
      end;
    end;
  finally
    EndUpdate;
  end;
end;

{********************************************************}
procedure TALStringsW.SetEncoding(const Value: TEncoding);
begin
  if (FEncoding <> nil) and (not TEncoding.IsStandardEncoding(FEncoding)) then
    ALFreeAndNil(FEncoding);
  if TEncoding.IsStandardEncoding(Value) then
    FEncoding := Value
  else if Value <> nil then
    FEncoding := Value.Clone
  else
    FEncoding := TEncoding.UTF8;
end;

{*****************************************************************}
function TALStringsW.CompareStrings(const S1, S2: String): Integer;
begin
  Result := ALCompareTextW(S1, S2);
end;

{*************************************************************}
function TALStringsW.GetValueFromIndex(Index: Integer): String;
var
  SepPos: Integer;
begin
  if Index >= 0 then
  begin
    Result := Get(Index);
    SepPos := ALPosW(NameValueSeparator, Result);
    if (SepPos > 0) then
      System.Delete(Result, 1, SepPos)
    else
      Result := '';
  end
  else
    Result := '';
end;

{***************************************************************************}
procedure TALStringsW.SetValueFromIndex(Index: Integer; const Value: String);
begin
  if Value <> '' then
  begin
    if Index < 0 then Add(NameValueSeparator + Value)
    else Put(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

{*************************************************************************************}
procedure TALStringsW.SetPersistentValueFromIndex(Index: Integer; const Value: String);
begin
  if Index < 0 then Add(NameValueSeparator + Value)
  else Put(Index, Names[Index] + NameValueSeparator + Value);
end;

{*************************************************}
function TALStringsW.ToStringArray: TArray<String>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Strings[I];
end;

{**************************************************}
function TALStringsW.ToObjectArray: TArray<TObject>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Objects[I];
end;

{*******************************************************************}
function TALStringsW.ToNameValueArray: TArray<TPair<String, String>>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := TPair<String, String>.create(Names[i], ValueFromIndex[I]);
end;

{********************************}
destructor TALStringListW.Destroy;
var
  I: Integer;
  Temp: Array of TObject;
begin
  FOnChange := nil;
  FOnChanging := nil;

  // If the list owns the Objects gather them and free after the list is disposed
  if OwnsObjects then
  begin
    SetLength(Temp, FCount);
    for I := 0 to FCount - 1 do
      Temp[I] := FList[I].FObject;
  end;

  inherited Destroy;
  FCount := 0;
  SetCapacity(0);

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      ALFreeAndNil(Temp[I]);
end;

{****************************************************}
function TALStringListW.Add(const S: String): Integer;
begin
  Result := AddObject(S, nil);
end;

{****************************************************************************}
function TALStringListW.AddObject(const S: String; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0);
      end;
  InsertItem(Result, S, AObject);
end;

{***************************************************}
procedure TALStringListW.Assign(Source: TPersistent);
begin
  if Source is TALStringListW then
  begin
    Clear;
    FCaseSensitive := TALStringListW(Source).FCaseSensitive;
    FDuplicates := TALStringListW(Source).FDuplicates;
    FSorted := TALStringListW(Source).FSorted;
  end
  else if Source is TALNVStringListW then
  begin
    Clear;
    FCaseSensitive := TALNVStringListW(Source).FCaseSensitive;
    FDuplicates := TALNVStringListW(Source).FDuplicates;
    FSorted := TALNVStringListW(Source).FSorted;
  end
  else if Source is TStringList then
  begin
    Clear;
    CaseSensitive := TStringList(Source).CaseSensitive;
    Duplicates := TStringList(Source).Duplicates;
    Sorted := TStringList(Source).Sorted;
  end;
  inherited Assign(Source);
end;

{***************************************************}
procedure TALStringListW.AssignTo(Dest: TPersistent);
begin
  if Dest is TStringList then
  begin
    TStringList(Dest).clear;
    TStringList(Dest).CaseSensitive := fCaseSensitive;
    TStringList(Dest).Duplicates := fDuplicates;
    TStringList(Dest).Sorted := fSorted;
  end;
  inherited AssignTo(Dest);
end;

{*******************************}
procedure TALStringListW.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{********************************}
procedure TALStringListW.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{*****************************}
procedure TALStringListW.Clear;
var
  I: Integer;
  Temp: Array of TObject;
begin
  if FCount <> 0 then
  begin
    Changing;

    // If the list owns the Objects gather them and free after the list is disposed
    if OwnsObjects then
    begin
      SetLength(Temp, FCount);
      for I := 0 to FCount - 1 do
        Temp[I] := FList[I].FObject;
    end;

    FCount := 0;
    SetCapacity(0);

    // Free the objects that were owned by the list
    if Length(Temp) > 0 then
      for I := 0 to Length(Temp) - 1 do
        ALFreeAndNil(Temp[I]);

    Changed;
  end;
end;

{**********************************************}
procedure TALStringListW.Delete(Index: Integer);
var
  Obj: TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index, FCount - 1);
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  // Direct memory writing to managed array follows
  //  see http://dn.embarcadero.com/article/33423
  // Explicitly finalize the element we about to stomp on with move
  Finalize(FList[Index]);
  Dec(FCount);
  if Index < FCount then
  begin
    ALMove(
      FList[Index + 1],
      FList[Index],
      (FCount - Index) * SizeOf(TALStringItemW));
    // Make sure there is no danglng pointer in the last (now unused) element
    PPointer(@FList[FCount].FString)^ := nil;
    PPointer(@FList[FCount].FObject)^ := nil;
  end;
  if Obj <> nil then
    ALFreeAndNil(Obj);
  Changed;
end;

{**************************************************************}
function  TALStringListW.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index, FCount - 1);
  Changing;
  result := FList[Index].FObject;
  FList[Index].FObject := nil;
  Changed;
end;

{*********************************************************}
procedure TALStringListW.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    IndexError(Index1, FCount - 1);
  if (Index2 < 0) or (Index2 >= FCount) then
    IndexError(Index2, FCount - 1);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{**************************************************************}
procedure TALStringListW.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Pointer;
  Item1, Item2: PALStringItemW;
begin
  Item1 := @FList[Index1];
  Item2 := @FList[Index2];
  Temp := Pointer(Item1^.FString);
  Pointer(Item1^.FString) := Pointer(Item2^.FString);
  Pointer(Item2^.FString) := Temp;
  Temp := pointer(Item1^.FObject);
  pointer(Item1^.FObject) := pointer(Item2^.FObject);
  pointer(Item2^.FObject) := Temp;
end;

{*************************************************************************}
function TALStringListW.Find(const S: String; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FString, S);
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

{*****************************************************************************}
function TALStringListW.FindName(const S: String; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(ExtractName(FList[I].FString), S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        //if Duplicates <> dupAccept then L := I; //because we need the last value of name in any case
                                                  //ex: a
                                                  //    aaa
                                                  //    aaa=
                                                  //    bbb
                                                  //then doing values['aaa'] := 'xxx' must result in
                                                  //    a
                                                  //    aaa
                                                  //    aaa=xxx
                                                  //    bbb
                                                  //and not in
                                                  //    a
                                                  //    aaa=xxx
                                                  //    aaa=
                                                  //    bbb
        L := I; // this mean L > H
        I := I + 1;
        while I <= FCount - 1 do begin
          if CompareStrings(ExtractName(FList[I].FString), s) = 0 then L := I
          else break;
          I := I + 1;
        end;
      end;
    end;
  end;
  Index := L;
end;

{**************************************************}
function TALStringListW.Get(Index: Integer): String;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  Result := FList[Index].FString;
end;

{*******************************************}
function TALStringListW.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

{****************************************}
function TALStringListW.GetCount: Integer;
begin
  Result := FCount;
end;

{*********************************************************}
function TALStringListW.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  Result := FList[Index].FObject;
end;

{****************************}
procedure TALStringListW.Grow;
{$IF CompilerVersion <= 32}{tokyo}
var
  Delta: Integer;
{$endif}
begin
  {$IF CompilerVersion <= 32}{tokyo}
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
  {$else}
  SetCapacity(GrowCollection(FCapacity, FCount + 1));
  {$endif}
end;

{********************************************************}
function TALStringListW.IndexOf(const S: String): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

{***************************************************************}
function TALStringListW.IndexOfName(const Name: String): Integer;
begin
  if (not Sorted) or (not FNameValueOptimization) then Result := inherited IndexOfName(Name)
  else if not FindName(Name, Result) then Result := -1;
end;

{***************************************************************}
procedure TALStringListW.Insert(Index: Integer; const S: String);
begin
  InsertObject(Index, S, nil);
end;

{***************************************************************************************}
procedure TALStringListW.InsertObject(Index: Integer; const S: String; AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then
    IndexError(Index, FCount);
  InsertItem(Index, S, AObject);
end;

{*********************************************************}
procedure TALStringListW.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: String;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      FList[CurIndex].FObject := nil;
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

{*************************************************************************************}
procedure TALStringListW.InsertItem(Index: Integer; const S: String; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    ALMove(
      FList[Index],
      FList[Index + 1],
      (FCount - Index) * SizeOf(TALStringItemW));
  Pointer(FList[Index].FString) := nil;
  Pointer(FList[Index].FObject) := nil;
  FList[Index].FObject := AObject;
  FList[Index].FString := S;
  Inc(FCount);
  Changed;
end;

{************************************************************}
procedure TALStringListW.Put(Index: Integer; const S: String);
begin
  if not sorted then begin
    if Cardinal(Index) >= Cardinal(FCount) then
      IndexError(Index, FCount - 1);
    Changing;
    FList[Index].FString := S;
    Changed;
  end
  else begin
    delete(index);
    add(s);
  end;
end;

{*******************************************************************}
procedure TALStringListW.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  FList[Index].FObject := AObject;

  if Obj <> nil then
    ALFreeAndNil(Obj);

  Changed;
end;

{*************************************************************************************}
procedure TALStringListW.QuickSort(L, R: Integer; ACompare: TALStringListSortCompareW);
var
  I, J, P: Integer;
begin
  while L < R do
  begin
    if (R - L) = 1 then
    begin
      if ACompare(Self, L, R) > 0 then
        ExchangeItems(L, R);
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
          ExchangeItems(I, J);
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

{*********************************************************}
procedure TALStringListW.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{*************************************************}
procedure TALStringListW.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

{*********************************************************}
procedure TALStringListW.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{*******************************************************************************************}
function ALStringListCompareStringsW(List: TALStringListW; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(
              List.FList[Index1].FString,
              List.FList[Index2].FString);
end;

{****************************}
procedure TALStringListW.Sort;
begin
  CustomSort(ALStringListCompareStringsW);
end;

{**********************************************************************}
procedure TALStringListW.CustomSort(Compare: TALStringListSortCompareW);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

{********************************************************************}
function TALStringListW.CompareStrings(const S1, S2: String): Integer;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function internalCompareStr(const S1, S2: String): Integer;
  var
    P1, P2: PChar;
    I: Integer;
    L1, L2: Integer;
  begin
    { Length and PChar of S1 }
    L1 := Length(S1);
    P1 := PChar(S1);

    { Length and PChar of S2 }
    L2 := Length(S2);
    P2 := PChar(S2);

    { Continue the loop until the end of one string is reached. }
    I := 0;
    while (I < L1) and (I < L2) do
    begin
      if (P1^ <> P2^) then begin
        if (P1^ = NameValueSeparator) then result := -1
        else if (P2^ = NameValueSeparator) then result := 1
        else result := Ord(P1^) - Ord(P2^);
        Exit;
      end;

      Inc(P1);
      Inc(P2);
      Inc(I);
    end;

    { If chars were not different return the difference in length }
    Result := L1 - L2;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function internalCompareText(const S1, S2: String): Integer;
  var
    P1, P2: PChar;
    I: Integer;
    C1, C2: Char;
    L1, L2: Integer;
  begin
    { Length and PChar of S1 }
    L1 := Length(S1);
    P1 := PChar(S1);

    { Length and PChar of S2 }
    L2 := Length(S2);
    P2 := PChar(S2);

    { Continue the loop until the end of one string is reached. }
    I := 0;
    while (I < L1) and (I < L2) do
    begin
      if (P1^ >= char('a')) and (P1^ <= char('z')) then
        C1 := Char(Byte(P1^) xor $20)
      else
        C1 := P1^;

      if (P2^ >= char('a')) and (P2^ <= char('z')) then
        C2 := Char(Byte(P2^) xor $20)
      else
        C2 := P2^;

      if (C1 <> C2) then begin
        if (C1 = NameValueSeparator) then result := -1
        else if (C2 = NameValueSeparator) then result := 1
        else result := Ord(C1) - Ord(C2);
        Exit;
      end;

      Inc(P1);
      Inc(P2);
      Inc(I);
    end;

    { If chars were not different return the difference in length }
    Result := L1 - L2;
  end;

begin

  // Orignial Delphi Code
  // the difference between TALStringListA and TStringList is that
  // TstringList use ansiCompareStr or ansiCompareText that are
  // dependant from the local. I don't like this behavior because
  // as you can read
  // http://msdn.microsoft.com/en-us/library/windows/desktop/dd317759(v=vs.85).aspx
  // "Using CompareString incorrectly can compromise the security of your
  // application. Strings that are not compared correctly can produce
  // invalid input. For example, the function can raise security issues when
  // used for a non-linguistic comparison, because two strings that are
  // distinct in their binary representation can be linguistically equivalent"
  // so i prefere to use instead CompareStr and CompareText but only
  // a..z = A..Z will be handle when case insensitive is set.
  // other behavior must be handle in descendant classe
  //
  // also not the ansiCompareStr and ansiCompareText
  // are 10x more slower than CompareStr and CompareText
  //
  // if CaseSensitive then
  //   Result := AnsiCompareStr(S1, S2)
  // else
  //   Result := AnsiCompareText(S1, S2);

  // it's important that the order is not change because
  // of the ord(NameValueSeparator) this is need because of
  // function FindName
  //
  // EX the items
  //
  //   aaa0
  //   aaa=123
  //   aaaa
  //
  // must be ordered like
  //
  //   aaa=123   |     aaa
  //   aaa0      |     aaa0
  //   aaaa      |     aaaa
  //                   => OK, ordered work with findname
  //
  // but with just Result := ALCompareTextA(S1, S2)
  // it's will be ordered like
  //
  //   aaa0      |     aaa0
  //   aaa=123   |     aaa
  //   aaaa      |     aaaa
  //                   => KO, NOT ordered, break the findname
  //
  // so we need to give to the NameValueSeparator the lowest ASCII
  // number (#0). for this we must use custom CompareStr and
  // custom CompareText. the cost is that our custom implementation
  // of internalCompareStr and internalCompareText (based on the pure
  // pascal implementation of CompareStr and CompareText, will be aound
  // 50% more slower than the ASM version of CompareStr and CompareText
  // IE 50% more slower with average 50 bytes for S1 and S2, it's even
  // more slower when the number of bytes in S1 and S2 increase

  if  fNameValueOptimization then begin
    if CaseSensitive then
      Result := InternalCompareStr(S1, S2)
    else
      Result := InternalCompareText(S1, S2);
  end
  else begin
    if CaseSensitive then
      Result := ALCompareStrW(S1, S2)
    else
      Result := ALCompareTextW(S1, S2);
  end;
end;

{**************************************************}
procedure TALStringListW.init(OwnsObjects: Boolean);
begin
  setlength(FList, 0);
  FCount := 0;
  FCapacity := 0;
  FSorted := False;
  FDuplicates := dupIgnore;
  FCaseSensitive := False;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := OwnsObjects;
  FNameValueOptimization := True;
end;

{********************************}
constructor TALStringListW.Create;
begin
  inherited Create;
  init(False);
end;

{******************************************************}
constructor TALStringListW.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects);
end;

{**************************************************************}
procedure TALStringListW.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then
    begin
      // Calling Sort won't sort the list because CustomSort will
      // only sort the list if it's not already sorted
      Sorted := False;
      Sorted := True;
    end;
  end;
end;

{**********************************}
destructor TALNVStringListW.Destroy;
var
  I: Integer;
  Temp: Array of TObject;
begin
  FOnChange := nil;
  FOnChanging := nil;

  // If the list owns the Objects gather them and free after the list is disposed
  if OwnsObjects then
  begin
    SetLength(Temp, FCount);
    for I := 0 to FCount - 1 do
      Temp[I] := FList[I].FObject;
  end;

  inherited Destroy;
  FCount := 0;
  SetCapacity(0);

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      ALFreeAndNil(Temp[I]);
end;

{******************************************************}
function TALNVStringListW.Add(const S: String): Integer;
begin
  Result := AddObject(S, nil);
end;

{******************************************************************************}
function TALNVStringListW.AddObject(const S: String; AObject: TObject): Integer;
Var LName, LValue: String;
begin
  if not Sorted then begin
    Result := FCount;
    InsertItem(Result, S, AObject);
  end
  else begin
    if ExtractNameValue(S, LName, LValue) then begin
      if FindNameValue(LName, LValue, Result) then
        case Duplicates of
          dupIgnore: Exit;
          dupError: Error(@SDuplicateString, 0);
        end;
      InsertItem(Result, LName, LValue, AObject);
    end
    else begin
      if FindName(s, false{WithNvS}, Result) then
        case Duplicates of
          dupIgnore: Exit;
          dupError: Error(@SDuplicateString, 0);
        end;
      InsertItem(Result, s, False{WithNvS}, AObject);
    end;
  end;
end;

{*************************************************************************}
function TALNVStringListW.AddNameValue(const Name, Value: String): Integer;
begin
  Result := AddNameValueObject(Name, Value, nil);
end;

{*************************************************************************************************}
function TALNVStringListW.AddNameValueObject(const Name, Value: String; AObject: TObject): Integer;
begin
  if not Sorted then begin
    Result := FCount;
  end
  else begin
    if FindNameValue(Name, Value, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0);
      end;
  end;
  InsertItem(Result, Name, Value, AObject);
end;

{*****************************************************}
procedure TALNVStringListW.Assign(Source: TPersistent);
begin
  if Source is TALNVStringListW then
  begin
    Clear;
    FCaseSensitive := TALNVStringListW(Source).FCaseSensitive;
    FDuplicates := TALNVStringListW(Source).FDuplicates;
    FSorted := TALNVStringListW(Source).FSorted;
  end
  else if Source is TALStringListW then
  begin
    Clear;
    FCaseSensitive := TALStringListW(Source).FCaseSensitive;
    FDuplicates := TALStringListW(Source).FDuplicates;
    FSorted := TALStringListW(Source).FSorted;
  end
  else if Source is TStringList then
  begin
    Clear;
    CaseSensitive := TStringList(Source).CaseSensitive;
    Duplicates := TStringList(Source).Duplicates;
    Sorted := TStringList(Source).Sorted;
  end;
  inherited Assign(Source);
end;

{*****************************************************}
procedure TALNVStringListW.AssignTo(Dest: TPersistent);
begin
  if Dest is TStringList then
  begin
    TStringList(Dest).clear;
    TStringList(Dest).CaseSensitive := fCaseSensitive;
    TStringList(Dest).Duplicates := fDuplicates;
    TStringList(Dest).Sorted := fSorted;
  end;
  inherited AssignTo(Dest);
end;

{*********************************}
procedure TALNVStringListW.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{**********************************}
procedure TALNVStringListW.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{*******************************}
procedure TALNVStringListW.Clear;
var
  I: Integer;
  Temp: Array of TObject;
begin
  if FCount <> 0 then
  begin
    Changing;

    // If the list owns the Objects gather them and free after the list is disposed
    if OwnsObjects then
    begin
      SetLength(Temp, FCount);
      for I := 0 to FCount - 1 do
        Temp[I] := FList[I].FObject;
    end;

    FCount := 0;
    SetCapacity(0);

    // Free the objects that were owned by the list
    if Length(Temp) > 0 then
      for I := 0 to Length(Temp) - 1 do
        ALFreeAndNil(Temp[I]);

    Changed;
  end;
end;

{************************************************}
procedure TALNVStringListW.Delete(Index: Integer);
var
  Obj: TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index, FCount - 1);
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  // Direct memory writing to managed array follows
  //  see http://dn.embarcadero.com/article/33423
  // Explicitly finalize the element we about to stomp on with move
  Finalize(FList[Index]);
  Dec(FCount);
  if Index < FCount then
  begin
    ALMove(
      FList[Index + 1],
      FList[Index],
      (FCount - Index) * SizeOf(TALNVStringItemW));
    // Make sure there is no danglng pointer in the last (now unused) element
    PPointer(@FList[FCount].FName)^   := nil;
    PPointer(@FList[FCount].FValue)^  := nil;
    PPointer(@FList[FCount].FObject)^ := nil;
  end;
  if Obj <> nil then
    ALFreeAndNil(Obj);
  Changed;
end;

{****************************************************************}
function  TALNVStringListW.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index, FCount - 1);
  Changing;
  result := FList[Index].FObject;
  FList[Index].FObject := nil;
  Changed;
end;

{***********************************************************}
procedure TALNVStringListW.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    IndexError(Index1, FCount - 1);
  if (Index2 < 0) or (Index2 >= FCount) then
    IndexError(Index2, FCount - 1);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{****************************************************************}
procedure TALNVStringListW.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Pointer;
  Item1, Item2: PALNVStringItemW;
begin
  Item1 := @FList[Index1];
  Item2 := @FList[Index2];

  Temp := Pointer(Item1^.FName);
  Pointer(Item1^.FName) := Pointer(Item2^.FName);
  Pointer(Item2^.FName) := Temp;

  Temp := Pointer(Item1^.FValue);
  Pointer(Item1^.FValue) := Pointer(Item2^.FValue);
  Pointer(Item2^.FValue) := Temp;

  Temp := pointer(Item1^.FObject);
  pointer(Item1^.FObject) := pointer(Item2^.FObject);
  pointer(Item2^.FObject) := Temp;
end;

{***************************************************************************}
function TALNVStringListW.Find(const S: String; var Index: Integer): Boolean;
var Name, Value: String;
begin
  if ExtractNameValue(S, Name, Value) then result := FindNameValue(Name, Value, Index)
  else result := FindName(Name, False{WithNvS}, Index);
end;

{**********************************************************************************}
function TALNVStringListW.FindName(const Name: String; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FName, Name);     // The return value is less than 0 if FList[I].FName < Name, 0 if FList[I].FName = Name, or greater than 0 if FList[I].FName > Name.
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        //if Duplicates <> dupAccept then L := I; //because we need the last value of name in any case
                                                  //ex: a
                                                  //    aaa
                                                  //    aaa=
                                                  //    bbb
                                                  //then doing values['aaa'] := 'xxx' must result in
                                                  //    a
                                                  //    aaa
                                                  //    aaa=xxx
                                                  //    bbb
                                                  //and not in
                                                  //    a
                                                  //    aaa=xxx
                                                  //    aaa=
                                                  //    bbb
        L := I; // this mean L > H
        I := I + 1;
        while I <= FCount - 1 do begin
          if CompareStrings(FList[I].FName, Name) = 0 then L := I
          else break;
          I := I + 1;
        end;
      end;
    end;
  end;
  Index := L;
end;

{****************************************************************************************************}
function TALNVStringListW.FindName(const Name: String; WithNvS: boolean; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FName, Name);     // The return value is less than 0 if FList[I].FName < Name, 0 if FList[I].FName = Name, or greater than 0 if FList[I].FName > Name.
    if (C = 0) then begin
      if (not WithNvS) and FList[I].FNVS then c := 1   // Must be ordered in this order :
                                                       // aa
                                                       // aaa
                                                       // aaa=
                                                       // aaaa
      else if (WithNvS) and (not FList[I].FNVS) then c := -1;  // Must be ordered in this order :
                                                               // aa
                                                               // aaa
                                                               // aaa=
                                                               // aaaa
    end;
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        //if Duplicates <> dupAccept then L := I; //because we need the last value of name in any case
                                                  //ex: a
                                                  //    aaa
                                                  //    aaa=
                                                  //    bbb
                                                  //then doing values['aaa'] := 'xxx' must result in
                                                  //    a
                                                  //    aaa
                                                  //    aaa=xxx
                                                  //    bbb
                                                  //and not in
                                                  //    a
                                                  //    aaa=xxx
                                                  //    aaa=
                                                  //    bbb
        L := I; // this mean L > H
        I := I + 1;
        while I <= FCount - 1 do begin
          C := CompareStrings(FList[I].FName, Name);
          if (C = 0) then begin
            if (not WithNvS) and FList[I].FNVS then c := 1   // Must be ordered in this order :
                                                             // aa
                                                             // aaa
                                                             // aaa=
                                                             // aaaa
            else if (WithNvS) and (not FList[I].FNVS) then c := -1;  // Must be ordered in this order :
                                                                     // aa
                                                                     // aaa
                                                                     // aaa=
                                                                     // aaaa
          end;
          if c = 0 then L := I
          else break;
          I := I + 1;
        end;
      end;
    end;
  end;
  Index := L;
end;

{**********************************************************************************************}
function TALNVStringListW.FindNameValue(const Name, Value: String; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FName, Name);  // The return value is less than 0 if FList[I].FName < Name, 0 if FList[I].FName = Name, or greater than 0 if FList[I].FName > Name.
    if (C = 0) then begin
      if (not FList[I].FNVS) then c := -1 // Must be ordered in this order :
                                          // aa
                                          // aaa
                                          // aaa=
                                          // aaaa
      else C := CompareStrings(FList[I].FValue, Value);  // The return value is less than 0 if FList[I].FValue < Value, 0 if FList[I].FValue = Value, or greater than 0 if FList[I].FValue > Value.
    end;
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

{****************************************************}
function TALNVStringListW.Get(Index: Integer): String;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  if FList[Index].fNvs then Result := FList[Index].FName + NameValueSeparator + FList[Index].fValue
  else Result := FList[Index].fname;
end;

{*********************************************}
function TALNVStringListW.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

{******************************************}
function TALNVStringListW.GetCount: Integer;
begin
  Result := FCount;
end;

{***********************************************************}
function TALNVStringListW.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  Result := FList[Index].FObject;
end;

{******************************}
procedure TALNVStringListW.Grow;
{$IF CompilerVersion <= 32}{tokyo}
var
  Delta: Integer;
{$endif}
begin
  {$IF CompilerVersion <= 32}{tokyo}
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
  {$else}
  SetCapacity(GrowCollection(FCapacity, FCount + 1));
  {$endif}
end;

{*******************************************}
function TALNVStringListW.GetTextStr: String;
var
  I, L, Size: Integer;
  P: PChar;
  S, LB: String;
  NvS: Char;
begin
  Size := 0;
  LB := LineBreak;
  NvS := nameValueSeparator;
  for I := 0 to FCount - 1 do begin
    if FList[i].fNvs then Inc(Size, Length(FList[i].fName) + 1{length(NameValueSeparator)} +  Length(FList[i].fValue) + Length(LB))
    else Inc(Size, Length(FList[i].fName) + Length(LB))
  end;
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to FCount - 1 do
  begin
    S := FList[i].fName;
    L := Length(S);
    if L <> 0 then
    begin
      ALMove(Pointer(S)^, P^, L*SizeOf(Char));
      Inc(P, L);
    end;
    if FList[i].fNvs then begin
      ALMove(NvS, P^, 1*SizeOf(Char));
      Inc(P, 1);
      S := FList[i].fValue;
      L := Length(S);
      if L <> 0 then
      begin
        ALMove(Pointer(S)^, P^, L*SizeOf(Char));
        Inc(P, L);
      end;
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      ALMove(Pointer(LB)^, P^, L*SizeOf(Char));
      Inc(P, L);
    end;
  end;
end;

{**********************************************************}
function TALNVStringListW.IndexOf(const S: String): Integer;
Var LName, LValue: String;
begin
  if ExtractNameValue(S, LName, LValue) then begin
    if not Sorted then begin
      for Result := 0 to FCount - 1 do
        if Flist[Result].FNVS and
           (CompareStrings(Flist[Result].FName, LName) = 0) and
           (CompareStrings(Flist[Result].FValue, LValue) = 0) then Exit;
      Result := -1;
    end
    else begin
      if not FindNameValue(LName, LValue, Result) then Result := -1;
    end;
  end
  else begin
    if not Sorted then begin
      for Result := 0 to FCount - 1 do
        if (not Flist[Result].FNVS) and
           (CompareStrings(Flist[Result].FName, s) = 0) then Exit;
      Result := -1;
    end
    else begin
      if not FindName(s, false{WinthNvS}, Result) then Result := -1;
    end;
  end;
end;

{*****************************************************************}
function TALNVStringListW.IndexOfName(const Name: String): Integer;
begin
  if not Sorted then begin
    for Result := 0 to FCount - 1 do
      if CompareStrings(Flist[Result].FName, Name) = 0 then Exit;
    Result := -1;
  end
  else begin
    if not FindName(Name, Result) then Result := -1;
  end;
end;

{*****************************************************************}
procedure TALNVStringListW.Insert(Index: Integer; const S: String);
begin
  InsertObject(Index, S, nil);
end;

{*****************************************************************************************}
procedure TALNVStringListW.InsertObject(Index: Integer; const S: String; AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then
    IndexError(Index, FCount);
  InsertItem(Index, S, AObject);
end;

{************************************************************************************}
procedure TALNVStringListW.InsertNameValue(Index: Integer; const Name, Value: String);
begin
  InsertNameValueObject(Index, Name, Value, nil);
end;

{************************************************************************************************************}
procedure TALNVStringListW.InsertNameValueObject(Index: Integer; const Name, Value: String; AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then
    IndexError(Index, FCount - 1);
  InsertItem(Index, Name, Value, AObject);
end;

{***********************************************************}
procedure TALNVStringListW.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempName: String;
  TempValue: String;
  TempNvS: Boolean;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempName := Flist[curIndex].FName;
      TempNvs := Flist[curIndex].FNvs;
      if TempNvs then TempValue := Flist[curIndex].FValue;
      TempObject := Flist[curIndex].FObject;
      FList[CurIndex].FObject := nil;
      Delete(CurIndex);
      if TempNvs then InsertObject(NewIndex, TempName, TempObject)
      else InsertNameValueObject(NewIndex, TempName, TempValue, TempObject)
    finally
      EndUpdate;
    end;
  end;
end;

{***************************************************************************************}
procedure TALNVStringListW.InsertItem(Index: Integer; const S: String; AObject: TObject);
var Name, Value: String;
begin
  if ExtractNameValue(S, Name, Value) then InsertItem(Index, Name, Value, AObject)
  else InsertItem(Index, s, False{WithNvS}, AObject);
end;

{*************************************************************************************************}
procedure TALNVStringListW.InsertItem(Index: Integer; const Name, Value: String; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    ALMove(
      FList[Index],
      FList[Index + 1],
      (FCount - Index) * SizeOf(TALNVStringItemW));
  Pointer(FList[Index].FName) := nil;
  Pointer(FList[Index].FValue) := nil;
  Pointer(FList[Index].FObject) := nil;
  FList[Index].FObject := AObject;
  FList[Index].FName := Name;
  FList[Index].fNvs := true;
  FList[Index].FValue := Value;
  Inc(FCount);
  Changed;
end;

{************************************************************************************************************}
procedure TALNVStringListW.InsertItem(Index: Integer; const Name: String; WithNvS: boolean; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    ALMove(
      FList[Index],
      FList[Index + 1],
      (FCount - Index) * SizeOf(TALNVStringItemW));
  Pointer(FList[Index].FName) := nil;
  Pointer(FList[Index].FValue) := nil;
  Pointer(FList[Index].FObject) := nil;
  FList[Index].FObject := AObject;
  FList[Index].FName := Name;
  FList[Index].fNvs := WithNvS;
  FList[Index].FValue := '';
  Inc(FCount);
  Changed;
end;

{**************************************************************}
procedure TALNVStringListW.Put(Index: Integer; const S: String);
var Name, Value: String;
begin
  if not sorted then begin
    if Cardinal(Index) >= Cardinal(FCount) then
      IndexError(Index, FCount - 1);
    Changing;
    if ExtractNameValue(S, Name, Value) then begin
      FList[Index].FName := Name;
      FList[Index].FNvS := True;
      FList[Index].FValue := Value;
    end
    else begin
      FList[Index].FName := S;
      FList[Index].FNvS := False;
      FList[Index].FValue := '';
    end;
    Changed;
  end
  else begin
    delete(index);
    add(s);
  end;
end;

{*********************************************************************}
procedure TALNVStringListW.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  FList[Index].FObject := AObject;

  if Obj <> nil then
    ALFreeAndNil(Obj);

  Changed;
end;

{*****************************************************************************************}
procedure TALNVStringListW.QuickSort(L, R: Integer; ACompare: TALNVStringListSortCompareW);
var
  I, J, P: Integer;
begin
  while L < R do
  begin
    if (R - L) = 1 then
    begin
      if ACompare(Self, L, R) > 0 then
        ExchangeItems(L, R);
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
          ExchangeItems(I, J);
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

{***********************************************************}
procedure TALNVStringListW.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{***************************************************}
procedure TALNVStringListW.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

{***********************************************************}
procedure TALNVStringListW.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{***********************************************************************************************}
function ALNVStringListCompareStringsW(List: TALNVStringListW; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(
              List.FList[Index1].FName,
              List.FList[Index2].FName);  // The return value is less than 0 if List.FList[Index1].FName < List.FList[Index2].FName, 0 if List.FList[Index1].FName = List.FList[Index2].FName, or greater than 0 if List.FList[Index1].FName > List.FList[Index2].FName.
  if result = 0 then begin
    if (not List.FList[Index1].fNvS) and List.FList[Index2].FNVS then result := -1  // Must be ordered in this order :
                                                                                    // aa
                                                                                    // aaa
                                                                                    // aaa=
                                                                                    // aaaa
    else if (List.FList[Index1].fNvS) and (not List.FList[Index2].FNVS) then result := 1;  // Must be ordered in this order :
                                                                                           // aa
                                                                                           // aaa
                                                                                           // aaa=
                                                                                           // aaaa
    if (result=0) then Result := List.CompareStrings(
                                   List.FList[Index1].FValue,
                                   List.FList[Index2].FValue);  // The return value is less than 0 if List.FList[Index1].FValue < List.FList[Index2].FValue, 0 if List.FList[Index1].FValue = List.FList[Index2].FValue, or greater than 0 if List.FList[Index1].FValue > List.FList[Index2].FValue.
  end;
end;

{******************************}
procedure TALNVStringListW.Sort;
begin
  CustomSort(ALNVStringListCompareStringsW);
end;

{**************************************************************************}
procedure TALNVStringListW.CustomSort(Compare: TALNVStringListSortCompareW);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

{**********************************************************************}
function TALNVStringListW.CompareStrings(const S1, S2: String): Integer;
begin

  // Orignial Delphi Code
  // the difference between TALNVStringListA and TStringList is that
  // TstringList use ansiCompareStr or ansiCompareText that are
  // dependant from the local. I don't like this behavior because
  // as you can read
  // http://msdn.microsoft.com/en-us/library/windows/desktop/dd317759(v=vs.85).aspx
  // "Using CompareString incorrectly can compromise the security of your
  // application. Strings that are not compared correctly can produce
  // invalid input. For example, the function can raise security issues when
  // used for a non-linguistic comparison, because two strings that are
  // distinct in their binary representation can be linguistically equivalent"
  // so i prefere to use instead CompareStr and CompareText but only
  // a..z = A..Z will be handle when case insensitive is set.
  // other behavior must be handle in descendant classe
  //
  // also not the ansiCompareStr and ansiCompareText
  // are 10x more slower than CompareStr and CompareText
  //
  // if CaseSensitive then
  //   Result := AnsiCompareStr(S1, S2)
  // else
  //   Result := AnsiCompareText(S1, S2);

  // it's important that the order is not change because
  // of the ord(NameValueSeparator) this is need because of
  // function FindName
  //
  // EX the items
  //
  //   aaa0
  //   aaa=123
  //   aaaa
  //
  // must be ordered like
  //
  //   aaa=123   |     aaa
  //   aaa0      |     aaa0
  //   aaaa      |     aaaa
  //                   => OK, ordered work with findname
  //
  // but with just Result := ALCompareTextA(S1, S2)
  // it's will be ordered like
  //
  //   aaa0      |     aaa0
  //   aaa=123   |     aaa
  //   aaaa      |     aaaa
  //                   => KO, NOT ordered, break the findname
  //

  if CaseSensitive then
    Result := ALCompareStrW(S1, S2)
  else
    Result := ALCompareTextW(S1, S2);

end;

{****************************************************}
procedure TALNVStringListW.init(OwnsObjects: Boolean);
begin
  setlength(FList, 0);
  FCount := 0;
  FCapacity := 0;
  FSorted := False;
  FDuplicates := dupIgnore;
  FCaseSensitive := False;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := OwnsObjects;
end;

{**********************************}
constructor TALNVStringListW.Create;
begin
  inherited Create;
  init(False);
end;

{********************************************************}
constructor TALNVStringListW.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects);
end;

{****************************************************************}
procedure TALNVStringListW.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then
    begin
      // Calling Sort won't sort the list because CustomSort will
      // only sort the list if it's not already sorted
      Sorted := False;
      Sorted := True;
    end;
  end;
end;

{********************************************************************************************}
Function TALNVStringListW.ExtractNameValue(const S: String; var Name, Value: String): Boolean;
Var P1: Integer;
begin
  P1 := ALPosW(NameValueSeparator,S);
  if P1 > 0 then begin
    result := True;
    Name := ALCopyStr(S,1,P1-1);
    Value := ALCopyStr(S,P1+1, maxint);
  end
  else begin
    Result := False;
    Name := S;
    Value := '';
  end;
end;

{********************************************************}
function TALNVStringListW.GetName(Index: Integer): String;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  Result := Flist[Index].fName;
end;

{**************************************************************}
function TALNVStringListW.GetStrictName(Index: Integer): String;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    IndexError(Index, FCount - 1);
  if Flist[Index].fnvs then Result := Flist[Index].fName
  else result := ''
end;

{*************************************************************}
function TALNVStringListW.GetValue(const Name: String): String;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then begin
    if Flist[i].fnvs then Result := Flist[i].fValue
    else Result := '';
  end else
    Result := '';
end;

{*************************************************************}
procedure TALNVStringListW.SetValue(const Name, Value: String);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then AddNameValue(Name, Value)
    else begin
      Changing;
      Flist[i].fValue := Value;
      Flist[i].fNVS := True;
      Changed;
    end
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{******************************************************************}
function TALNVStringListW.GetValueFromIndex(Index: Integer): String;
begin
  if Index >= 0 then
  begin
    if Cardinal(Index) >= Cardinal(FCount) then
      IndexError(Index, FCount - 1);
    if (Flist[index].fNvs) then
      result := Flist[index].fValue
    else
      Result := '';
  end
  else
    Result := '';
end;

{********************************************************************************}
procedure TALNVStringListW.SetValueFromIndex(Index: Integer; const Value: String);
begin
  if Value <> '' then
  begin
    if Index < 0 then AddNameValue('', Value)
    else begin
      if Cardinal(Index) >= Cardinal(FCount) then
        IndexError(Index, FCount - 1);
      Changing;
      Flist[Index].fValue := Value;
      Flist[Index].fNVS := True;
      Changed;
    end;
  end
  else
    if Index >= 0 then Delete(Index);
end;

{***********************************************************************}
procedure TALNVStringListW.SetPersistentValue(const Name, Value: String);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then AddNameValue(Name, Value)
  else begin
    Changing;
    Flist[I].fValue := Value;
    Flist[I].fNVS := True;
    Changed;
  end
end;

{******************************************************************************************}
procedure TALNVStringListW.SetPersistentValueFromIndex(Index: Integer; const Value: String);
begin
  if Index < 0 then AddNameValue('', Value)
  else begin
    if Cardinal(Index) >= Cardinal(FCount) then
      IndexError(Index, FCount - 1);
    Changing;
    Flist[Index].fValue := Value;
    Flist[Index].fNVS := True;
    Changed;
  end;
end;

end.
