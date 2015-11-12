{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALStringList
Version:      4.01

Description:  TALStringList
              TALStringList Work the same as Delphi TstringList except that it's
              allow to search a name=value using a quicksort algorithm when the
              list is sorted. Also TALStringList use a locale independant
              algorithme (based on the 8-bit ordinal value of each character)
              instead of the AnsiCompareText and AnsiCompareStr used by the
              Delphi TstringList. at the end the sort in TALStringList is up to
              10x more faster than in Delphi TstringList. Also TALStringList is
              not an unicode TstringList but an 100% Ansi StringList

              TALNVStringList
              TALNVStringList (NV for NameValue) is same as TALStringList (use
              also a quicksort algorithme) except that here optimisation is
              oriented for name/value list instead of string list.

              TALAVLStringList
              TALAVLStringList is same as TALStringList except that it's use
              internally a self-balancing binary Tree instead of a quicksort
              algorithm

              TALHashedStringList
              TALHashedStringList is same as TALStringList except that it's use
              an internal hash table instead of a quicksort algorithm. By using
              TALHashedStringList instead of TALStringList, you can improve
              performance when the list contains a large number of strings
              (else if you list don't contain a lot of strings the performance
              is lower than TALStringList because of the cost to calculate the
              hash)

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

History :     27/10/2007: add ForceValues and ForceValueFromIndex
                          that do not delete an entry when we do
                          ForceValue[name] := ''
              26/01/2009: add TALAVLStringList
              15/06/2012: update TALStringList to be the 8 bit version
                          of TstringList. Replace ForceValues and
                          ForceValueFromIndex by PersistentValues and
                          PersistentValueFromIndex
              26/06/2012: Add xe2 support
              03/12/2012: Add StrictNames property

Link :

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALStringList;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

Uses {$IF CompilerVersion >= 23} {Delphi XE2}
     System.Classes,
     System.Contnrs,
     System.Generics.Defaults,
     System.Generics.Collections,
     System.Math,
     {$ELSE}
     Classes,
     Contnrs,
     Math,
     {$IFEND}
     ALQuickSortList,
     AlAvlBinaryTRee;

Type

  {$IF CompilerVersion < 18.5} {Delphi 2007}
  TStringsDefined = set of (sdDelimiter, sdQuoteChar, sdNameValueSeparator,
    sdLineBreak, sdStrictDelimiter);
  {$IFEND}

  {-----------------}
  TALStrings = class;

  {--------------------------}
  TALStringsEnumerator = class
  private
    FIndex: Integer;
    FStrings: TALStrings;
  public
    constructor Create(AStrings: TALStrings);
    function GetCurrent: AnsiString; {$IF CompilerVersion >= 17.0}inline;{$IFEND}
    function MoveNext: Boolean;
    property Current: AnsiString read GetCurrent;
  end;

  {-----------------------------}
  TALStrings = class(TPersistent)
  private
    //[deleted from Tstrings] FEncoding: TEncoding;
    //[deleted from Tstrings] FDefaultEncoding: TEncoding;
    //[deleted from Tstrings] FWriteBOM: Boolean;
    //[deleted from Tstrings] procedure SetDefaultEncoding(const Value: TEncoding);
    //[deleted from Tstrings] procedure SetStringsAdapter(const Value: IStringsAdapter);
    //[deleted from Tstrings] FAdapter: IStringsAdapter;
    //[deleted from Tstrings] procedure ReadData(Reader: TReader);
    //[deleted from Tstrings] procedure WriteData(Writer: TWriter);
    FDefined: TStringsDefined;
    FDelimiter: AnsiChar;
    FLineBreak: AnsiString;
    FQuoteChar: AnsiChar;
    FNameValueSeparator: AnsiChar;
    FStrictDelimiter: Boolean;
    FUpdateCount: Integer;
    function GetCommaText: AnsiString;
    function GetDelimitedText: AnsiString;
    procedure SetCommaText(const Value: AnsiString);
    procedure SetDelimitedText(const Value: AnsiString);
    function GetDelimiter: AnsiChar;
    procedure SetDelimiter(const Value: AnsiChar);
    function GetLineBreak: AnsiString;
    procedure SetLineBreak(const Value: AnsiString);
    function GetQuoteChar: AnsiChar;
    procedure SetQuoteChar(const Value: AnsiChar);
    function GetNameValueSeparator: AnsiChar;
    procedure SetNameValueSeparator(const Value: AnsiChar);
    function GetStrictDelimiter: Boolean;
    procedure SetStrictDelimiter(const Value: Boolean);
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
    procedure AddStrings(Strings: TALStrings); overload; virtual;
    {$IF CompilerVersion >= 23} {Delphi XE2}
    procedure AddStrings(const Strings: TArray<AnsiString>); overload;
    procedure AddStrings(const Strings: TArray<AnsiString>; const Objects: TArray<TObject>); overload;
    {$IFEND}
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TALStrings): Boolean; reintroduce;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetEnumerator: TALStringsEnumerator;
    function GetText: PAnsiChar; virtual;
    function IndexOf(const S: AnsiString): Integer; virtual;
    function IndexOfName(const Name: AnsiString): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: AnsiString); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: AnsiString; AObject: TObject); virtual;
    procedure InsertNameValue(Index: Integer; const Name, Value: AnsiString); virtual; // [added from Tstrings]
    procedure InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject); virtual; // [added from Tstrings]
    procedure LoadFromFile(const FileName: AnsiString); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: AnsiString); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(Text: PAnsiChar); virtual;
    {$IF CompilerVersion >= 23} {Delphi XE2}
    function ToStringArray: TArray<AnsiString>;
    function ToObjectArray: TArray<TObject>;
    {$IFEND}
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: AnsiString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Delimiter: AnsiChar read GetDelimiter write SetDelimiter;
    property DelimitedText: AnsiString read GetDelimitedText write SetDelimitedText;
    property LineBreak: AnsiString read GetLineBreak write SetLineBreak;
    property Names[Index: Integer]: AnsiString read GetName;
    property StrictNames[Index: Integer]: AnsiString read GetStrictName; // [added from Tstrings]
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: AnsiChar read GetQuoteChar write SetQuoteChar;
    property Values[const Name: AnsiString]: AnsiString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: AnsiString read GetValueFromIndex write SetValueFromIndex;
    property PersistentValues[const Name: AnsiString]: AnsiString read GetValue write SetPersistentValue; // [added from Tstrings]
    property PersistentValueFromIndex[Index: Integer]: AnsiString read GetValueFromIndex write SetPersistentValueFromIndex; // [added from Tstrings]
    property NameValueSeparator: AnsiChar read GetNameValueSeparator write SetNameValueSeparator;
    property StrictDelimiter: Boolean read GetStrictDelimiter write SetStrictDelimiter;
    property Strings[Index: Integer]: AnsiString read Get write Put; default;
    property Text: AnsiString read GetTextStr write SetTextStr;
  end;

  {--------------------}
  TALStringList = class;

  {-----------------------------}
  PALStringItem = ^TALStringItem;
  TALStringItem = record
    FString: AnsiString;
    FObject: TObject;
  end;

  {-----------------------------------------}
  TALStringItemList = array of TALStringItem;
  {$IF CompilerVersion >= 23} {Delphi XE2}
  TALStringListSortCompare = reference to function(List: TALStringList; Index1, Index2: Integer): Integer;
  {$ELSE}
  TALStringListSortCompare = function(List: TALStringList; Index1, Index2: Integer): Integer;
  {$IFEND}

  {-------------------------------}
  TALStringList = class(TALStrings)
  private
    FList: TALStringItemList;
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
    procedure QuickSort(L, R: Integer; SCompare: TALStringListSortCompare);
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
    function Find(const S: AnsiString; var Index: Integer): Boolean; virtual;
    function FindName(const S: AnsiString; var Index: Integer): Boolean; // [added from TStringList]
    function IndexOf(const S: AnsiString): Integer; override;
    function IndexOfName(const Name: AnsiString): Integer; override; // [added from TStringList]
    procedure Insert(Index: Integer; const S: AnsiString); override;
    procedure InsertObject(Index: Integer; const S: AnsiString; AObject: TObject); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TALStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    property NameValueOptimization: Boolean read FNameValueOptimization write FNameValueOptimization;
  end;

  {----------------------}
  TALNVStringList = class;

  {---------------------------------}
  PALNVStringItem = ^TALNVStringItem;
  TALNVStringItem = record
    FName: AnsiString;
    FValue: AnsiString;
    FNVS: Boolean;
    FObject: TObject;
  end;

  {---------------------------------------------}
  TALNVStringItemList = array of TALNVStringItem;
  {$IF CompilerVersion >= 23} {Delphi XE2}
  TALNVStringListSortCompare = reference to function(List: TALNVStringList; Index1, Index2: Integer): Integer;
  {$ELSE}
  TALNVStringListSortCompare = function(List: TALNVStringList; Index1, Index2: Integer): Integer;
  {$IFEND}

  {---------------------------------}
  TALNVStringList = class(TALStrings)
  private
    FList: TALNVStringItemList;
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
    procedure QuickSort(L, R: Integer; SCompare: TALNVStringListSortCompare);
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
    function Find(const S: AnsiString; var Index: Integer): Boolean; virtual;
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
    procedure CustomSort(Compare: TALNVStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

  {-----------------------}
  TALAVLStringList = class;
  TALAVLStringListSortCompare = function(List: TALAVLStringList; Index1, Index2: Integer): Integer;

  {-------------------------------------------------------------------}
  TALAVLStringListBinaryTreeNode = class(TALStringKeyAVLBinaryTreeNode)
  Private
  Protected
  Public
    Val: AnsiString;  // Value
    Obj: Tobject; // Object
    Idx: integer; // Index in the NodeList
    Nvs: Boolean; // if NameValueSeparator was present
  end;

  {----------------------------------}
  //when duplicate is set to ignore
  //it's take care only about the "name" part
  //and ignore the value part. exemple
  //if name1=value2 is in the list and
  //we add name1=value3 then if ignore duplicates
  //no error will be raise and the add command
  //will be silently ignored
  TALAVLStringList = class(TALStrings)
  private
    FNodeList: TObjectList;
    FAVLBinTree: TALStringKeyAVLBinaryTree;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure QuickSort(L, R: Integer; SCompare: TALAVLStringListSortCompare);
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
    procedure CustomSort(Compare: TALAVLStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write SetDuplicates;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

{$IF CompilerVersion >= 23} {Delphi XE2 - because we use TDictionary but i think TDictionary was 1rt introduced in D2009}

  {--------------------------}
  TALHashedStringList = class;
  TALHashedStringListSortCompare = function(List: TALHashedStringList; Index1, Index2: Integer): Integer;

  {-------------------------------------------------}
  TALHashedStringListDictionaryNode = class(Tobject)
  Private
  Protected
  Public
    ID: AnsiString; // Name
    Val: AnsiString;  // Value
    Obj: Tobject; // Object
    Idx: integer; // Index in the NodeList
    Nvs: Boolean; // if NameValueSeparator was present
  end;

  {----------------------------------}
  //when duplicate is set to ignore
  //it's take care only about the "name" part
  //and ignore the value part. exemple
  //if name1=value2 is in the list and
  //we add name1=value3 then if ignore duplicates
  //no error will be raise and the add command
  //will be silently ignored
  TALHashedStringList = class(TALStrings)
  private
    FNodeList: TObjectList<TALHashedStringListDictionaryNode>;
    FDictionary: TALObjectDictionary<ansiString, TALHashedStringListDictionaryNode>;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    FCaseSensitive: boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure QuickSort(L, R: Integer; SCompare: TALHashedStringListSortCompare);
    procedure SetCaseSensitive(const Value: Boolean);
    function GetCaseSensitive: Boolean;
    Function ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean;
    procedure SetDuplicates(const Value: TDuplicates);
    function CreateDictionary(ACapacity: integer; aCaseSensitive: boolean): TALObjectDictionary<ansiString, TALHashedStringListDictionaryNode>;
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
    procedure CustomSort(Compare: TALHashedStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write SetDuplicates;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

{$IFEND}

implementation

Uses {$IF CompilerVersion >= 23} {Delphi XE2}
     System.Sysutils,
     System.RTLConsts,
     {$IF CompilerVersion >= 24}{Delphi XE3}
     System.Ansistrings,
     {$IFEND}
     {$ELSE}
     sysutils,
     RTLConsts,
     {$IFEND}
     ALCipher,
     ALString;

{************************************************************}
constructor TALStringsEnumerator.Create(AStrings: TALStrings);
begin
  inherited Create;
  FIndex := -1;
  FStrings := AStrings;
end;

{***************************************************}
function TALStringsEnumerator.GetCurrent: AnsiString;
begin
  Result := FStrings[FIndex];
end;

{**********************************************}
function TALStringsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FStrings.Count - 1;
  if Result then
    Inc(FIndex);
end;

{****************************}
constructor TALStrings.Create;
begin
  inherited Create;
  FDefined := [];
  //FDelimiter := ',';          // doesn't matter what we set here because of fDefined
  //FLineBreak := sLineBreak;   // doesn't matter what we set here because of fDefined
  //FQuoteChar := '"';          // doesn't matter what we set here because of fDefined
  //FNameValueSeparator := '='; // doesn't matter what we set here because of fDefined
  //FStrictDelimiter:= False;   // doesn't matter what we set here because of fDefined
  FUpdateCount:= 0;
end;

{****************************************************}
function TALStrings.Add(const S: AnsiString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

{****************************************************************************}
function TALStrings.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

{***********************************************************************}
function TALStrings.AddNameValue(const Name, Value: AnsiString): Integer;
begin
  result := add(name + NameValueSeparator + Value);
end;

{***********************************************************************************************}
function TALStrings.AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer;
begin
  result := addObject(name + NameValueSeparator + Value, AObject);
end;

{***********************************************}
procedure TALStrings.Append(const S: AnsiString);
begin
  Add(S);
end;

{***************************************************}
procedure TALStrings.AddStrings(Strings: TALStrings);
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

{**************************************}
{$IF CompilerVersion >= 23} {Delphi XE2}
procedure TALStrings.AddStrings(const Strings: TArray<AnsiString>);
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
{$IFEND}

{**************************************}
{$IF CompilerVersion >= 23} {Delphi XE2}
procedure TALStrings.AddStrings(const Strings: TArray<AnsiString>; const Objects: TArray<TObject>);
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
{$IFEND}

{***********************************************}
procedure TALStrings.Assign(Source: TPersistent);
var i: integer;
begin
  if Source is TALStrings then
  begin
    BeginUpdate;
    try
      Clear;
      FDefined := TALStrings(Source).FDefined;
      FNameValueSeparator := TALStrings(Source).FNameValueSeparator;
      FQuoteChar := TALStrings(Source).FQuoteChar;
      FDelimiter := TALStrings(Source).FDelimiter;
      FLineBreak := TALStrings(Source).FLineBreak;
      FStrictDelimiter := TALStrings(Source).FStrictDelimiter;
      AddStrings(TALStrings(Source));
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
      FNameValueSeparator := AnsiChar(TStrings(Source).NameValueSeparator);
      FQuoteChar := AnsiChar(TStrings(Source).QuoteChar);
      FDelimiter := AnsiChar(TStrings(Source).Delimiter);
      {$IF CompilerVersion >= 18.5} {Delphi D2007}
      FLineBreak := AnsiString(TStrings(Source).LineBreak);
      FStrictDelimiter := TStrings(Source).StrictDelimiter;
      {$IFEND}
      for I := 0 to Tstrings(Source).Count - 1 do
        AddObject(Ansistring(Tstrings(Source)[I]), Tstrings(Source).Objects[I]);
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

{***********************************************}
procedure TALStrings.AssignTo(Dest: TPersistent);
var i: integer;
begin
  if Dest is TStrings then
  begin
    Tstrings(Dest).BeginUpdate;
    try
      Tstrings(Dest).Clear;
      Tstrings(Dest).NameValueSeparator := char(FNameValueSeparator);
      Tstrings(Dest).QuoteChar := char(FQuoteChar);
      Tstrings(Dest).Delimiter := char(FDelimiter);
      {$IF CompilerVersion >= 18.5} {Delphi D2007}
      Tstrings(Dest).LineBreak := String(FLineBreak);
      Tstrings(Dest).StrictDelimiter := FStrictDelimiter;
      {$IFEND}
      for I := 0 to Count - 1 do
        Tstrings(Dest).AddObject(string(get(I)), Objects[I]);
    finally
      Tstrings(Dest).EndUpdate;
    end;
    Exit;
  end;
  inherited AssignTo(Dest);
end;

{*******************************}
procedure TALStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

{*****************************}
procedure TALStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

{*******************************************************}
function TALStrings.Equals(Strings: TALStrings): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

{***********************************************************}
procedure TALStrings.Error(const Msg: String; Data: Integer);
begin
  raise EStringListError.CreateFmt(Msg, [Data]);
end;

{************************************************************}
procedure TALStrings.Error(Msg: PResStringRec; Data: Integer);
begin
  raise EStringListError.CreateFmt(LoadResString(Msg), [Data]);
end;

{*****************************************************}
procedure TALStrings.Exchange(Index1, Index2: Integer);
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

{***************************************************************}
function TALStrings.ExtractName(const S: AnsiString): AnsiString;
var
  P: Integer;
begin
  Result := S;
  P := ALPos(NameValueSeparator, Result);

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

{***************************************}
function TALStrings.GetCapacity: Integer;
begin  // descendents may optionally override/replace this default implementation
  Result := Count;
end;

{*******************************************}
function TALStrings.GetCommaText: AnsiString;
var
  LOldDefined: TStringsDefined;
  LOldDelimiter: AnsiChar;
  LOldQuoteChar: AnsiChar;
begin
  LOldDefined := FDefined;
  LOldDelimiter := FDelimiter;
  LOldQuoteChar := FQuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    Result := GetDelimitedText;
  finally
    FDelimiter := LOldDelimiter;
    FQuoteChar := LOldQuoteChar;
    FDefined := LOldDefined;
  end;
end;

{***********************************************}
function TALStrings.GetDelimitedText: AnsiString;
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

{******************************************************}
function TALStrings.GetEnumerator: TALStringsEnumerator;
begin
  Result := TALStringsEnumerator.Create(Self);
end;

{******************************************************}
function TALStrings.GetName(Index: Integer): AnsiString;
begin
  Result := ExtractName(Get(Index));
end;

{************************************************************}
function TALStrings.GetStrictName(Index: Integer): AnsiString;
var P: Integer;
begin
  Result := Get(Index);
  P := ALPos(NameValueSeparator, Result);
  if P <> 0 then SetLength(Result, P-1)
  else SetLength(Result, 0);
end;

{*****************************************************}
function TALStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

{*************************************}
function TALStrings.GetText: PAnsiChar;
begin
  Result := {$IF CompilerVersion >= 24}{Delphi XE3}System.Ansistrings.{$IFEND}StrNew(PAnsiChar(GetTextStr));
end;

{*****************************************}
function TALStrings.GetTextStr: AnsiString;
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

{***************************************************************}
function TALStrings.GetValue(const Name: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := ALCopyStr(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

{********************************************************}
function TALStrings.IndexOf(const S: AnsiString): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

{***************************************************************}
function TALStrings.IndexOfName(const Name: AnsiString): Integer;
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
  //   P := ALPos(NameValueSeparator, S);
  //   if (P <> 0) and (CompareStrings(ALCopyStr(S, 1, P - 1), Name) = 0) then Exit;
  // end;
  // Result := -1;

  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := ALPos(NameValueSeparator, S);
    if ((P <> 0) and (CompareStrings(ALCopyStr(S, 1, P - 1), Name) = 0)) or
       ((P = 0) and (CompareStrings(S, Name) = 0)) then Exit;
  end;
  Result := -1;
end;

{***********************************************************}
function TALStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{***************************************************************************************}
procedure TALStrings.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

{**********************************************************************************}
procedure TALStrings.InsertNameValue(Index: Integer; const Name, Value: AnsiString);
begin
  Insert(Index, name + NameValueSeparator + Value);
end;

{**********************************************************************************************************}
procedure TALStrings.InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
begin
  InsertObject(Index, name + NameValueSeparator + Value, AObject);
end;

{************************************************************}
procedure TALStrings.LoadFromFile(const FileName: AnsiString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(String(FileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

{***************************************************}
procedure TALStrings.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  S: AnsiString;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size);
    Stream.Read(Pointer(S)^, Size);
    SetTextStr(S);
  finally
    EndUpdate;
  end;
end;

{*****************************************************}
procedure TALStrings.Move(CurIndex, NewIndex: Integer);
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

{************************************************************}
procedure TALStrings.Put(Index: Integer; const S: AnsiString);
begin
end;

{***************************************************************}
procedure TALStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

{**********************************************************}
procedure TALStrings.SaveToFile(const FileName: AnsiString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(String(FileName), fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

{*************************************************}
procedure TALStrings.SaveToStream(Stream: TStream);
var
  S: AnsiString;
begin
  S := GetTextStr;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

{*****************************************************}
procedure TALStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendents may optionally implement this method
end;

{*********************************************************}
procedure TALStrings.SetCommaText(const Value: AnsiString);
var
  LOldDefined: TStringsDefined;
  LOldDelimiter: AnsiChar;
  LOldQuoteChar: AnsiChar;
begin
  LOldDefined := FDefined;
  LOldDelimiter := FDelimiter;
  LOldQuoteChar := FQuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    SetDelimitedText(Value);
  finally
    FDelimiter := LOldDelimiter;
    FQuoteChar := LOldQuoteChar;
    FDefined := LOldDefined;
  end;
end;

{********************************************}
procedure TALStrings.SetText(Text: PAnsiChar);
begin
  SetTextStr(Text);
end;

{*******************************************************}
procedure TALStrings.SetTextStr(const Value: AnsiString);
var
  P, Start, LB: PAnsiChar;
  S: AnsiString;
  LineBreakLen: Integer;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      if ALCompareStr(LineBreak, sLineBreak) = 0 then
      begin
        // This is a lot faster than using StrPos/AnsiStrPos when
        // LineBreak is the default (#13#10)
        while P^ <> #0 do
        begin
          Start := P;
          while not (P^ in [#0, #10, #13]) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P^ = #13 then Inc(P);
          if P^ = #10 then Inc(P);
        end;
      end
      else
      begin
        LineBreakLen := Length(LineBreak);
        while P^ <> #0 do
        begin
          Start := P;
          LB := {$IF CompilerVersion >= 24}{Delphi XE3}System.Ansistrings.{$IFEND}StrPos(P, PAnsiChar(LineBreak));
          while (P^ <> #0) and (P <> LB) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P = LB then
            Inc(P, LineBreakLen);
        end;
      end;
  finally
    EndUpdate;
  end;
end;

{*****************************************************}
procedure TALStrings.SetUpdateState(Updating: Boolean);
begin
end;

{***********************************************************}
procedure TALStrings.SetValue(const Name, Value: AnsiString);
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

{*********************************************************************}
procedure TALStrings.SetPersistentValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then Add(Name + NameValueSeparator + Value)
  else Put(I, Name + NameValueSeparator + Value);
end;

{*************************************************************}
procedure TALStrings.SetDelimitedText(const Value: AnsiString);
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
        while ((not FStrictDelimiter and (P^ > ' ')) or
              (FStrictDelimiter and (P^ <> #0))) and (P^ <> Delimiter) do
          Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);
      if not FStrictDelimiter then
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
        until not (not FStrictDelimiter and (P^ in [#1..' ']));
      end;
    end;
  finally
    EndUpdate;
  end;
end;

{*****************************************}
function TALStrings.GetDelimiter: AnsiChar;
begin
  if not (sdDelimiter in FDefined) then
    Delimiter := ',';
  Result := FDelimiter;
end;

{*******************************************}
function TALStrings.GetLineBreak: AnsiString;
begin
  if not (sdLineBreak in FDefined) then
    LineBreak := sLineBreak;
  Result := FLineBreak;
end;

{*****************************************}
function TALStrings.GetQuoteChar: AnsiChar;
begin
  if not (sdQuoteChar in FDefined) then
    QuoteChar := '"';
  Result := FQuoteChar;
end;

{**********************************************}
function TALStrings.GetStrictDelimiter: Boolean;
begin
  if not (sdStrictDelimiter in FDefined) then
    StrictDelimiter := False;
  Result := FStrictDelimiter;
end;

{*******************************************************}
procedure TALStrings.SetDelimiter(const Value: AnsiChar);
begin
  if (FDelimiter <> Value) or not (sdDelimiter in FDefined) then
  begin
    Include(FDefined, sdDelimiter);
    FDelimiter := Value;
  end
end;

{*********************************************************}
procedure TALStrings.SetLineBreak(const Value: AnsiString);
begin
  if (FLineBreak <> Value) or not (sdLineBreak in FDefined) then
  begin
    Include(FDefined, sdLineBreak);
    FLineBreak := Value;
  end
end;

{*******************************************************}
procedure TALStrings.SetQuoteChar(const Value: AnsiChar);
begin
  if (FQuoteChar <> Value) or not (sdQuoteChar in FDefined) then
  begin
    Include(FDefined, sdQuoteChar);
    FQuoteChar := Value;
  end
end;

{************************************************************}
procedure TALStrings.SetStrictDelimiter(const Value: Boolean);
begin
  if (FStrictDelimiter <> Value) or not (sdStrictDelimiter in FDefined) then
  begin
    Include(FDefined, sdStrictDelimiter);
    FStrictDelimiter := Value;
  end
end;

{********************************************************************}
function TALStrings.CompareStrings(const S1, S2: AnsiString): Integer;
begin
  Result := ALCompareText(S1, S2);
end;

{**************************************************}
function TALStrings.GetNameValueSeparator: AnsiChar;
begin
  if not (sdNameValueSeparator in FDefined) then
    NameValueSeparator := '=';
  Result := FNameValueSeparator;
end;

{****************************************************************}
procedure TALStrings.SetNameValueSeparator(const Value: AnsiChar);
begin
  if (FNameValueSeparator <> Value) or not (sdNameValueSeparator in FDefined) then
  begin
    Include(FDefined, sdNameValueSeparator);
    FNameValueSeparator := Value;
  end
end;

{****************************************************************}
function TALStrings.GetValueFromIndex(Index: Integer): AnsiString;
var
  SepPos: Integer;
begin
  if Index >= 0 then
  begin
    Result := Get(Index);
    SepPos := ALPos(NameValueSeparator, Result);
    if (SepPos > 0) then
      System.Delete(Result, 1, SepPos)
    else
      Result := '';
  end
  else
    Result := '';
end;

{******************************************************************************}
procedure TALStrings.SetValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Value <> '' then
  begin
    if Index < 0 then Add(NameValueSeparator + Value)
    else Put(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

{****************************************************************************************}
procedure TALStrings.SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Index < 0 then Add(NameValueSeparator + Value)
  else Put(Index, Names[Index] + NameValueSeparator + Value);
end;

{**************************************}
{$IF CompilerVersion >= 23} {Delphi XE2}
function TALStrings.ToStringArray: TArray<AnsiString>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Strings[I];
end;
{$IFEND}

{**************************************}
{$IF CompilerVersion >= 23} {Delphi XE2}
function TALStrings.ToObjectArray: TArray<TObject>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Objects[I];
end;
{$IFEND}

{*******************************}
destructor TALStringList.Destroy;
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
      Temp[I].Free;
end;

{*******************************************************}
function TALStringList.Add(const S: AnsiString): Integer;
begin
  Result := AddObject(S, nil);
end;

{*******************************************************************************}
function TALStringList.AddObject(const S: AnsiString; AObject: TObject): Integer;
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

{**************************************************}
procedure TALStringList.Assign(Source: TPersistent);
begin
  if Source is TALStringList then
  begin
    Clear;
    FCaseSensitive := TALStringList(Source).FCaseSensitive;
    FDuplicates := TALStringList(Source).FDuplicates;
    FSorted := TALStringList(Source).FSorted;
  end
  else if Source is TALNVStringList then
  begin
    Clear;
    FCaseSensitive := TALNVStringList(Source).FCaseSensitive;
    FDuplicates := TALNVStringList(Source).FDuplicates;
    FSorted := TALNVStringList(Source).FSorted;
  end
  {$IF CompilerVersion >= 23} {Delphi XE2}
  else if Source is TALHashedStringList then
  begin
    Clear;
    FCaseSensitive := TALHashedStringList(Source).CaseSensitive;
    FDuplicates := TALHashedStringList(Source).FDuplicates;
    FSorted := False;
  end
  {$ifend}
  else if Source is TALAVLStringList then
  begin
    Clear;
    FCaseSensitive := TALAVLStringList(Source).CaseSensitive;
    FDuplicates := TALAVLStringList(Source).FDuplicates;
    FSorted := False;
  end
  else if Source is TStringList then
  begin
    Clear;
    FCaseSensitive := TStringList(Source).CaseSensitive;
    FDuplicates := TStringList(Source).Duplicates;
    FSorted := TStringList(Source).Sorted;
  end;
  inherited Assign(Source);
end;

{**************************************************}
procedure TALStringList.AssignTo(Dest: TPersistent);
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

{******************************}
procedure TALStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{*******************************}
procedure TALStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{****************************}
procedure TALStringList.Clear;
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
        Temp[I].Free;

    Changed;
  end;
end;

{*********************************************}
procedure TALStringList.Delete(Index: Integer);
var
  Obj: TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
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
    System.Move(FList[Index + 1], FList[Index],
      (FCount - Index) * SizeOf(TALStringItem));
    // Make sure there is no danglng pointer in the last (now unused) element
    PPointer(@FList[FCount].FString)^ := nil;
    PPointer(@FList[FCount].FObject)^ := nil;
  end;
  if Obj <> nil then
    Obj.Free;
  Changed;
end;

{*************************************************************}
function  TALStringList.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  result := FList[Index].FObject;
  FList[Index].FObject := nil;
end;

{********************************************************}
procedure TALStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{*************************************************************}
procedure TALStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Pointer;
  Item1, Item2: PALStringItem;
begin
  Item1 := @FList[Index1];
  Item2 := @FList[Index2];
  Temp := Pointer(Item1^.FString);
  Pointer(Item1^.FString) := Pointer(Item2^.FString);
  Pointer(Item2^.FString) := Temp;
  Temp := Item1^.FObject;
  Item1^.FObject := Item2^.FObject;
  Item2^.FObject := Temp;
end;

{****************************************************************************}
function TALStringList.Find(const S: AnsiString; var Index: Integer): Boolean;
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

{********************************************************************************}
function TALStringList.FindName(const S: AnsiString; var Index: Integer): Boolean;
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

{*****************************************************}
function TALStringList.Get(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index].FString;
end;

{******************************************}
function TALStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

{***************************************}
function TALStringList.GetCount: Integer;
begin
  Result := FCount;
end;

{********************************************************}
function TALStringList.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index].FObject;
end;

{***************************}
procedure TALStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

{***********************************************************}
function TALStringList.IndexOf(const S: AnsiString): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

{******************************************************************}
function TALStringList.IndexOfName(const Name: ansistring): Integer;
begin
  if (not Sorted) or (not FNameValueOptimization) then Result := inherited IndexOfName(Name)
  else if not FindName(Name, Result) then Result := -1;
end;

{******************************************************************}
procedure TALStringList.Insert(Index: Integer; const S: AnsiString);
begin
  InsertObject(Index, S, nil);
end;

{******************************************************************************************}
procedure TALStringList.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

{********************************************************}
procedure TALStringList.Move(CurIndex, NewIndex: Integer);
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

{****************************************************************************************}
procedure TALStringList.InsertItem(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TALStringItem));
  with FList[Index] do
  begin
    Pointer(FString) := nil;
    FObject := AObject;
    FString := S;
  end;
  Inc(FCount);
  Changed;
end;

{***************************************************************}
procedure TALStringList.Put(Index: Integer; const S: AnsiString);
begin
  if not sorted then begin
    if Cardinal(Index) >= Cardinal(FCount) then
      Error(@SListIndexError, Index);
    Changing;
    FList[Index].FString := S;
    Changed;
  end
  else begin
    delete(index);
    add(s);
  end;
end;

{******************************************************************}
procedure TALStringList.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  FList[Index].FObject := AObject;

  if Obj <> nil then
    Obj.Free;

  Changed;
end;

{***********************************************************************************}
procedure TALStringList.QuickSort(L, R: Integer; SCompare: TALStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
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
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

{********************************************************}
procedure TALStringList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{************************************************}
procedure TALStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

{********************************************************}
procedure TALStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{*****************************************************************************************}
function ALStringListCompareStrings(List: TALStringList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList[Index1].FString,
                                List.FList[Index2].FString);
end;

{***************************}
procedure TALStringList.Sort;
begin
  CustomSort(ALStringListCompareStrings);
end;

{********************************************************************}
procedure TALStringList.CustomSort(Compare: TALStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

{***********************************************************************}
function TALStringList.CompareStrings(const S1, S2: AnsiString): Integer;

  {-------------------------------------------------------------}
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

  {--------------------------------------------------------------}
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
  // the difference between TALStringList and TStringList is that
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
  // but with just Result := ALCompareText(S1, S2)
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
      Result := AlCompareStr(S1, S2)
    else
      Result := AlCompareText(S1, S2);
  end;
end;

{*************************************************}
procedure TALStringList.init(OwnsObjects: Boolean);
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

{*******************************}
constructor TALStringList.Create;
begin
  inherited Create;
  init(False);
end;

{*****************************************************}
constructor TALStringList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects);
end;

{*************************************************************}
procedure TALStringList.SetCaseSensitive(const Value: Boolean);
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

{*********************************}
destructor TALNVStringList.Destroy;
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
      Temp[I].Free;
end;

{*********************************************************}
function TALNVStringList.Add(const S: AnsiString): Integer;
begin
  Result := AddObject(S, nil);
end;

{*********************************************************************************}
function TALNVStringList.AddObject(const S: AnsiString; AObject: TObject): Integer;
Var aName, aValue: AnsiString;
begin
  if not Sorted then begin
    Result := FCount;
    InsertItem(Result, S, AObject);
  end
  else begin
    if ExtractNameValue(S, aName, aValue) then begin
      if FindNameValue(aName, aValue, Result) then
        case Duplicates of
          dupIgnore: Exit;
          dupError: Error(@SDuplicateString, 0);
        end;
      InsertItem(Result, aName, aValue, AObject);
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

{****************************************************************************}
function TALNVStringList.AddNameValue(const Name, Value: AnsiString): Integer;
begin
  Result := AddNameValueObject(Name, Value, nil);
end;

{****************************************************************************************************}
function TALNVStringList.AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer;
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

{****************************************************}
procedure TALNVStringList.Assign(Source: TPersistent);
begin
  if Source is TALNVStringList then
  begin
    Clear;
    FCaseSensitive := TALNVStringList(Source).FCaseSensitive;
    FDuplicates := TALNVStringList(Source).FDuplicates;
    FSorted := TALNVStringList(Source).FSorted;
  end
  else if Source is TALStringList then
  begin
    Clear;
    FCaseSensitive := TALStringList(Source).FCaseSensitive;
    FDuplicates := TALStringList(Source).FDuplicates;
    FSorted := TALStringList(Source).FSorted;
  end
  {$IF CompilerVersion >= 23} {Delphi XE2}
  else if Source is TALHashedStringList then
  begin
    Clear;
    FCaseSensitive := TALHashedStringList(Source).CaseSensitive;
    FDuplicates := TALHashedStringList(Source).FDuplicates;
    FSorted := False;
  end
  {$ifend}
  else if Source is TALAVLStringList then
  begin
    Clear;
    FCaseSensitive := TALAVLStringList(Source).CaseSensitive;
    FDuplicates := TALAVLStringList(Source).FDuplicates;
    FSorted := False;
  end
  else if Source is TStringList then
  begin
    Clear;
    FCaseSensitive := TStringList(Source).CaseSensitive;
    FDuplicates := TStringList(Source).Duplicates;
    FSorted := TStringList(Source).Sorted;
  end;
  inherited Assign(Source);
end;

{****************************************************}
procedure TALNVStringList.AssignTo(Dest: TPersistent);
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

{********************************}
procedure TALNVStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{*********************************}
procedure TALNVStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{******************************}
procedure TALNVStringList.Clear;
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
        Temp[I].Free;

    Changed;
  end;
end;

{***********************************************}
procedure TALNVStringList.Delete(Index: Integer);
var
  Obj: TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
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
    System.Move(FList[Index + 1], FList[Index],
      (FCount - Index) * SizeOf(TALNVStringItem));
    // Make sure there is no danglng pointer in the last (now unused) element
    PPointer(@FList[FCount].FName)^   := nil;
    PPointer(@FList[FCount].FValue)^  := nil;
    PPointer(@FList[FCount].FObject)^ := nil;
  end;
  if Obj <> nil then
    Obj.Free;
  Changed;
end;

{***************************************************************}
function  TALNVStringList.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  result := FList[Index].FObject;
  FList[Index].FObject := nil;
end;

{**********************************************************}
procedure TALNVStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{***************************************************************}
procedure TALNVStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Pointer;
  Item1, Item2: PALNVStringItem;
begin
  Item1 := @FList[Index1];
  Item2 := @FList[Index2];

  Temp := Pointer(Item1^.FName);
  Pointer(Item1^.FName) := Pointer(Item2^.FName);
  Pointer(Item2^.FName) := Temp;

  Temp := Pointer(Item1^.FValue);
  Pointer(Item1^.FValue) := Pointer(Item2^.FValue);
  Pointer(Item2^.FValue) := Temp;

  Temp := Item1^.FObject;
  Item1^.FObject := Item2^.FObject;
  Item2^.FObject := Temp;
end;

{******************************************************************************}
function TALNVStringList.Find(const S: AnsiString; var Index: Integer): Boolean;
var Name, Value: ansiString;
begin
  if ExtractNameValue(S, Name, Value) then result := FindNameValue(Name, Value, Index)
  else result := FindName(Name, False{WithNvS}, Index);
end;

{*************************************************************************************}
function TALNVStringList.FindName(const Name: AnsiString; var Index: Integer): Boolean;
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

{*******************************************************************************************************}
function TALNVStringList.FindName(const Name: AnsiString; WithNvS: boolean; var Index: Integer): Boolean;
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

{*************************************************************************************************}
function TALNVStringList.FindNameValue(const Name, Value: AnsiString; var Index: Integer): Boolean;
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

{*******************************************************}
function TALNVStringList.Get(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  if FList[Index].fNvs then Result := FList[Index].FName + NameValueSeparator + FList[Index].fValue
  else Result := FList[Index].fname;
end;

{********************************************}
function TALNVStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

{*****************************************}
function TALNVStringList.GetCount: Integer;
begin
  Result := FCount;
end;

{**********************************************************}
function TALNVStringList.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index].FObject;
end;

{*****************************}
procedure TALNVStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

{**********************************************}
function TALNVStringList.GetTextStr: AnsiString;
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

{*************************************************************}
function TALNVStringList.IndexOf(const S: AnsiString): Integer;
Var aName, aValue: AnsiString;
begin
  if ExtractNameValue(S, aName, aValue) then begin
    if not Sorted then begin
      for Result := 0 to FCount - 1 do
        if Flist[Result].FNVS and
           (CompareStrings(Flist[Result].FName, aName) = 0) and
           (CompareStrings(Flist[Result].FValue, aValue) = 0) then Exit;
      Result := -1;
    end
    else begin
      if not FindNameValue(aName, aValue, Result) then Result := -1;
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

{********************************************************************}
function TALNVStringList.IndexOfName(const Name: ansistring): Integer;
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

{********************************************************************}
procedure TALNVStringList.Insert(Index: Integer; const S: AnsiString);
begin
  InsertObject(Index, S, nil);
end;

{********************************************************************************************}
procedure TALNVStringList.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

{***************************************************************************************}
procedure TALNVStringList.InsertNameValue(Index: Integer; const Name, Value: AnsiString);
begin
  InsertNameValueObject(Index, Name, Value, nil);
end;

{***************************************************************************************************************}
procedure TALNVStringList.InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, Name, Value, AObject);
end;

{**********************************************************}
procedure TALNVStringList.Move(CurIndex, NewIndex: Integer);
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

{******************************************************************************************}
procedure TALNVStringList.InsertItem(Index: Integer; const S: AnsiString; AObject: TObject);
var Name, Value: ansiString;
begin
  if ExtractNameValue(S, Name, Value) then InsertItem(Index, Name, Value, AObject)
  else InsertItem(Index, s, False{WithNvS}, AObject);
end;

{****************************************************************************************************}
procedure TALNVStringList.InsertItem(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TALNVStringItem));
  with FList[Index] do
  begin
    Pointer(FName) := nil;
    Pointer(FValue) := nil;
    FObject := AObject;
    FName := Name;
    fNvs := True;
    FValue := Value;
  end;
  Inc(FCount);
  Changed;
end;

{***************************************************************************************************************}
procedure TALNVStringList.InsertItem(Index: Integer; const Name: AnsiString; WithNvS: boolean; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TALNVStringItem));
  with FList[Index] do
  begin
    Pointer(FName) := nil;
    Pointer(FValue) := nil;
    FObject := AObject;
    FName := Name;
    fNvs := WithNvS;
    FValue := '';
  end;
  Inc(FCount);
  Changed;
end;

{*****************************************************************}
procedure TALNVStringList.Put(Index: Integer; const S: AnsiString);
var Name, Value: ansiString;
begin
  if not sorted then begin
    if Cardinal(Index) >= Cardinal(FCount) then
      Error(@SListIndexError, Index);
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

{********************************************************************}
procedure TALNVStringList.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  FList[Index].FObject := AObject;

  if Obj <> nil then
    Obj.Free;

  Changed;
end;

{***************************************************************************************}
procedure TALNVStringList.QuickSort(L, R: Integer; SCompare: TALNVStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
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
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

{**********************************************************}
procedure TALNVStringList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{**************************************************}
procedure TALNVStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

{**********************************************************}
procedure TALNVStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{*********************************************************************************************}
function ALNVStringListCompareStrings(List: TALNVStringList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList[Index1].FName,
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
    if (result=0) then Result := List.CompareStrings(List.FList[Index1].FValue,
                                                     List.FList[Index2].FValue);  // The return value is less than 0 if List.FList[Index1].FValue < List.FList[Index2].FValue, 0 if List.FList[Index1].FValue = List.FList[Index2].FValue, or greater than 0 if List.FList[Index1].FValue > List.FList[Index2].FValue.
  end;
end;

{*****************************}
procedure TALNVStringList.Sort;
begin
  CustomSort(ALNVStringListCompareStrings);
end;

{************************************************************************}
procedure TALNVStringList.CustomSort(Compare: TALNVStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

{*************************************************************************}
function TALNVStringList.CompareStrings(const S1, S2: AnsiString): Integer;
begin

  // Orignial Delphi Code
  // the difference between TALNVStringList and TStringList is that
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
  // but with just Result := ALCompareText(S1, S2)
  // it's will be ordered like
  //
  //   aaa0      |     aaa0
  //   aaa=123   |     aaa
  //   aaaa      |     aaaa
  //                   => KO, NOT ordered, break the findname
  //

  if CaseSensitive then
    Result := ALCompareStr(S1, S2)
  else
    Result := ALCompareText(S1, S2);

end;

{***************************************************}
procedure TALNVStringList.init(OwnsObjects: Boolean);
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

{*********************************}
constructor TALNVStringList.Create;
begin
  inherited Create;
  init(False);
end;

{*******************************************************}
constructor TALNVStringList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects);
end;

{***************************************************************}
procedure TALNVStringList.SetCaseSensitive(const Value: Boolean);
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

{***************************************************************************************************}
Function TALNVStringList.ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean;
Var P1: Integer;
begin
  P1 := AlPos(NameValueSeparator,S);
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

{***********************************************************}
function TALNVStringList.GetName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Result := Flist[Index].fName;
end;

{*****************************************************************}
function TALNVStringList.GetStrictName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  if Flist[Index].fnvs then Result := Flist[Index].fName
  else result := ''
end;

{********************************************************************}
function TALNVStringList.GetValue(const Name: AnsiString): AnsiString;
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

{****************************************************************}
procedure TALNVStringList.SetValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then AddNameValue(Name, Value)
    else begin
      Flist[i].fValue := Value;
      Flist[i].fNVS := True;
    end
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{*********************************************************************}
function TALNVStringList.GetValueFromIndex(Index: Integer): AnsiString;
begin
  if Index >= 0 then
  begin
    if Cardinal(Index) >= Cardinal(Count) then
      Error(@SListIndexError, Index);
    if (Flist[index].fNvs) then
      result := Flist[index].fValue
    else
      Result := '';
  end
  else
    Result := '';
end;

{***********************************************************************************}
procedure TALNVStringList.SetValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Value <> '' then
  begin
    if Index < 0 then AddNameValue('', Value)
    else begin
      if Cardinal(Index) >= Cardinal(Count) then
        Error(@SListIndexError, Index);
      Flist[Index].fValue := Value;
      Flist[Index].fNVS := True;
    end;
  end
  else
    if Index >= 0 then Delete(Index);
end;

{**************************************************************************}
procedure TALNVStringList.SetPersistentValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then AddNameValue(Name, Value)
  else begin
    Flist[I].fValue := Value;
    Flist[I].fNVS := True;
  end
end;

{*********************************************************************************************}
procedure TALNVStringList.SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Index < 0 then AddNameValue('', Value)
  else begin
    if Cardinal(Index) >= Cardinal(Count) then
      Error(@SListIndexError, Index);
    Flist[Index].fValue := Value;
    Flist[Index].fNVS := True;
  end;
end;

{**********************************}
destructor TALAVLStringList.Destroy;
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

  FAVLBinTree.free;
  FNodeList.free;
  inherited Destroy;

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      Temp[I].Free;
end;

{**********************************************************}
function TALAVLStringList.Add(const S: AnsiString): Integer;
begin
  Result := AddObject(S, nil);
end;

{**********************************************************************************}
function TALAVLStringList.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  Result := Count;
  InsertItem(Result, S, AObject);
end;

{*****************************************************************************}
function TALAVLStringList.AddNameValue(const Name, Value: AnsiString): Integer;
begin
  Result := AddNameValueObject(Name, Value, nil);
end;

{*****************************************************************************************************}
function TALAVLStringList.AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer;
begin
  Result := Count;
  InsertItem(Result, Name, Value, AObject);
end;

{*****************************************************}
procedure TALAVLStringList.Assign(Source: TPersistent);
begin
  if Source is TALAVLStringList then
  begin
    Clear;
    CaseSensitive := TALAVLStringList(Source).CaseSensitive;
    FDuplicates := TALAVLStringList(Source).FDuplicates;
  end
  else if Source is TALStringList then
  begin
    Clear;
    CaseSensitive := TALStringList(Source).CaseSensitive;
    FDuplicates := TALStringList(Source).Duplicates;
  end
  else if Source is TALNVStringList then
  begin
    Clear;
    CaseSensitive := TALNVStringList(Source).FCaseSensitive;
    FDuplicates := TALNVStringList(Source).FDuplicates;
  end
  {$IF CompilerVersion >= 23} {Delphi XE2}
  else if Source is TALHashedStringList then
  begin
    Clear;
    CaseSensitive := TALHashedStringList(Source).CaseSensitive;
    FDuplicates := TALHashedStringList(Source).FDuplicates;
  end
  {$ifend}
  else if Source is TStringList then
  begin
    Clear;
    CaseSensitive := TStringList(Source).CaseSensitive;
    FDuplicates := TStringList(Source).Duplicates;
  end;
  inherited Assign(Source);
end;

{*****************************************************}
procedure TALAVLStringList.AssignTo(Dest: TPersistent);
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

{*********************************}
procedure TALAVLStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{**********************************}
procedure TALAVLStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{*******************************}
procedure TALAVLStringList.Clear;
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
        Temp[I].Free;

    Changed;
  end;
end;

{************************************************}
procedure TALAVLStringList.Delete(Index: Integer);
var
  Obj: TObject;
  i: integer;
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := Objects[Index]
  else
    Obj := nil;

  FAVLBinTree.DeleteNode(TALAVLStringListBinaryTreeNode(FNodelist[Index]).ID);
  FNodelist.Delete(Index);
  for i := Index to FNodeList.Count - 1 do
    TALAVLStringListBinaryTreeNode(FNodelist[i]).Idx := i;

  if Obj <> nil then
    Obj.Free;
  Changed;
end;

{****************************************************************}
function  TALAVLStringList.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  with TALAVLStringListBinaryTreeNode(FNodeList[Index]) do begin
    result := Obj;
    Obj := nil;
  end;
end;

{***********************************************************}
procedure TALAVLStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= Count) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= Count) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{****************************************************************}
procedure TALAVLStringList.ExchangeItems(Index1, Index2: Integer);
var Item1, Item2: TALAVLStringListBinaryTreeNode;
begin
  Item1 := TALAVLStringListBinaryTreeNode(FNodelist[Index1]);
  Item2 := TALAVLStringListBinaryTreeNode(FNodelist[Index2]);
  FNodeList.Exchange(Index1,Index2);
  Item1.idx := Index2;
  Item2.idx := Index1;
end;

{********************************************************}
function TALAVLStringList.Get(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  with TALAVLStringListBinaryTreeNode(FNodelist[Index]) do begin
    if Nvs then Result := ID + NameValueSeparator + Val
    else Result := ID;
  end;
end;

{******************************************}
function TALAVLStringList.GetCount: Integer;
begin
  Result := FNodeList.Count;
end;

{***********************************************************}
function TALAVLStringList.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Result := TALAVLStringListBinaryTreeNode(FNodelist[Index]).Obj;
end;

{***********************************************}
function TALAVLStringList.GetTextStr: AnsiString;
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
    if TALAVLStringListBinaryTreeNode(fNodeList[i]).Nvs then Inc(Size, Length(TALAVLStringListBinaryTreeNode(fNodeList[i]).ID) + 1{length(NameValueSeparator)} +  Length(TALAVLStringListBinaryTreeNode(fNodeList[i]).Val) + Length(LB))
    else Inc(Size, Length(TALAVLStringListBinaryTreeNode(fNodeList[i]).ID) + Length(LB))
  end;
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := TALAVLStringListBinaryTreeNode(fNodeList[i]).ID;
    L := Length(S);
    if L <> 0 then
    begin
      ALMove(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    if TALAVLStringListBinaryTreeNode(fNodeList[i]).Nvs then begin
      ALMove(NvS, P^, 1);
      Inc(P, 1);
      S := TALAVLStringListBinaryTreeNode(fNodeList[i]).Val;
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
function TALAVLStringList.IndexOf(const S: AnsiString): Integer;
Var aName, aValue: AnsiString;
    aNode: TALAVLStringListBinaryTreeNode;
begin
  if ExtractNameValue(S, aName, aValue) then begin
    aNode := TALAVLStringListBinaryTreeNode(FAVLBinTree.FindNode(aName));
    if (not assigned(aNode))
       or
       ((CaseSensitive) and
        (aNode.Val <> aValue))
       or
       ((not CaseSensitive) and
        (not ALSametext(aNode.Val, aValue)))
    then result := -1
    else result := aNode.idx;
  end
  else begin
    aNode := TALAVLStringListBinaryTreeNode(FAVLBinTree.FindNode(S));
    if (not assigned(aNode)) or (aNode.Nvs) then result := -1
    else result := aNode.idx;
  end;
end;

{*********************************************************************}
function TALAVLStringList.IndexOfName(const Name: ansistring): Integer;
Var aNode: TALAVLStringListBinaryTreeNode;
begin
  aNode := TALAVLStringListBinaryTreeNode(FAVLBinTree.FindNode(Name));
  if assigned(aNode) then result := aNode.Idx
  else result := -1;
end;

{*********************************************************************}
procedure TALAVLStringList.Insert(Index: Integer; const S: AnsiString);
begin
  InsertObject(Index, S, nil);
end;

{*********************************************************************************************}
procedure TALAVLStringList.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  if (Index < 0) or (Index > Count) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

{****************************************************************************************}
procedure TALAVLStringList.InsertNameValue(Index: Integer; const Name, Value: AnsiString);
begin
  InsertNameValueObject(Index, Name, Value, nil);
end;

{****************************************************************************************************************}
procedure TALAVLStringList.InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
begin
  if (Index < 0) or (Index > Count) then Error(@SListIndexError, Index);
  InsertItem(Index, Name, Value, AObject);
end;

{***********************************************************}
procedure TALAVLStringList.Move(CurIndex, NewIndex: Integer);
var i: integer;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      FNodeList.Move(CurIndex, NewIndex);
      for i := min(CurIndex, NewIndex) to FNodeList.Count - 1 do
        TALAVLStringListBinaryTreeNode(FNodelist[i]).Idx := i;
    finally
      EndUpdate;
    end;
  end;
end;

{********************************************************************************************************}
procedure TALAVLStringList.InsertItem(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
Var aNode: TALAVLStringListBinaryTreeNode;
    i: integer;
begin
  Changing;

  aNode := TALAVLStringListBinaryTreeNode.Create;
  aNode.Idx := Index;
  aNode.ID := Name;
  aNode.Val := Value;
  aNode.Nvs := True;
  aNode.Obj := AObject;
  if not FAVLBinTree.AddNode(aNode) then begin
    aNode.free;
    case Duplicates of
      dupIgnore: Exit;
      else Error(@SDuplicateString, 0);
    end;
  end;
  FNodeList.Insert(Index, aNode);
  for i := Index + 1 to FNodeList.Count - 1 do
    TALAVLStringListBinaryTreeNode(FNodelist[i]).Idx := i;

  Changed;
end;

{*******************************************************************************************}
procedure TALAVLStringList.InsertItem(Index: Integer; const S: AnsiString; AObject: TObject);
Var aName, AValue: AnsiString;
    aNvs: Boolean;
    aNode: TALAVLStringListBinaryTreeNode;
    i: integer;
begin
  Changing;

  aNvs := ExtractNameValue(S, aName, aValue);
  aNode := TALAVLStringListBinaryTreeNode.Create;
  aNode.Idx := Index;
  aNode.ID := aName;
  aNode.Val := aValue;
  aNode.Nvs := aNvs;
  aNode.Obj := AObject;
  if not FAVLBinTree.AddNode(aNode) then begin
    aNode.free;
    case Duplicates of
      dupIgnore: Exit;
      else Error(@SDuplicateString, 0);
    end;
  end;
  FNodeList.Insert(Index, aNode);
  for i := Index + 1 to FNodeList.Count - 1 do
    TALAVLStringListBinaryTreeNode(FNodelist[i]).Idx := i;

  Changed;
end;

{******************************************************************}
procedure TALAVLStringList.Put(Index: Integer; const S: AnsiString);
Var aNewName, aNewValue: AnsiString;
    aNewNvs: Boolean;
    aNewNode, aOldNode: TALAVLStringListBinaryTreeNode;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Changing;

  aNewNvs := ExtractNameValue(S, aNewName, aNewValue);
  aOldNode := TALAVLStringListBinaryTreeNode(FNodeList[index]);
  if (CaseSensitive and (aOldNode.ID <> aNewName)) or
     ((not CaseSensitive) and (not ALSametext(aOldNode.ID, aNewName))) then begin
    aNewNode := TALAVLStringListBinaryTreeNode.Create;
    aNewNode.Idx := Index;
    aNewNode.ID := aNewName;
    aNewNode.Val := ANewValue;
    aNewNode.NVS := aNewNvs;
    aNewNode.Obj := aOldNode.Obj;
    if not FAVLBinTree.AddNode(aNewNode) then begin
      aNewNode.free;
      case Duplicates of
        dupIgnore: Exit;
        else Error(@SDuplicateString, 0);
      end;
    end;
    FNodeList[Index] := aNewNode;
    FAVLBinTree.DeleteNode(aOldNode.ID);
  end
  else begin
    aOldNode.Val := aNewValue;
    aOldNode.NVS := aNewNVS;
  end;

  Changed;
end;

{*********************************************************************}
procedure TALAVLStringList.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := TALAVLStringListBinaryTreeNode(FNodeList[Index]).Obj
  else
    Obj := nil;

  TALAVLStringListBinaryTreeNode(FNodeList[Index]).Obj := AObject;

  if Obj <> nil then
    Obj.Free;

  Changed;
end;

{*****************************************************************************************}
procedure TALAVLStringList.QuickSort(L, R: Integer; SCompare: TALAVLStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
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
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

{***********************************************************}
procedure TALAVLStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{**************************************************************************}
procedure TALAVLStringList.CustomSort(Compare: TALAVLStringListSortCompare);
begin
  if (Count > 1) then
  begin
    Changing;
    QuickSort(0, Count - 1, Compare);
    Changed;
  end;
end;

{****************************************************}
procedure TALAVLStringList.init(OwnsObjects: Boolean);
begin
  FAVLBinTree:= TALStringKeyAVLBinaryTree.Create;
  FAVLBinTree.CaseSensitive := False;
  FNodeList := TObjectList.Create(False);
  FDuplicates := dupError;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := OwnsObjects;
end;

{**********************************}
constructor TALAVLStringList.Create;
begin
  inherited Create;
  init(False);
end;

{********************************************************}
constructor TALAVLStringList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects);
end;

{****************************************************************}
procedure TALAVLStringList.SetCaseSensitive(const Value: Boolean);
begin
  FAVLBinTree.CaseSensitive := Value;
end;

{**************************************************}
function TALAVLStringList.GetCaseSensitive: Boolean;
begin
  result := FAVLBinTree.CaseSensitive;
end;

{*****************************************************************}
procedure TALAVLStringList.SetDuplicates(const Value: TDuplicates);
begin
  if value = dupAccept then raise exception.Create('TALAVLStringList does not support duplicate Names');
  FDuplicates := Value;
end;

{****************************************************************************************************}
Function TALAVLStringList.ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean;
Var P1: Integer;
begin
  P1 := AlPos(NameValueSeparator,S);
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
function TALAVLStringList.GetName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Result := TALAVLStringListBinaryTreeNode(FNodelist[Index]).ID;
end;

{******************************************************************}
function TALAVLStringList.GetStrictName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  if TALAVLStringListBinaryTreeNode(FNodelist[Index]).nvs then Result := TALAVLStringListBinaryTreeNode(FNodelist[Index]).ID
  else result := ''
end;

{*********************************************************************}
function TALAVLStringList.GetValue(const Name: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then begin
    if TALAVLStringListBinaryTreeNode(FNodelist[i]).nvs then Result := TALAVLStringListBinaryTreeNode(FNodelist[i]).Val
    else Result := '';
  end else
    Result := '';
end;

{*****************************************************************}
procedure TALAVLStringList.SetValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then AddNameValue(Name, Value)
    else begin
      TALAVLStringListBinaryTreeNode(FNodeList[i]).Val := Value;
      TALAVLStringListBinaryTreeNode(FNodeList[i]).NVS := True;
    end
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{**********************************************************************}
function TALAVLStringList.GetValueFromIndex(Index: Integer): AnsiString;
begin
  if Index >= 0 then
  begin
    if Cardinal(Index) >= Cardinal(Count) then
      Error(@SListIndexError, Index);
    if (TALAVLStringListBinaryTreeNode(FNodeList[index]).Nvs) then
      result := TALAVLStringListBinaryTreeNode(FNodeList[index]).Val
    else
      Result := '';
  end
  else
    Result := '';
end;

{************************************************************************************}
procedure TALAVLStringList.SetValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Value <> '' then
  begin
    if Index < 0 then AddNameValue('', Value)
    else begin
      if Cardinal(Index) >= Cardinal(Count) then
        Error(@SListIndexError, Index);
      TALAVLStringListBinaryTreeNode(FNodeList[Index]).Val := Value;
      TALAVLStringListBinaryTreeNode(FNodeList[Index]).NVS := True;
    end;
  end
  else
    if Index >= 0 then Delete(Index);
end;

{***************************************************************************}
procedure TALAVLStringList.SetPersistentValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then AddNameValue(Name, Value)
  else begin
    TALAVLStringListBinaryTreeNode(FNodeList[I]).Val := Value;
    TALAVLStringListBinaryTreeNode(FNodeList[I]).NVS := True;
  end
end;

{**********************************************************************************************}
procedure TALAVLStringList.SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Index < 0 then AddNameValue('', Value)
  else begin
    if Cardinal(Index) >= Cardinal(Count) then
      Error(@SListIndexError, Index);
    TALAVLStringListBinaryTreeNode(FNodeList[Index]).Val := Value;
    TALAVLStringListBinaryTreeNode(FNodeList[Index]).NVS := True;
  end;
end;

{$IF CompilerVersion >= 23} {Delphi XE2 - because we use TDictionary but i think TDictionary was 1rt introduced in D2009}

{*************************************}
destructor TALHashedStringList.Destroy;
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

  FDictionary.free;
  FNodeList.free;
  inherited Destroy;

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      Temp[I].Free;
end;

{*************************************************************}
function TALHashedStringList.Add(const S: AnsiString): Integer;
begin
  Result := AddObject(S, nil);
end;

{*************************************************************************************}
function TALHashedStringList.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  Result := Count;
  InsertItem(Result, S, AObject);
end;

{********************************************************************************}
function TALHashedStringList.AddNameValue(const Name, Value: AnsiString): Integer;
begin
  Result := AddNameValueObject(Name, Value, nil);
end;

{********************************************************************************************************}
function TALHashedStringList.AddNameValueObject(const Name, Value: AnsiString; AObject: TObject): Integer;
begin
  Result := Count;
  InsertItem(Result, Name, Value, AObject);
end;

{********************************************************}
procedure TALHashedStringList.Assign(Source: TPersistent);
begin
  if Source is TALHashedStringList then
  begin
    Clear;
    CaseSensitive := TALHashedStringList(Source).CaseSensitive;
    FDuplicates := TALHashedStringList(Source).FDuplicates;
  end
  else if Source is TALStringList then
  begin
    Clear;
    CaseSensitive := TALStringList(Source).CaseSensitive;
    FDuplicates := TALStringList(Source).Duplicates;
  end
  else if Source is TALNVStringList then
  begin
    Clear;
    FCaseSensitive := TALNVStringList(Source).FCaseSensitive;
    FDuplicates := TALNVStringList(Source).FDuplicates;
  end
  else if Source is TALAvlStringList then
  begin
    Clear;
    CaseSensitive := TALAvlStringList(Source).CaseSensitive;
    FDuplicates := TALAvlStringList(Source).FDuplicates;
  end
  else if Source is TStringList then
  begin
    Clear;
    CaseSensitive := TStringList(Source).CaseSensitive;
    FDuplicates := TStringList(Source).Duplicates;
  end;
  inherited Assign(Source);
end;

{********************************************************}
procedure TALHashedStringList.AssignTo(Dest: TPersistent);
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

{************************************}
procedure TALHashedStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{*************************************}
procedure TALHashedStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{**********************************}
procedure TALHashedStringList.Clear;
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
        Temp[I].Free;

    Changed;
  end;
end;

{***************************************************}
procedure TALHashedStringList.Delete(Index: Integer);
var
  Obj: TObject;
  i: integer;
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
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
    Obj.Free;
  Changed;
end;

{*******************************************************************}
function  TALHashedStringList.ExtractObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  with FNodeList[Index] do begin
    result := Obj;
    Obj := nil;
  end;
end;

{**************************************************************}
procedure TALHashedStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= Count) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= Count) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

{*******************************************************************}
procedure TALHashedStringList.ExchangeItems(Index1, Index2: Integer);
var Item1, Item2: TALHashedStringListDictionaryNode;
begin
  Item1 := FNodelist[Index1];
  Item2 := FNodelist[Index2];
  FNodeList.Exchange(Index1,Index2);
  Item1.idx := Index2;
  Item2.idx := Index1;
end;

{***********************************************************}
function TALHashedStringList.Get(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  with FNodelist[Index] do begin
    if Nvs then Result := ID + NameValueSeparator + Val
    else Result := ID;
  end;
end;

{*********************************************}
function TALHashedStringList.GetCount: Integer;
begin
  Result := FNodeList.Count;
end;

{**************************************************************}
function TALHashedStringList.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Result := FNodelist[Index].Obj;
end;

{**************************************************}
function TALHashedStringList.GetTextStr: AnsiString;
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

{*****************************************************************}
function TALHashedStringList.IndexOf(const S: AnsiString): Integer;
Var aName, aValue: AnsiString;
    aNode: TALHashedStringListDictionaryNode;
begin
  if ExtractNameValue(S, aName, aValue) then begin
    if (not fDictionary.TryGetValue(aName,aNode))
       or
       ((CaseSensitive) and
        (aNode.Val <> aValue))
       or
       ((not CaseSensitive) and
        (not ALSametext(aNode.Val, aValue)))
    then result := -1
    else result := aNode.idx;
  end
  else begin
    if (not fDictionary.TryGetValue(S,aNode)) or (aNode.Nvs) then result := -1
    else result := aNode.idx;
  end;
end;

{************************************************************************}
function TALHashedStringList.IndexOfName(const Name: ansistring): Integer;
Var aNode: TALHashedStringListDictionaryNode;
begin
  if fDictionary.TryGetValue(Name,aNode) then result := aNode.Idx
  else result := -1;
end;

{************************************************************************}
procedure TALHashedStringList.Insert(Index: Integer; const S: AnsiString);
begin
  InsertObject(Index, S, nil);
end;

{************************************************************************************************}
procedure TALHashedStringList.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  if (Index < 0) or (Index > Count) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

{*******************************************************************************************}
procedure TALHashedStringList.InsertNameValue(Index: Integer; const Name, Value: AnsiString);
begin
  InsertNameValueObject(Index, Name, Value, nil);
end;

{*******************************************************************************************************************}
procedure TALHashedStringList.InsertNameValueObject(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
begin
  if (Index < 0) or (Index > Count) then Error(@SListIndexError, Index);
  InsertItem(Index, Name, Value, AObject);
end;

{**************************************************************}
procedure TALHashedStringList.Move(CurIndex, NewIndex: Integer);
var i: integer;
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

{********************************************************************************************************}
procedure TALHashedStringList.InsertItem(Index: Integer; const Name, Value: AnsiString; AObject: TObject);
Var aNode: TALHashedStringListDictionaryNode;
    i: integer;
begin
  Changing;

  aNode := TALHashedStringListDictionaryNode.Create;
  aNode.Idx := Index;
  aNode.ID := Name;
  aNode.Val := Value;
  aNode.Nvs := True;
  aNode.Obj := AObject;
  if not Fdictionary.TryAdd(Name,aNode) then begin
    aNode.Free;
    case Duplicates of
      dupIgnore: Exit;
      else Error(@SDuplicateString, 0);
    end;
  end;
  FNodeList.Insert(Index, aNode);
  for i := Index + 1 to FNodeList.Count - 1 do
    FNodelist[i].Idx := i;

  Changed;
end;

{**********************************************************************************************}
procedure TALHashedStringList.InsertItem(Index: Integer; const S: AnsiString; AObject: TObject);
Var aName, AValue: AnsiString;
    aNvs: Boolean;
    aNode: TALHashedStringListDictionaryNode;
    i: integer;
begin
  Changing;

  aNvs := ExtractNameValue(S, aName, aValue);
  aNode := TALHashedStringListDictionaryNode.Create;
  aNode.Idx := Index;
  aNode.ID := aName;
  aNode.Val := aValue;
  aNode.Nvs := aNvs;
  aNode.Obj := AObject;
  if not Fdictionary.TryAdd(aName,aNode) then begin
    aNode.Free;
    case Duplicates of
      dupIgnore: Exit;
      else Error(@SDuplicateString, 0);
    end;
  end;
  FNodeList.Insert(Index, aNode);
  for i := Index + 1 to FNodeList.Count - 1 do
    FNodelist[i].Idx := i;

  Changed;
end;

{*********************************************************************}
procedure TALHashedStringList.Put(Index: Integer; const S: AnsiString);
Var aNewName, aNewValue: AnsiString;
    aNewNvs: Boolean;
    aNewNode, aOldNode: TALHashedStringListDictionaryNode;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Changing;

  aNewNvs := ExtractNameValue(S, aNewName, aNewValue);
  aOldNode := FNodeList[index];
  if (CaseSensitive and (aOldNode.ID <> aNewName)) or
     ((not CaseSensitive) and (not ALSametext(aOldNode.ID, aNewName))) then begin
    aNewNode := TALHashedStringListDictionaryNode.Create;
    aNewNode.Idx := Index;
    aNewNode.ID := aNewName;
    aNewNode.Val := ANewValue;
    aNewNode.NVS := aNewNvs;
    aNewNode.Obj := aOldNode.Obj;
    if not Fdictionary.TryAdd(aNewName, aNewNode) then begin
      aNewNode.free;
      case Duplicates of
        dupIgnore: Exit;
        else Error(@SDuplicateString, 0);
      end;
    end;
    FNodeList[Index] := aNewNode;
    Fdictionary.Remove(aOldNode.ID);
  end
  else begin
    aOldNode.Val := aNewValue;
    aOldNode.NVS := aNewNVS;
  end;

  Changed;
end;

{************************************************************************}
procedure TALHashedStringList.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Changing;

  // Change from orignal TStringList
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FNodeList[Index].Obj
  else
    Obj := nil;

  FNodeList[Index].Obj := AObject;

  if Obj <> nil then
    Obj.Free;

  Changed;
end;

{**************************************************************}
procedure TALHashedStringList.SetCapacity(NewCapacity: Integer);
begin
  FDictionary.SetCapacity(NewCapacity);
  FNodeList.Capacity := NewCapacity;
end;

{***********************************************************************************************}
procedure TALHashedStringList.QuickSort(L, R: Integer; SCompare: TALHashedStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
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
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

{**************************************************************}
procedure TALHashedStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

{********************************************************************************}
procedure TALHashedStringList.CustomSort(Compare: TALHashedStringListSortCompare);
begin
  if (Count > 1) then
  begin
    Changing;
    QuickSort(0, Count - 1, Compare);
    Changed;
  end;
end;

{*************************************************************************************************************************************************************}
function TALHashedStringList.CreateDictionary(ACapacity: integer; aCaseSensitive: boolean): TALObjectDictionary<ansiString, TALHashedStringListDictionaryNode>;
begin
  if aCaseSensitive then result := TALObjectDictionary<ansiString, TALHashedStringListDictionaryNode>.create([doOwnsValues],
                                                                                                             ACapacity,
                                                                                                             TDelegatedEqualityComparer<ansiString>.Create(
                                                                                                               function(const Left, Right: ansiString): Boolean
                                                                                                               begin
                                                                                                                 Result := ALSameText(Left, Right);
                                                                                                               end,
                                                                                                               function(const Value: ansiString): Integer
                                                                                                               begin
                                                                                                                 Result := integer(ALStringHashCrc32(Value));
                                                                                                               end))
  else result := TALObjectDictionary<ansiString, TALHashedStringListDictionaryNode>.create([doOwnsValues],
                                                                                           ACapacity,
                                                                                           TDelegatedEqualityComparer<ansiString>.Create(
                                                                                             function(const Left, Right: ansiString): Boolean
                                                                                             begin
                                                                                               Result := ALSameText(Left, Right);
                                                                                             end,
                                                                                             function(const Value: ansiString): Integer
                                                                                             begin
                                                                                               Result := integer(ALStringHashCrc32(ALLowerCase(Value)));
                                                                                             end));
end;

{***************************************************************************}
procedure TALHashedStringList.init(OwnsObjects: Boolean; ACapacity: Integer);
begin
  FDictionary := CreateDictionary(ACapacity, False);
  FCaseSensitive := False;
  FNodeList := TObjectList<TALHashedStringListDictionaryNode>.Create(False);
  FNodeList.Capacity := ACapacity;
  FDuplicates := dupError;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := OwnsObjects;
end;

{*************************************}
constructor TALHashedStringList.Create;
begin
  inherited Create;
  init(False, 0);
end;

{***********************************************************}
constructor TALHashedStringList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  init(OwnsObjects, 0);
end;

{*********************************************************}
constructor TALHashedStringList.Create(ACapacity: Integer);
begin
  inherited Create;
  init(False, ACapacity);
end;

{*******************************************************************************}
constructor TALHashedStringList.Create(OwnsObjects: Boolean; ACapacity: Integer);
begin
  inherited Create;
  init(OwnsObjects, ACapacity);
end;

{*******************************************************************}
procedure TALHashedStringList.SetCaseSensitive(const Value: Boolean);
var aTmpDictionary: TALObjectDictionary<ansiString, TALHashedStringListDictionaryNode>;
    i: integer;
begin
  if fCaseSensitive <> Value then begin
    aTmpDictionary := CreateDictionary(count, Value);
    for I := 0 to FnodeList.Count - 1 do
      aTmpDictionary.Add(FNodeList[i].ID,FNodeList[i]);
    Fdictionary.Ownerships := [];
    Fdictionary.free;
    FDictionary := aTmpDictionary;
  end;
end;

{*****************************************************}
function TALHashedStringList.GetCaseSensitive: Boolean;
begin
  result := fCaseSensitive;
end;

{********************************************************************}
procedure TALHashedStringList.SetDuplicates(const Value: TDuplicates);
begin
  if value = dupAccept then raise exception.Create('TALHashedStringList does not support duplicate Names');
  FDuplicates := Value;
end;

{*******************************************************************************************************}
Function TALHashedStringList.ExtractNameValue(const S: AnsiString; var Name, Value: AnsiString): Boolean;
Var P1: Integer;
begin
  P1 := AlPos(NameValueSeparator,S);
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

{***************************************************************}
function TALHashedStringList.GetName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Result := FNodelist[Index].ID;
end;

{*********************************************************************}
function TALHashedStringList.GetStrictName(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  if FNodelist[Index].nvs then Result := FNodelist[Index].ID
  else result := ''
end;

{************************************************************************}
function TALHashedStringList.GetValue(const Name: AnsiString): AnsiString;
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

{********************************************************************}
procedure TALHashedStringList.SetValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then AddNameValue(Name, Value)
    else begin
      FNodeList[i].Val := Value;
      FNodeList[i].NVS := True;
    end
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{*************************************************************************}
function TALHashedStringList.GetValueFromIndex(Index: Integer): AnsiString;
begin
  if Index >= 0 then
  begin
    if Cardinal(Index) >= Cardinal(Count) then
      Error(@SListIndexError, Index);
    if (FNodeList[index].Nvs) then
      result := FNodeList[index].Val
    else
      Result := '';
  end
  else
    Result := '';
end;

{***************************************************************************************}
procedure TALHashedStringList.SetValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Value <> '' then
  begin
    if Index < 0 then AddNameValue('', Value)
    else begin
      if Cardinal(Index) >= Cardinal(Count) then
        Error(@SListIndexError, Index);
      FNodeList[Index].Val := Value;
      FNodeList[Index].NVS := True;
    end;
  end
  else
    if Index >= 0 then Delete(Index);
end;

{******************************************************************************}
procedure TALHashedStringList.SetPersistentValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then AddNameValue(Name, Value)
  else begin
    FNodeList[I].Val := Value;
    FNodeList[I].NVS := True;
  end
end;

{*************************************************************************************************}
procedure TALHashedStringList.SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Index < 0 then AddNameValue('', Value)
  else begin
    if Cardinal(Index) >= Cardinal(Count) then
      Error(@SListIndexError, Index);
    FNodeList[Index].Val := Value;
    FNodeList[Index].NVS := True;
  end;
end;

{$IFEND}

end.



