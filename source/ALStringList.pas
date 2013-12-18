{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALStringList
Version:      4.01

Description:  *TALStringList Work the same as Delphi TstringList except
               that it's allow to search a name=value using a quicksort
               algorithm when the list is sorted. Also TALStringList
               use a locale independant algorithme (based on the 8-bit
               ordinal value of each character) instead of the AnsiCompareText
               and AnsiCompareStr used by the Delphi TstringList. at the
               end the sort in TALStringList is up to 10x more faster
               than in Delphi TstringList. Also TALStringList is not an
               unicode TstringList but an 100% Ansi StringList

              *TALAVLStringList it's also a like a TStringlist, but use
               internaly an AVL binary Tree to speed up the
               insert / delete / lookup.

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

{$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)

Uses {$IF CompilerVersion >= 23} {Delphi XE2}
     System.Classes,
     System.Contnrs,
     {$ELSE}
     Classes,
     Contnrs,
     {$IFEND}
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
    function GetName(Index: Integer): AnsiString;
    function GetStrictName(Index: Integer): AnsiString; // [added from Tstrings]
    function GetValue(const Name: AnsiString): AnsiString;
    procedure SetCommaText(const Value: AnsiString);
    procedure SetDelimitedText(const Value: AnsiString);
    procedure SetValue(const Name, Value: AnsiString);
    procedure SetPersistentValue(const Name, Value: AnsiString); // [added from Tstrings]
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
    function GetValueFromIndex(Index: Integer): AnsiString;
    procedure SetValueFromIndex(Index: Integer; const Value: AnsiString);
    procedure SetPersistentValueFromIndex(Index: Integer; const Value: AnsiString); // [added from Tstrings]
  protected
    //[deleted from Tstrings] procedure SetEncoding(const Value: TEncoding);
    //[deleted from Tstrings] procedure DefineProperties(Filer: TFiler); override;
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
    constructor Create;
    function Add(const S: AnsiString): Integer; virtual;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; virtual;
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
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
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
    Constructor Create; Override;
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
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): AnsiString; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: AnsiString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure InsertItem(Index: Integer; const S: AnsiString; AObject: TObject); virtual;
    procedure AssignTo(Dest: TPersistent); override; //[added from Tstrings]
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    destructor Destroy; override;
    function Add(const S: AnsiString): Integer; override;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function  ExtractObject(Index: Integer): TObject; overload; virtual;
    procedure Exchange(Index1, Index2: Integer); override;
    function IndexOf(const S: AnsiString): Integer; override;
    function IndexOfName(const Name: AnsiString): Integer; override; // [added from TStringList]
    procedure Insert(Index: Integer; const S: AnsiString); override;
    procedure InsertObject(Index: Integer; const S: AnsiString; AObject: TObject); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure CustomSort(Compare: TALAVLStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write SetDuplicates;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

implementation

Uses {$IF CompilerVersion >= 23} {Delphi XE2}
     System.Sysutils,
     System.RTLConsts,
     {$IF CompilerVersion >= 24}{Delphi XE3}System.Ansistrings,{$IFEND}
     {$ELSE}
     sysutils,
     RTLConsts,
     {$IFEND}
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
  FDelimiter := ',';          // doesn't matter what we set here because of fDefined
  FLineBreak := sLineBreak;   // doesn't matter what we set here because of fDefined
  FQuoteChar := '"';          // doesn't matter what we set here because of fDefined
  FNameValueSeparator := '='; // doesn't matter what we set here because of fDefined
  FStrictDelimiter:= False;   // doesn't matter what we set here because of fDefined
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
    PPointer(@FList[FCount])^ := nil;
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
        if Duplicates <> dupAccept then L := I;
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
  if not Sorted then Result := inherited IndexOfName(Name)
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

  if CaseSensitive then
    Result := InternalCompareStr(S1, S2)
  else
    Result := InternalCompareText(S1, S2);

end;

{*******************************}
constructor TALStringList.Create;
begin
  inherited Create;
  setlength(FList, 0);
  FCount := 0;
  FCapacity := 0;
  FSorted := False;
  FDuplicates := dupIgnore;
  FCaseSensitive := False;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := False;
end;

{*****************************************************}
constructor TALStringList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
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
    dec(TALAVLStringListBinaryTreeNode(FNodelist[i]).Idx);

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

{***********************************************************}
procedure TALAVLStringList.Move(CurIndex, NewIndex: Integer);
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
      TALAVLStringListBinaryTreeNode(FNodeList[CurIndex]).Obj := nil;
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
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
    inc(TALAVLStringListBinaryTreeNode(FNodelist[i]).Idx);

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

{**********************************}
constructor TALAVLStringList.Create;
begin
  inherited Create;
  FAVLBinTree:= TALStringKeyAVLBinaryTree.Create;
  FAVLBinTree.CaseSensitive := False;
  FNodeList := TObjectList.Create(False);
  FDuplicates := dupError;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := False;
end;

{********************************************************}
constructor TALAVLStringList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FAVLBinTree:= TALStringKeyAVLBinaryTree.Create;
  FAVLBinTree.CaseSensitive := False;
  FNodeList := TObjectList.Create(False);
  FDuplicates := dupError;
  FOnChange := nil;
  FOnChanging := nil;
  FOwnsObject := OwnsObjects;
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

{************************************************}
constructor TALAVLStringListBinaryTreeNode.Create;
begin
  inherited;
  Val := '';
  Obj := nil;
  idx := -1;
  NVS := False;
end;

end.



