{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    Stéphane Vander Clock (skype/email: svanderclock@yahoo.fr)
							
product:      ALQuickSortList
Version:      4.00

Description:  TALIntegerList or TALDoubleList that work exactly
              like TstringList but with integer or Double.

Know bug :

History :     16/06/2012: Add xe2 Support

Link :

**************************************************************}
unit ALQuickSortList;

{Exemple of a QuickSort Algorithm :

procedure _SampleQuickSort(aArray: TArrayOfxxx; L, R: Integer);

  Function _Compare(I1,I2: xxx): Integer;
  Begin
    if I1 < I2 then result := -1
    else if I1 > I2 then result := 1
    else result := 0;
  end;

var aItem: xxx;
    I, J, P: Integer;

begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while _Compare(aArray[I], aArray[P]) < 0 do Inc(I);
      while _Compare(aArray[J], aArray[P]) > 0 do Dec(J);
      if I <= J then
      begin
        if I <> J then begin
          aItem := aArray[i];
          aArray[i] := aArray[j];
          aArray[j] := aItem;
        end;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then _SampleQuickSort(L, J);
    L := I;
  until I >= R;
end;

}

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

Uses System.Classes,
     System.RTLConsts,
     System.Generics.Defaults,
     System.Generics.Collections;

Type

  {----------------------------------------------------------------------------------}
  TALQuickSortListCompare = function(List: TObject; Index1, Index2: Integer): Integer;

  {-----------------------------------------}
  TALQuickSortPointerList = array of Pointer;

  {-----------------------------------}
  TALBaseQuickSortList = class(TObject)
  private
    FList: TALQuickSortPointerList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    procedure SetSorted(Value: Boolean);
    procedure QuickSort(L, R: Integer; SCompare: TALQuickSortListCompare);
  protected
    function  Get(Index: Integer): Pointer;
    procedure Grow;
    procedure Put(Index: Integer; Item: Pointer);
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function  CompareItems(const Index1, Index2: Integer): Integer; virtual;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure InsertItem(Index: Integer; Item: Pointer);
    procedure Insert(Index: Integer; Item: Pointer);
    property  List: TALQuickSortPointerList read FList;
  public
    Constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: NativeInt); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: NativeInt); overload;
    procedure Exchange(Index1, Index2: Integer);
    function  Expand: TALBaseQuickSortList;
    procedure CustomSort(Compare: TALQuickSortListCompare); virtual;
    procedure Sort; virtual;
    property  Sorted: Boolean read FSorted write SetSorted;
    property  Capacity: Integer read FCapacity write SetCapacity;
    property  Count: Integer read FCount write SetCount;
    property  Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

  {---------------------------------------}
  PALIntegerListItem = ^TALIntegerListItem;
  TALIntegerListItem = record
    FInteger: integer;
    FObject: TObject;
  end;

  {------------------------------------------}
  TALIntegerList = class(TALBaseQuickSortList)
  private
    FOwnsObject: Boolean;
    function  GetItem(Index: Integer): Integer;
    procedure SetItem(Index: Integer; const Item: Integer);
    function  GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const item: integer; AObject: TObject);
    function  CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    function  IndexOf(Item: Integer): Integer;
    function  IndexOfObject(AObject: TObject): Integer;
    function  Add(const Item: integer): Integer;
    Function  AddObject(const Item: integer; AObject: TObject): Integer;
    function  Find(item: Integer; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const item: integer);
    procedure InsertObject(Index: Integer; const item: integer; AObject: TObject);
    property  Items[Index: Integer]: Integer read GetItem write SetItem; default;
    property  Objects[Index: Integer]: TObject read GetObject write PutObject;
    property  OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    function  Push(Item: Integer): Integer;
    function  Pop: Integer; inline;
    function  Peek: Integer; inline;
  end;

  {-----------------------------------------}
  PALCardinalListItem = ^TALCardinalListItem;
  TALCardinalListItem = record
    FCardinal: Cardinal;
    FObject: TObject;
  end;

  {-------------------------------------------}
  TALCardinalList = class(TALBaseQuickSortList)
  private
    FOwnsObject: Boolean;
    function  GetItem(Index: Integer): Cardinal;
    procedure SetItem(Index: Integer; const Item: Cardinal);
    function  GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const item: Cardinal; AObject: TObject);
    function  CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    function  IndexOf(Item: Cardinal): Integer;
    function  IndexOfObject(AObject: TObject): Integer;
    function  Add(const Item: Cardinal): Integer;
    Function  AddObject(const Item: Cardinal; AObject: TObject): Integer;
    function  Find(item: Cardinal; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const item: Cardinal);
    procedure InsertObject(Index: Integer; const item: Cardinal; AObject: TObject);
    property  Items[Index: Integer]: Cardinal read GetItem write SetItem; default;
    property  Objects[Index: Integer]: TObject read GetObject write PutObject;
    property  OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    function  Push(Item: Cardinal): Cardinal;
    function  Pop: Cardinal; inline;
    function  Peek: Cardinal; inline;
  end;

  {-----------------------------------}
  PALInt64ListItem = ^TALInt64ListItem;
  TALInt64ListItem = record
    FInt64: Int64;
    FObject: TObject;
  end;

  {----------------------------------------}
  TALInt64List = class(TALBaseQuickSortList)
  private
    FOwnsObject: Boolean;
    function  GetItem(Index: Integer): Int64;
    procedure SetItem(Index: Integer; const Item: Int64);
    function  GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const item: Int64; AObject: TObject);
    function  CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    function  IndexOf(Item: Int64): Integer;
    function  IndexOfObject(AObject: TObject): Integer;
    function  Add(const Item: Int64): Integer;
    Function  AddObject(const Item: Int64; AObject: TObject): Integer;
    function  Find(item: Int64; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const item: Int64);
    procedure InsertObject(Index: Integer; const item: Int64; AObject: TObject);
    property  Items[Index: Integer]: Int64 read GetItem write SetItem; default;
    property  Objects[Index: Integer]: TObject read GetObject write PutObject;
    property  OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    function  Push(Item: Int64): Int64;
    function  Pop: Int64; inline;
    function  Peek: Int64; inline;
  end;

  {-------------------------------------------}
  PALNativeIntListItem = ^TALNativeIntListItem;
  TALNativeIntListItem = record
    FNativeInt: NativeInt;
    FObject: TObject;
  end;

  {--------------------------------------------}
  TALNativeIntList = class(TALBaseQuickSortList)
  private
    FOwnsObject: Boolean;
    function  GetItem(Index: Integer): NativeInt;
    procedure SetItem(Index: Integer; const Item: NativeInt);
    function  GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const item: NativeInt; AObject: TObject);
    function  CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    function  IndexOf(Item: NativeInt): Integer;
    function  IndexOfObject(AObject: TObject): Integer;
    function  Add(const Item: NativeInt): Integer;
    Function  AddObject(const Item: NativeInt; AObject: TObject): Integer;
    function  Find(item: NativeInt; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const item: NativeInt);
    procedure InsertObject(Index: Integer; const item: NativeInt; AObject: TObject);
    property  Items[Index: Integer]: NativeInt read GetItem write SetItem; default;
    property  Objects[Index: Integer]: TObject read GetObject write PutObject;
    property  OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    function  Push(Item: NativeInt): NativeInt;
    function  Pop: NativeInt; inline;
    function  Peek: NativeInt; inline;
  end;

  {-------------------------------------}
  PALDoubleListItem = ^TALDoubleListItem;
  TALDoubleListItem = record
    FDouble: Double;
    FObject: TObject;
  end;

  {-----------------------------------------}
  TALDoubleList = class(TALBaseQuickSortList)
  private
    FOwnsObject: Boolean;
    function  GetItem(Index: Integer): Double;
    procedure SetItem(Index: Integer; const Item: Double);
    function  GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const item: Double; AObject: TObject);
    function  CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    function  IndexOf(Item: Double): Integer;
    function  IndexOfObject(AObject: TObject): Integer;
    function  Add(const Item: Double): Integer;
    Function  AddObject(const Item: Double; AObject: TObject): Integer;
    function  Find(item: Double; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const item: Double);
    procedure InsertObject(Index: Integer; const item: Double; AObject: TObject);
    property  Items[Index: Integer]: Double read GetItem write SetItem; default;
    property  Objects[Index: Integer]: TObject read GetObject write PutObject;
    property  OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
    function  Push(Item: Double): Double;
    function  Pop: Double; inline;
    function  Peek: Double; inline;
  end;

  {------------------------}
  {$IF CompilerVersion > 32} // tokyo
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-13502 is still not yet implemented in System.Generics.Collections.pas'}
  {$IFEND}
  TALDictionary<TKey,TValue> = class(TEnumerable<TPair<TKey,TValue>>)
  private
    type
    TItem = record
      HashCode: Integer;
      Key: TKey;
      Value: TValue;
    end;
    TItemArray = array of TItem;
    private
    FItems: TItemArray;
    FCount: Integer;
    FComparer: IEqualityComparer<TKey>;
    FGrowThreshold: Integer;

    {$HINTS OFF}
    function ToArrayImpl(Count: Integer): TArray<TPair<TKey,TValue>>; // used by descendants
    {$HINTS ON}
    procedure Rehash(NewCapPow2: Integer);
    procedure Grow;
    function GetBucketIndex(const Key: TKey; HashCode: Integer): Integer;
    function Hash(const Key: TKey): Integer;
    function GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);
    procedure RehashAdd(HashCode: Integer; const Key: TKey; const Value: TValue);
    procedure DoAdd(HashCode, Index: Integer; const Key: TKey; const Value: TValue);
    procedure DoSetValue(Index: Integer; const Value: TValue);
    function DoRemove(const Key: TKey; HashCode: Integer; Notification: TCollectionNotification): TValue;
  protected
    function DoGetEnumerator: TEnumerator<TPair<TKey,TValue>>; override;
    procedure KeyNotify(const Key: TKey; Action: TCollectionNotification); virtual;
    procedure ValueNotify(const Value: TValue; Action: TCollectionNotification); virtual;
  public
    constructor Create(ACapacity: Integer = 0); overload;
    constructor Create(const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(ACapacity: Integer; const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(const Collection: TEnumerable<TPair<TKey,TValue>>); overload;
    constructor Create(const Collection: TEnumerable<TPair<TKey,TValue>>; const AComparer: IEqualityComparer<TKey>); overload;
    destructor Destroy; override;

    procedure SetCapacity(ACapacity: Integer);
    function TryAdd(const Key: TKey; const Value: TValue): boolean;
    procedure Add(const Key: TKey; const Value: TValue);
    procedure Remove(const Key: TKey);
    function ExtractPair(const Key: TKey): TPair<TKey,TValue>;
    procedure Clear;
    procedure TrimExcess;
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure AddOrSetValue(const Key: TKey; const Value: TValue);
    function ContainsKey(const Key: TKey): Boolean;
    function ContainsValue(const Value: TValue): Boolean;
    function ToArray: TArray<TPair<TKey,TValue>>; override; final;

    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property Count: Integer read FCount;

    type
      TPairEnumerator = class(TEnumerator<TPair<TKey,TValue>>)
      private
        FDictionary: TALDictionary<TKey,TValue>;
        FIndex: Integer;
        function GetCurrent: TPair<TKey,TValue>;
      protected
        function DoGetCurrent: TPair<TKey,TValue>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const ADictionary: TALDictionary<TKey,TValue>);
        property Current: TPair<TKey,TValue> read GetCurrent;
        function MoveNext: Boolean;
      end;

      TKeyEnumerator = class(TEnumerator<TKey>)
      private
        FDictionary: TALDictionary<TKey,TValue>;
        FIndex: Integer;
        function GetCurrent: TKey;
      protected
        function DoGetCurrent: TKey; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const ADictionary: TALDictionary<TKey,TValue>);
        property Current: TKey read GetCurrent;
        function MoveNext: Boolean;
      end;

      TValueEnumerator = class(TEnumerator<TValue>)
      private
        FDictionary: TALDictionary<TKey,TValue>;
        FIndex: Integer;
        function GetCurrent: TValue;
      protected
        function DoGetCurrent: TValue; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const ADictionary: TALDictionary<TKey,TValue>);
        property Current: TValue read GetCurrent;
        function MoveNext: Boolean;
      end;

      TValueCollection = class(TEnumerable<TValue>)
      private
        [Weak] FDictionary: TALDictionary<TKey,TValue>;
        {$HINTS OFF}
        function ToArrayImpl(Count: Integer): TArray<TValue>; // used by descendants
        {$HINTS ON}
        function GetCount: Integer;
      protected
        function DoGetEnumerator: TEnumerator<TValue>; override;
      public
        constructor Create(const ADictionary: TALDictionary<TKey,TValue>);
        function GetEnumerator: TValueEnumerator; reintroduce;
        function ToArray: TArray<TValue>; override; final;
        property Count: Integer read GetCount;
      end;

      TKeyCollection = class(TEnumerable<TKey>)
      private
        [Weak] FDictionary: TALDictionary<TKey,TValue>;
        {$HINTS OFF}
        function ToArrayImpl(Count: Integer): TArray<TKey>; // used by descendants
        {$HINTS ON}
        function GetCount: Integer;
      protected
        function DoGetEnumerator: TEnumerator<TKey>; override;
      public
        constructor Create(const ADictionary: TALDictionary<TKey,TValue>);
        function GetEnumerator: TKeyEnumerator; reintroduce;
        function ToArray: TArray<TKey>; override; final;
        property Count: Integer read GetCount;
      end;

  private
    FOnKeyNotify: TCollectionNotifyEvent<TKey>;
    FOnValueNotify: TCollectionNotifyEvent<TValue>;
    FKeyCollection: TKeyCollection;
    FValueCollection: TValueCollection;
    function GetKeys: TKeyCollection;
    function GetValues: TValueCollection;
  public
    function GetEnumerator: TPairEnumerator; reintroduce;
    property Keys: TKeyCollection read GetKeys;
    property Values: TValueCollection read GetValues;
    property OnKeyNotify: TCollectionNotifyEvent<TKey> read FOnKeyNotify write FOnKeyNotify;
    property OnValueNotify: TCollectionNotifyEvent<TValue> read FOnValueNotify write FOnValueNotify;
  end;

  {------------------------------------------------------------------}
  TALObjectDictionary<TKey,TValue> = class(TALDictionary<TKey,TValue>)
  private
    FOwnerships: TDictionaryOwnerships;
  protected
    procedure KeyNotify(const Key: TKey; Action: TCollectionNotification); override;
    procedure ValueNotify(const Value: TValue; Action: TCollectionNotification); override;
  public
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer = 0); overload;
    constructor Create(Ownerships: TDictionaryOwnerships; const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer; const AComparer: IEqualityComparer<TKey>); overload;
    property Ownerships: TDictionaryOwnerships read fOwnerships write fOwnerships;
  end;

resourcestring
  SALDuplicateItem = 'List does not allow duplicates';
  SALListCapacityError = 'List capacity out of bounds (%d)';
  SALListCountError = 'List count out of bounds (%d)';
  SALListIndexError = 'List index out of bounds (%d)';
  SALSortedListError = 'Operation not allowed on sorted list';

implementation

uses System.SysUtils,
     System.SysConst,
     System.TypInfo,
     System.Math,
     ALCommon,
     ALString;

{***********************************************************************************}
function AlBaseQuickSortListCompare(List: TObject; Index1, Index2: Integer): Integer;
Begin
  result := TALBaseQuickSortList(List).CompareItems(Index1, Index2);
end;

{**************************************}
constructor TALBaseQuickSortList.Create;
begin
  SetLength(FList,0);
  FCount:= 0;
  FCapacity:= 0;
  FSorted := False;
  FDuplicates := dupIgnore;
end;

{**************************************}
destructor TALBaseQuickSortList.Destroy;
begin
  Clear;
  inherited;
end;

{***********************************}
procedure TALBaseQuickSortList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

{***********************************************************************}
procedure TALBaseQuickSortList.InsertItem(Index: Integer; Item: Pointer);
begin
  if FCount = FCapacity then Grow;
  if Index < FCount then
    ALMove(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  Pointer(FList[Index]) := nil;
  FList[Index] := Item;
  Inc(FCount);
  if (Item <> nil) then
    Notify(Item, lnAdded);
end;

{********************************************************************}
procedure TALBaseQuickSortList.ExchangeItems(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  Item := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Item;
end;

{****************************************************}
procedure TALBaseQuickSortList.Delete(Index: Integer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SALListIndexError, Index);
  Temp := FList[Index];
  Dec(FCount);
  if Index < FCount then begin
    FList[Index] := nil;
    ALMove(FList[Index + 1],
           FList[Index],
          (FCount - Index) * SizeOf(Pointer));
    Pointer(FList[FCount]) := nil;
  end;
  if (Temp <> nil) then
    Notify(Temp, lnDeleted);
end;

{*****************************************************************************}
class procedure TALBaseQuickSortList.Error(const Msg: string; Data: NativeInt);
begin
  raise EListError.CreateFmt(Msg, [Data]);
end;

{******************************************************************************}
class procedure TALBaseQuickSortList.Error(Msg: PResStringRec; Data: NativeInt);
begin
  raise EListError.CreateFmt(LoadResString(Msg), [Data]);
end;

{***************************************************************}
procedure TALBaseQuickSortList.Exchange(Index1, Index2: Integer);
begin
  {Warning:	Do not call Exchange on a sorted list except to swap two identical
   items with different associated objects. Exchange does not check whether
   the list is sorted, and can destroy the sort order of a sorted list.}
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(@SALListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(@SALListIndexError, Index2);
  ExchangeItems(Index1, Index2);
end;

{*******************************************************************}
procedure TALBaseQuickSortList.Insert(Index: Integer; Item: Pointer);
begin
  if Sorted then Error(@SALSortedListError, 0);
  if (Index < 0) or (Index > FCount) then
    Error(@SALListIndexError, Index);
  InsertItem(Index, Item);
end;

{****************************************************************}
procedure TALBaseQuickSortList.Put(Index: Integer; Item: Pointer);
var
  Temp: Pointer;
begin
  if Sorted then
    Error(@SALSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then
    Error(@SALListIndexError, Index);
  if Item <> FList[Index] then
  begin
    Temp := FList[Index];
    FList[Index] := Item;
    if Temp <> nil then
      Notify(Temp, lnDeleted);
    if Item <> nil then
      Notify(Item, lnAdded);
  end;
end;

{*********************************************************}
function TALBaseQuickSortList.Get(Index: Integer): Pointer;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SALListIndexError, Index);
  Result := FList[Index];
end;

{*********************************************************}
function TALBaseQuickSortList.Expand: TALBaseQuickSortList;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

{**********************************}
procedure TALBaseQuickSortList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

{***************************************************************}
procedure TALBaseQuickSortList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SALListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{*********************************************************}
procedure TALBaseQuickSortList.SetCount(NewCount: Integer);
var
  I: Integer;
  Temp: Pointer;
begin
  if NewCount < 0 then
    Error(@SALListCountError, NewCount);
  if NewCount <> FCount then
  begin
    if NewCount > FCapacity then
      SetCapacity(NewCount);
    if NewCount > FCount then
      FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
    else
    for I := FCount - 1 downto NewCount do
    begin
      Dec(FCount);
      Temp := List[I];
      if Temp <> nil then
        Notify(Temp, lnDeleted);
    end;
    FCount := NewCount;
  end;
end;

{*****************************************************************************}
procedure TALBaseQuickSortList.Notify(Ptr: Pointer; Action: TListNotification);
begin
end;

{*******************************************************}
procedure TALBaseQuickSortList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

{*****************************************************************************************}
procedure TALBaseQuickSortList.QuickSort(L, R: Integer; SCompare: TALQuickSortListCompare);
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

{**************************************************************************}
procedure TALBaseQuickSortList.CustomSort(Compare: TALQuickSortListCompare);
begin
  if not Sorted and (FCount > 1) then
    QuickSort(0, FCount - 1, Compare);
end;

{**********************************}
procedure TALBaseQuickSortList.Sort;
begin
  CustomSort(AlBaseQuickSortListCompare);
end;

{*********************************************************************************}
function TALBaseQuickSortList.CompareItems(const Index1, Index2: Integer): Integer;
begin
  Result := 0;
end;

{********************************************************}
function TALIntegerList.Add(const Item: integer): Integer;
begin
  Result := AddObject(Item, nil);
end;

{********************************************************************************}
function TALIntegerList.AddObject(const Item: integer; AObject: TObject): Integer;
begin
  if not Sorted then Result := FCount
  else if Find(Item, Result) then
    case Duplicates of
      dupIgnore: Exit;
      dupError: Error(@SALDuplicateItem, 0);
    end;
  InsertItem(Result, Item, AObject);
end;

{*****************************************************************************************}
procedure TALIntegerList.InsertItem(Index: Integer; const item: integer; AObject: TObject);
Var aPALIntegerListItem: PALIntegerListItem;
begin
  New(aPALIntegerListItem);
  aPALIntegerListItem^.FInteger := item;
  aPALIntegerListItem^.FObject := AObject;
  try
    inherited InsertItem(index,aPALIntegerListItem);
  except
    aPALIntegerListItem^.FObject := nil;
    Dispose(aPALIntegerListItem);
    raise;
  end;
end;

{***************************************************************************}
function TALIntegerList.CompareItems(const Index1, Index2: integer): Integer;
begin
  result := CompareValue(PALIntegerListItem(Get(Index1))^.FInteger, PALIntegerListItem(Get(Index2))^.FInteger);
end;

{***********************************************************************}
function TALIntegerList.Find(item: Integer; var Index: Integer): Boolean;
var L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := GetItem(I) - item;
    if C < 0 then L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

{*******************************************************}
function TALIntegerList.GetItem(Index: Integer): Integer;
begin
  Result := PALIntegerListItem(Get(index))^.FInteger
end;

{********************************}
constructor TALIntegerList.Create;
begin
  inherited Create;
  FOwnsObject := false;
end;

{******************************************************}
constructor TALIntegerList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

{******************************************************}
function TALIntegerList.IndexOf(Item: Integer): Integer;
begin
  if not Sorted then Begin
    Result := 0;
    while (Result < FCount) and (GetItem(result) <> Item) do Inc(Result);
    if Result = FCount then Result := -1;
  end
  else if not Find(Item, Result) then Result := -1;
end;

{*******************************************************************}
procedure TALIntegerList.Insert(Index: Integer; const Item: Integer);
begin
  InsertObject(Index, Item, nil);
end;

{*******************************************************************************************}
procedure TALIntegerList.InsertObject(Index: Integer; const item: integer; AObject: TObject);
Var aPALIntegerListItem: PALIntegerListItem;
begin
  New(aPALIntegerListItem);
  aPALIntegerListItem^.FInteger := item;
  aPALIntegerListItem^.FObject := AObject;
  try
    inherited insert(index,aPALIntegerListItem);
  except
    aPALIntegerListItem^.FObject := nil;
    Dispose(aPALIntegerListItem);
    raise;
  end;
end;

{***********************************************************************}
procedure TALIntegerList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if OwnsObjects then ALfreeandnil(PALIntegerListItem(Ptr).FObject)
    else PALIntegerListItem(Ptr).FObject := nil; // to decrease the refcount in ARC
    dispose(ptr);
  end;
  inherited Notify(Ptr, Action);
end;

{********************************************************************}
procedure TALIntegerList.SetItem(Index: Integer; const Item: Integer);
Var aPALIntegerListItem: PALIntegerListItem;
begin
  New(aPALIntegerListItem);
  aPALIntegerListItem^.FInteger := item;
  aPALIntegerListItem^.FObject := nil;
  Try
    Put(Index, aPALIntegerListItem);
  except
    Dispose(aPALIntegerListItem);
    raise;
  end;
end;

{*********************************************************}
function TALIntegerList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SALListIndexError, Index);
  Result :=  PALIntegerListItem(Get(index))^.FObject;
end;

{***************************************************************}
function TALIntegerList.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to Count - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{*******************************************************************}
procedure TALIntegerList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SALListIndexError, Index);
  PALIntegerListItem(Get(index))^.FObject := AObject;
end;

{***************************************************}
function TALIntegerList.Push(Item: integer): integer;
begin
  Add(Item);
  result := Item;
end;

{***********************************}
function TALIntegerList.Pop: integer;
begin
  Result := Peek;
  Delete(Count-1);
end;

{************************************}
function TALIntegerList.Peek: integer;
begin
  Result := GetItem(Count-1);
end;

{**********************************************************}
function TALCardinalList.Add(const Item: Cardinal): Integer;
begin
  Result := AddObject(Item, nil);
end;

{**********************************************************************************}
function TALCardinalList.AddObject(const Item: Cardinal; AObject: TObject): Integer;
begin
  if not Sorted then Result := FCount
  else if Find(Item, Result) then
    case Duplicates of
      dupIgnore: Exit;
      dupError: Error(@SALDuplicateItem, 0);
    end;
  InsertItem(Result, Item, AObject);
end;

{*******************************************************************************************}
procedure TALCardinalList.InsertItem(Index: Integer; const item: Cardinal; AObject: TObject);
Var aPALCardinalListItem: PALCardinalListItem;
begin
  New(aPALCardinalListItem);
  aPALCardinalListItem^.FCardinal := item;
  aPALCardinalListItem^.FObject := AObject;
  try
    inherited InsertItem(index,aPALCardinalListItem);
  except
    aPALCardinalListItem^.FObject := nil;
    Dispose(aPALCardinalListItem);
    raise;
  end;
end;

{****************************************************************************}
function TALCardinalList.CompareItems(const Index1, Index2: integer): Integer;
var aCard1: Cardinal;
    aCard2: Cardinal;
begin
  aCard1 := PALCardinalListItem(Get(Index1))^.FCardinal;
  aCard2 := PALCardinalListItem(Get(Index2))^.FCardinal;
  if aCard1 < aCard2 then result := -1
  else if aCard1 > aCard2 then result := 1
  else result := 0;
end;

{*************************************************************************}
function TALCardinalList.Find(item: Cardinal; var Index: Integer): Boolean;
var L, H, I, C: Integer;

  {---------------------------------------------------------}
  Function _CompareCardinal(D1,D2: Cardinal): Integer; inline
  Begin
    if D1 < D2 then result := -1
    else if D1 > D2 then result := 1
    else result := 0;
  end;

begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := _CompareCardinal(GetItem(I),item);
    if C < 0 then L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

{*********************************************************}
function TALCardinalList.GetItem(Index: Integer): Cardinal;
begin
  Result := PALCardinalListItem(Get(index))^.FCardinal
end;

{*********************************}
constructor TALCardinalList.Create;
begin
  inherited Create;
  FOwnsObject := false;
end;

{*******************************************************}
constructor TALCardinalList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

{********************************************************}
function TALCardinalList.IndexOf(Item: Cardinal): Integer;
begin
  if not Sorted then Begin
    Result := 0;
    while (Result < FCount) and (GetItem(result) <> Item) do Inc(Result);
    if Result = FCount then Result := -1;
  end
  else if not Find(Item, Result) then Result := -1;
end;

{*********************************************************************}
procedure TALCardinalList.Insert(Index: Integer; const Item: Cardinal);
begin
  InsertObject(Index, Item, nil);
end;

{*********************************************************************************************}
procedure TALCardinalList.InsertObject(Index: Integer; const item: Cardinal; AObject: TObject);
Var aPALCardinalListItem: PALCardinalListItem;
begin
  New(aPALCardinalListItem);
  aPALCardinalListItem^.FCardinal := item;
  aPALCardinalListItem^.FObject := AObject;
  try
    inherited insert(index,aPALCardinalListItem);
  except
    aPALCardinalListItem^.FObject := nil;
    Dispose(aPALCardinalListItem);
    raise;
  end;
end;

{************************************************************************}
procedure TALCardinalList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if OwnsObjects then ALfreeandnil(PALCardinalListItem(Ptr).FObject)
    else PALCardinalListItem(Ptr).FObject := nil; // to decrease the refcount in ARC
    dispose(ptr);
  end;
  inherited Notify(Ptr, Action);
end;

{**********************************************************************}
procedure TALCardinalList.SetItem(Index: Integer; const Item: Cardinal);
Var aPALCardinalListItem: PALCardinalListItem;
begin
  New(aPALCardinalListItem);
  aPALCardinalListItem^.FCardinal := item;
  aPALCardinalListItem^.FObject := nil;
  Try
    Put(Index, aPALCardinalListItem);
  except
    Dispose(aPALCardinalListItem);
    raise;
  end;
end;

{**********************************************************}
function TALCardinalList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SALListIndexError, Index);
  Result :=  PALCardinalListItem(Get(index))^.FObject;
end;

{****************************************************************}
function TALCardinalList.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to Count - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{********************************************************************}
procedure TALCardinalList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SALListIndexError, Index);
  PALCardinalListItem(Get(index))^.FObject := AObject;
end;

{******************************************************}
function TALCardinalList.Push(Item: cardinal): cardinal;
begin
  Add(Item);
  result := Item;
end;

{*************************************}
function TALCardinalList.Pop: cardinal;
begin
  Result := Peek;
  Delete(Count-1);
end;

{**************************************}
function TALCardinalList.Peek: cardinal;
begin
  Result := GetItem(Count-1);
end;

{****************************************************}
function TALInt64List.Add(const Item: Int64): Integer;
begin
  Result := AddObject(Item, nil);
end;

{****************************************************************************}
function TALInt64List.AddObject(const Item: Int64; AObject: TObject): Integer;
begin
  if not Sorted then Result := FCount
  else if Find(Item, Result) then
    case Duplicates of
      dupIgnore: Exit;
      dupError: Error(@SALDuplicateItem, 0);
    end;
  InsertItem(Result, Item, AObject);
end;

{*************************************************************************************}
procedure TALInt64List.InsertItem(Index: Integer; const item: Int64; AObject: TObject);
Var aPALInt64ListItem: PALInt64ListItem;
begin
  New(aPALInt64ListItem);
  aPALInt64ListItem^.FInt64 := item;
  aPALInt64ListItem^.FObject := AObject;
  try
    inherited InsertItem(index,aPALInt64ListItem);
  except
    aPALInt64ListItem^.FObject := nil;
    Dispose(aPALInt64ListItem);
    raise;
  end;
end;

{*************************************************************************}
function TALInt64List.CompareItems(const Index1, Index2: integer): Integer;
begin
  result := compareValue(PALInt64ListItem(Get(Index1))^.FInt64, PALInt64ListItem(Get(Index2))^.FInt64);
end;

{*******************************************************************}
function TALInt64List.Find(item: Int64; var Index: Integer): Boolean;
var L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := CompareValue(GetItem(I),item);
    if C < 0 then L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

{***************************************************}
function TALInt64List.GetItem(Index: Integer): Int64;
begin
  Result := PALInt64ListItem(Get(index))^.FInt64
end;

{******************************}
constructor TALInt64List.Create;
begin
  inherited Create;
  FOwnsObject := false;
end;

{****************************************************}
constructor TALInt64List.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

{**************************************************}
function TALInt64List.IndexOf(Item: Int64): Integer;
begin
  if not Sorted then Begin
    Result := 0;
    while (Result < FCount) and (GetItem(result) <> Item) do Inc(Result);
    if Result = FCount then Result := -1;
  end
  else if not Find(Item, Result) then Result := -1;
end;

{***************************************************************}
procedure TALInt64List.Insert(Index: Integer; const Item: Int64);
begin
  InsertObject(Index, Item, nil);
end;

{***************************************************************************************}
procedure TALInt64List.InsertObject(Index: Integer; const item: Int64; AObject: TObject);
Var aPALInt64ListItem: PALInt64ListItem;
begin
  New(aPALInt64ListItem);
  aPALInt64ListItem^.FInt64 := item;
  aPALInt64ListItem^.FObject := AObject;
  try
    inherited insert(index,aPALInt64ListItem);
  except
    aPALInt64ListItem^.FObject := nil;
    Dispose(aPALInt64ListItem);
    raise;
  end;
end;

{*********************************************************************}
procedure TALInt64List.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if OwnsObjects then ALfreeandnil(PALInt64ListItem(Ptr).FObject)
    else PALInt64ListItem(Ptr).FObject := nil; // to decrease the refcount in ARC
    dispose(ptr);
  end;
  inherited Notify(Ptr, Action);
end;

{****************************************************************}
procedure TALInt64List.SetItem(Index: Integer; const Item: Int64);
Var aPALInt64ListItem: PALInt64ListItem;
begin
  New(aPALInt64ListItem);
  aPALInt64ListItem^.FInt64 := item;
  aPALInt64ListItem^.FObject := nil;
  Try
    Put(Index, aPALInt64ListItem);
  except
    Dispose(aPALInt64ListItem);
    raise;
  end;
end;

{*******************************************************}
function TALInt64List.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SALListIndexError, Index);
  Result :=  PALInt64ListItem(Get(index))^.FObject;
end;

{*************************************************************}
function TALInt64List.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to Count - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{*****************************************************************}
procedure TALInt64List.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SALListIndexError, Index);
  PALInt64ListItem(Get(index))^.FObject := AObject;
end;

{**********************************************}
function TALInt64List.Push(Item: int64): int64;
begin
  Add(Item);
  result := Item;
end;

{*******************************}
function TALInt64List.Pop: int64;
begin
  Result := Peek;
  Delete(Count-1);
end;

{********************************}
function TALInt64List.Peek: int64;
begin
  Result := GetItem(Count-1);
end;

{************************************************************}
function TALNativeIntList.Add(const Item: NativeInt): Integer;
begin
  Result := AddObject(Item, nil);
end;

{************************************************************************************}
function TALNativeIntList.AddObject(const Item: NativeInt; AObject: TObject): Integer;
begin
  if not Sorted then Result := FCount
  else if Find(Item, Result) then
    case Duplicates of
      dupIgnore: Exit;
      dupError: Error(@SALDuplicateItem, 0);
    end;
  InsertItem(Result, Item, AObject);
end;

{*********************************************************************************************}
procedure TALNativeIntList.InsertItem(Index: Integer; const item: NativeInt; AObject: TObject);
Var aPALNativeIntListItem: PALNativeIntListItem;
begin
  New(aPALNativeIntListItem);
  aPALNativeIntListItem^.FNativeInt := item;
  aPALNativeIntListItem^.FObject := AObject;
  try
    inherited InsertItem(index,aPALNativeIntListItem);
  except
    aPALNativeIntListItem^.FObject := nil;
    Dispose(aPALNativeIntListItem);
    raise;
  end;
end;

{*****************************************************************************}
function TALNativeIntList.CompareItems(const Index1, Index2: integer): Integer;
var aNativeInt1: Cardinal;
    aNativeInt2: Cardinal;
begin
  aNativeInt1 := PALNativeIntListItem(Get(Index1))^.FNativeInt;
  aNativeInt2 := PALNativeIntListItem(Get(Index2))^.FNativeInt;
  if aNativeInt1 < aNativeInt2 then result := -1
  else if aNativeInt1 > aNativeInt2 then result := 1
  else result := 0;
end;

{***************************************************************************}
function TALNativeIntList.Find(item: NativeInt; var Index: Integer): Boolean;
var L, H, I, C: Integer;

  {------------------------------------------------------------}
  Function _CompareNativeInt(D1,D2: NativeInt): Integer; inline;
  Begin
    if D1 < D2 then result := -1
    else if D1 > D2 then result := 1
    else result := 0;
  end;

begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := _CompareNativeInt(GetItem(I),item);
    if C < 0 then L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

{***********************************************************}
function TALNativeIntList.GetItem(Index: Integer): NativeInt;
begin
  Result := PALNativeIntListItem(Get(index))^.FNativeInt
end;

{**********************************}
constructor TALNativeIntList.Create;
begin
  inherited Create;
  FOwnsObject := false;
end;

{********************************************************}
constructor TALNativeIntList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

{**********************************************************}
function TALNativeIntList.IndexOf(Item: NativeInt): Integer;
begin
  if not Sorted then Begin
    Result := 0;
    while (Result < FCount) and (GetItem(result) <> Item) do Inc(Result);
    if Result = FCount then Result := -1;
  end
  else if not Find(Item, Result) then Result := -1;
end;

{***********************************************************************}
procedure TALNativeIntList.Insert(Index: Integer; const Item: NativeInt);
begin
  InsertObject(Index, Item, nil);
end;

{***********************************************************************************************}
procedure TALNativeIntList.InsertObject(Index: Integer; const item: NativeInt; AObject: TObject);
Var aPALNativeIntListItem: PALNativeIntListItem;
begin
  New(aPALNativeIntListItem);
  aPALNativeIntListItem^.FNativeInt := item;
  aPALNativeIntListItem^.FObject := AObject;
  try
    inherited insert(index,aPALNativeIntListItem);
  except
    aPALNativeIntListItem^.FObject := nil;
    Dispose(aPALNativeIntListItem);
    raise;
  end;
end;

{*************************************************************************}
procedure TALNativeIntList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if OwnsObjects then ALfreeandnil(PALNativeIntListItem(Ptr).FObject)
    else PALNativeIntListItem(Ptr).FObject := nil; // to decrease the refcount in ARC
    dispose(ptr);
  end;
  inherited Notify(Ptr, Action);
end;

{************************************************************************}
procedure TALNativeIntList.SetItem(Index: Integer; const Item: NativeInt);
Var aPALNativeIntListItem: PALNativeIntListItem;
begin
  New(aPALNativeIntListItem);
  aPALNativeIntListItem^.FNativeInt := item;
  aPALNativeIntListItem^.FObject := nil;
  Try
    Put(Index, aPALNativeIntListItem);
  except
    Dispose(aPALNativeIntListItem);
    raise;
  end;
end;

{***********************************************************}
function TALNativeIntList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SALListIndexError, Index);
  Result :=  PALNativeIntListItem(Get(index))^.FObject;
end;

{*****************************************************************}
function TALNativeIntList.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to Count - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{*********************************************************************}
procedure TALNativeIntList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SALListIndexError, Index);
  PALNativeIntListItem(Get(index))^.FObject := AObject;
end;

{*********************************************************}
function TALNativeIntList.Push(Item: NativeInt): NativeInt;
begin
  Add(Item);
  result := Item;
end;

{***************************************}
function TALNativeIntList.Pop: NativeInt;
begin
  Result := Peek;
  Delete(Count-1);
end;

{****************************************}
function TALNativeIntList.Peek: NativeInt;
begin
  Result := GetItem(Count-1);
end;

{******************************************************}
function TALDoubleList.Add(const Item: Double): Integer;
begin
  Result := AddObject(Item, nil);
end;

{******************************************************************************}
function TALDoubleList.AddObject(const Item: Double; AObject: TObject): Integer;
begin
  if not Sorted then Result := FCount
  else if Find(Item, Result) then
    case Duplicates of
      dupIgnore: Exit;
      dupError: Error(@SALDuplicateItem, 0);
    end;
  InsertItem(Result, Item, AObject);
end;

{***************************************************************************************}
procedure TALDoubleList.InsertItem(Index: Integer; const item: Double; AObject: TObject);
Var aPALDoubleListItem: PALDoubleListItem;
begin
  New(aPALDoubleListItem);
  aPALDoubleListItem^.FDouble := item;
  aPALDoubleListItem^.FObject := AObject;
  try
    inherited InsertItem(index,aPALDoubleListItem);
  except
    aPALDoubleListItem^.FObject := nil;
    Dispose(aPALDoubleListItem);
    raise;
  end;
end;

{**************************************************************************}
function TALDoubleList.CompareItems(const Index1, Index2: integer): Integer;
begin
  result := compareValue(PALDoubleListItem(Get(Index1))^.FDouble, PALDoubleListItem(Get(Index2))^.FDouble);
end;

{*********************************************************************}
function TALDoubleList.Find(item: Double; var Index: Integer): Boolean;
var L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := compareValue(GetItem(I),item);
    if C < 0 then L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

{*****************************************************}
function TALDoubleList.GetItem(Index: Integer): Double;
begin
  Result := PALDoubleListItem(Get(index))^.FDouble
end;

{*******************************}
constructor TALDoubleList.Create;
begin
  inherited Create;
  FOwnsObject := false;
end;

{*****************************************************}
constructor TALDoubleList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

{****************************************************}
function TALDoubleList.IndexOf(Item: Double): Integer;
begin
  if not Sorted then Begin
    Result := 0;
    while (Result < FCount) and (GetItem(result) <> Item) do Inc(Result);
    if sameValue(Result, FCount) then Result := -1;
  end
  else if not Find(Item, Result) then Result := -1;
end;

{*****************************************************************}
procedure TALDoubleList.Insert(Index: Integer; const Item: Double);
begin
  InsertObject(Index, Item, nil);
end;

{*****************************************************************************************}
procedure TALDoubleList.InsertObject(Index: Integer; const item: Double; AObject: TObject);
Var aPALDoubleListItem: PALDoubleListItem;
begin
  New(aPALDoubleListItem);
  aPALDoubleListItem^.FDouble := item;
  aPALDoubleListItem^.FObject := AObject;
  try
    inherited insert(index,aPALDoubleListItem);
  except
    aPALDoubleListItem^.FObject := nil;
    Dispose(aPALDoubleListItem);
    raise;
  end;
end;

{**********************************************************************}
procedure TALDoubleList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if OwnsObjects then ALfreeandnil(PALDoubleListItem(Ptr).FObject)
    else PALDoubleListItem(Ptr).FObject := nil; // to decrease the refcount in ARC
    dispose(ptr);
  end;
  inherited Notify(Ptr, Action);
end;

{******************************************************************}
procedure TALDoubleList.SetItem(Index: Integer; const Item: Double);
Var aPALDoubleListItem: PALDoubleListItem;
begin
  New(aPALDoubleListItem);
  aPALDoubleListItem^.FDouble := item;
  aPALDoubleListItem^.FObject := nil;
  Try
    Put(Index, aPALDoubleListItem);
  except
    Dispose(aPALDoubleListItem);
    raise;
  end;
end;

{********************************************************}
function TALDoubleList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SALListIndexError, Index);
  Result :=  PALDoubleListItem(Get(index))^.FObject;
end;

{**************************************************************}
function TALDoubleList.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to Count - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{******************************************************************}
procedure TALDoubleList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SALListIndexError, Index);
  PALDoubleListItem(Get(index))^.FObject := AObject;
end;

{************************************************}
function TALDoubleList.Push(Item: Double): Double;
begin
  Add(Item);
  result := Item;
end;

{*********************************}
function TALDoubleList.Pop: Double;
begin
  Result := Peek;
  Delete(Count-1);
end;

{**********************************}
function TALDoubleList.Peek: Double;
begin
  Result := GetItem(Count-1);
end;

{***}
const
  EMPTY_HASH = -1;

{******************************************************************************************}
function TALDictionary<TKey,TValue>.ToArrayImpl(Count: Integer): TArray<TPair<TKey,TValue>>;
var
  Value: TPair<TKey,TValue>;
begin
  // We assume our caller has passed correct Count
  SetLength(Result, Count);
  Count := 0;
  for Value in Self do
  begin
    Result[Count] := Value;
    Inc(Count);
  end;
end;

{***************************************************************}
procedure TALDictionary<TKey,TValue>.Rehash(NewCapPow2: Integer);
var
  oldItems, newItems: TItemArray;
  i: Integer;
begin
  if NewCapPow2 = Length(FItems) then
    Exit
  else if NewCapPow2 < 0 then
    OutOfMemoryError;

  oldItems := FItems;
  SetLength(newItems, NewCapPow2);
  for i := 0 to Length(newItems) - 1 do
    newItems[i].HashCode := EMPTY_HASH;
  FItems := newItems;
  FGrowThreshold := NewCapPow2 shr 1 + NewCapPow2 shr 2; // 75%

  for i := 0 to Length(oldItems) - 1 do
    if oldItems[i].HashCode <> EMPTY_HASH then
      RehashAdd(oldItems[i].HashCode, oldItems[i].Key, oldItems[i].Value);
end;

{*******************************************************************}
procedure TALDictionary<TKey,TValue>.SetCapacity(ACapacity: Integer);
var
  newCap: Integer;
begin
  if ACapacity < Count then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  if ACapacity = 0 then
    Rehash(0)
  else
  begin
    newCap := 4;
    while newCap < ACapacity do
      newCap := newCap shl 1;
    Rehash(newCap);
  end
end;

{****************************************}
procedure TALDictionary<TKey,TValue>.Grow;
var
  newCap: Integer;
begin
  newCap := Length(FItems) * 2;
  if newCap = 0 then
    newCap := 4;
  Rehash(newCap);
end;

{**********************************************************************************************}
function TALDictionary<TKey,TValue>.GetBucketIndex(const Key: TKey; HashCode: Integer): Integer;
var
  start, hc: Integer;
begin
  if Length(FItems) = 0 then
    Exit(not High(Integer));

  start := HashCode and (Length(FItems) - 1);
  Result := start;
  while True do
  begin
    hc := FItems[Result].HashCode;

    // Not found: return complement of insertion point.
    if hc = EMPTY_HASH then
      Exit(not Result);

    // Found: return location.
    if (hc = HashCode) and FComparer.Equals(FItems[Result].Key, Key) then
      Exit(Result);

    Inc(Result);
    if Result >= Length(FItems) then
      Result := 0;
  end;
end;

{*****************************************************************}
function TALDictionary<TKey,TValue>.Hash(const Key: TKey): Integer;
const
  PositiveMask = not Integer($80000000);
begin
  // Double-Abs to avoid -MaxInt and MinInt problems.
  // Not using compiler-Abs because we *must* get a positive integer;
  // for compiler, Abs(Low(Integer)) is a null op.
  Result := PositiveMask and ((PositiveMask and FComparer.GetHashCode(Key)) + 1);
end;

{*******************************************************************}
function TALDictionary<TKey,TValue>.GetItem(const Key: TKey): TValue;
var
  index: Integer;
begin
  index := GetBucketIndex(Key, Hash(Key));
  if index < 0 then
    raise EListError.CreateRes(@SGenericItemNotFound);
  Result := FItems[index].Value;
end;

{*********************************************************************************}
procedure TALDictionary<TKey,TValue>.SetItem(const Key: TKey; const Value: TValue);
var
  index: Integer;
  oldValue: TValue;
begin
  index := GetBucketIndex(Key, Hash(Key));
  if index < 0 then
    raise EListError.CreateRes(@SGenericItemNotFound);

  oldValue := FItems[index].Value;
  FItems[index].Value := Value;

  ValueNotify(oldValue, cnRemoved);
  ValueNotify(Value, cnAdded);
end;

{******************************************************************************************************}
procedure TALDictionary<TKey,TValue>.RehashAdd(HashCode: Integer; const Key: TKey; const Value: TValue);
var
  index: Integer;
begin
  index := not GetBucketIndex(Key, HashCode);
  FItems[index].HashCode := HashCode;
  FItems[index].Key := Key;
  FItems[index].Value := Value;
end;

{***********************************************************************************************}
procedure TALDictionary<TKey,TValue>.KeyNotify(const Key: TKey; Action: TCollectionNotification);
begin
  if Assigned(FOnKeyNotify) then
    FOnKeyNotify(Self, Key, Action);
end;

{*****************************************************************************************************}
procedure TALDictionary<TKey,TValue>.ValueNotify(const Value: TValue; Action: TCollectionNotification);
begin
  if Assigned(FOnValueNotify) then
    FOnValueNotify(Self, Value, Action);
end;

{********************************************************************}
constructor TALDictionary<TKey,TValue>.Create(ACapacity: Integer = 0);
begin
  Create(ACapacity, nil);
end;

{**************************************************************************************}
constructor TALDictionary<TKey,TValue>.Create(const AComparer: IEqualityComparer<TKey>);
begin
  Create(0, AComparer);
end;

{**********************************************************************************************************}
constructor TALDictionary<TKey,TValue>.Create(ACapacity: Integer; const AComparer: IEqualityComparer<TKey>);
var
  cap: Integer;
begin
  inherited Create;
  if ACapacity < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  FComparer := AComparer;
  if FComparer = nil then
    FComparer := TEqualityComparer<TKey>.Default;
  SetCapacity(ACapacity);
end;

{*************************************************************************************************}
constructor TALDictionary<TKey, TValue>.Create(const Collection: TEnumerable<TPair<TKey, TValue>>);
var
  item: TPair<TKey,TValue>;
begin
  Create(0, nil);
  for item in Collection do
    AddOrSetValue(item.Key, item.Value);
end;

{************************************************************************************************}
constructor TALDictionary<TKey, TValue>.Create(const Collection: TEnumerable<TPair<TKey, TValue>>;
  const AComparer: IEqualityComparer<TKey>);
var
  item: TPair<TKey,TValue>;
begin
  Create(0, AComparer);
  for item in Collection do
    AddOrSetValue(item.Key, item.Value);
end;

{********************************************}
destructor TALDictionary<TKey,TValue>.Destroy;
begin
  Clear;
  ALFreeAndNil(FKeyCollection);
  ALFreeAndNil(FValueCollection);
  inherited;
end;

{****************************************************************************************}
function TALDictionary<TKey,TValue>.TryAdd(const Key: TKey; const Value: TValue): boolean;
var
  index, hc: Integer;
begin
  if Count >= FGrowThreshold then
    Grow;

  hc := Hash(Key);
  index := GetBucketIndex(Key, hc);
  if index >= 0 then exit(False);

  result := true;
  DoAdd(hc, not index, Key, Value);
end;

{*****************************************************************************}
procedure TALDictionary<TKey,TValue>.Add(const Key: TKey; const Value: TValue);
var
  index, hc: Integer;
begin
  if Count >= FGrowThreshold then
    Grow;

  hc := Hash(Key);
  index := GetBucketIndex(Key, hc);
  if index >= 0 then
    raise EListError.CreateRes(@SGenericDuplicateItem);

  DoAdd(hc, not index, Key, Value);
end;

{******************************************************************************}
function TALDictionary<TKey,TValue>.DoRemove(const Key: TKey; HashCode: Integer;
  Notification: TCollectionNotification): TValue;
var
  gap, index, hc, bucket: Integer;
  LKey: TKey;
begin
  index := GetBucketIndex(Key, HashCode);
  if index < 0 then
    Exit(Default(TValue));

  // Removing item from linear probe hash table is moderately
  // tricky. We need to fill in gaps, which will involve moving items
  // which may not even hash to the same location.
  // Knuth covers it well enough in Vol III. 6.4.; but beware, Algorithm R
  // (2nd ed) has a bug: step R4 should go to step R1, not R2 (already errata'd).
  // My version does linear probing forward, not backward, however.

  // gap refers to the hole that needs filling-in by shifting items down.
  // index searches for items that have been probed out of their slot,
  // but being careful not to move items if their bucket is between
  // our gap and our index (so that they'd be moved before their bucket).
  // We move the item at index into the gap, whereupon the new gap is
  // at the index. If the index hits a hole, then we're done.

  // If our load factor was exactly 1, we'll need to hit this hole
  // in order to terminate. Shouldn't normally be necessary, though.
  FItems[index].HashCode := EMPTY_HASH;
  Result := FItems[index].Value;
  LKey := FItems[index].Key;

  gap := index;
  while True do
  begin
    Inc(index);
    if index = Length(FItems) then
      index := 0;

    hc := FItems[index].HashCode;
    if hc = EMPTY_HASH then
      Break;

    bucket := hc and (Length(FItems) - 1);
    if not InCircularRange(gap, bucket, index) then
    begin
      FItems[gap] := FItems[index];
      gap := index;
      // The gap moved, but we still need to find it to terminate.
      FItems[gap].HashCode := EMPTY_HASH;
    end;
  end;

  FItems[gap].HashCode := EMPTY_HASH;
  FItems[gap].Key := Default(TKey);
  FItems[gap].Value := Default(TValue);
  Dec(FCount);

  KeyNotify(LKey, Notification);
  ValueNotify(Result, Notification);
end;

{***********************************************************}
procedure TALDictionary<TKey,TValue>.Remove(const Key: TKey);
begin
  DoRemove(Key, Hash(Key), cnRemoved);
end;

{***********************************************************************************}
function TALDictionary<TKey,TValue>.ExtractPair(const Key: TKey): TPair<TKey,TValue>;
var
  hc, index: Integer;
begin
  hc := Hash(Key);
  index := GetBucketIndex(Key, hc);
  if index < 0 then
    Exit(TPair<TKey,TValue>.Create(Key, Default(TValue)));

  Result := TPair<TKey,TValue>.Create(Key, DoRemove(Key, hc, cnExtracted));
end;

{*****************************************}
procedure TALDictionary<TKey,TValue>.Clear;
var
  i: Integer;
  oldItems: TItemArray;
begin
  oldItems := FItems;
  FCount := 0;
  SetLength(FItems, 0);
  SetCapacity(0);
  FGrowThreshold := 0;

  for i := 0 to Length(oldItems) - 1 do
  begin
    if oldItems[i].HashCode = EMPTY_HASH then
      Continue;
    KeyNotify(oldItems[i].Key, cnRemoved);
    ValueNotify(oldItems[i].Value, cnRemoved);
  end;
end;

{***********************************************************************}
function TALDictionary<TKey, TValue>.ToArray: TArray<TPair<TKey,TValue>>;
begin
  Result := ToArrayImpl(Count);
end;

{**********************************************}
procedure TALDictionary<TKey,TValue>.TrimExcess;
begin
  // Ensure at least one empty slot for GetBucketIndex to terminate.
  SetCapacity(Count + 1);
end;

{*******************************************************************************************}
function TALDictionary<TKey,TValue>.TryGetValue(const Key: TKey; out Value: TValue): Boolean;
var
  index: Integer;
begin
  index := GetBucketIndex(Key, Hash(Key));
  Result := index >= 0;
  if Result then
    Value := FItems[index].Value
  else
    Value := Default(TValue);
end;

{*********************************************************************************************************}
procedure TALDictionary<TKey,TValue>.DoAdd(HashCode, Index: Integer; const Key: TKey; const Value: TValue);
begin
  FItems[Index].HashCode := HashCode;
  FItems[Index].Key := Key;
  FItems[Index].Value := Value;
  Inc(FCount);

  KeyNotify(Key, cnAdded);
  ValueNotify(Value, cnAdded);
end;

{*************************************************************************************}
function TALDictionary<TKey, TValue>.DoGetEnumerator: TEnumerator<TPair<TKey, TValue>>;
begin
  Result := GetEnumerator;
end;

{***********************************************************************************}
procedure TALDictionary<TKey,TValue>.DoSetValue(Index: Integer; const Value: TValue);
var
  oldValue: TValue;
begin
  oldValue := FItems[Index].Value;
  FItems[Index].Value := Value;

  ValueNotify(oldValue, cnRemoved);
  ValueNotify(Value, cnAdded);
end;

{***************************************************************************************}
procedure TALDictionary<TKey,TValue>.AddOrSetValue(const Key: TKey; const Value: TValue);
var
  hc: Integer;
  index: Integer;
begin
  hc := Hash(Key);
  index := GetBucketIndex(Key, hc);
  if index >= 0 then
    DoSetValue(index, Value)
  else
  begin
    // We only grow if we are inserting a new value.
    if Count >= FGrowThreshold then
    begin
      Grow;
      // We need a new Bucket Index because the array has grown.
      index := GetBucketIndex(Key, hc);
    end;
    DoAdd(hc, not index, Key, Value);
  end;
end;

{************************************************************************}
function TALDictionary<TKey,TValue>.ContainsKey(const Key: TKey): Boolean;
begin
  Result := GetBucketIndex(Key, Hash(Key)) >= 0;
end;

{******************************************************************************}
function TALDictionary<TKey,TValue>.ContainsValue(const Value: TValue): Boolean;
var
  i: Integer;
  c: IEqualityComparer<TValue>;
begin
  c := TEqualityComparer<TValue>.Default;

  for i := 0 to Length(FItems) - 1 do
    if (FItems[i].HashCode <> EMPTY_HASH) and c.Equals(FItems[i].Value, Value) then
      Exit(True);
  Result := False;
end;

{*****************************************************************}
function TALDictionary<TKey,TValue>.GetEnumerator: TPairEnumerator;
begin
  Result := TPairEnumerator.Create(Self);
end;

{**********************************************************}
function TALDictionary<TKey,TValue>.GetKeys: TKeyCollection;
begin
  if FKeyCollection = nil then
    FKeyCollection := TKeyCollection.Create(Self);
  Result := FKeyCollection;
end;

{**************************************************************}
function TALDictionary<TKey,TValue>.GetValues: TValueCollection;
begin
  if FValueCollection = nil then
    FValueCollection := TValueCollection.Create(Self);
  Result := FValueCollection;
end;

{***********************************************************************************************************}
constructor TALDictionary<TKey,TValue>.TPairEnumerator.Create(const ADictionary: TALDictionary<TKey,TValue>);
begin
  inherited Create;
  FIndex := -1;
  FDictionary := ADictionary;
end;

{*************************************************************************************}
function TALDictionary<TKey, TValue>.TPairEnumerator.DoGetCurrent: TPair<TKey, TValue>;
begin
  Result := GetCurrent;
end;

{***********************************************************************}
function TALDictionary<TKey, TValue>.TPairEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

{*********************************************************************************}
function TALDictionary<TKey,TValue>.TPairEnumerator.GetCurrent: TPair<TKey,TValue>;
begin
  Result.Key := FDictionary.FItems[FIndex].Key;
  Result.Value := FDictionary.FItems[FIndex].Value;
end;

{********************************************************************}
function TALDictionary<TKey,TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  while FIndex < Length(FDictionary.FItems) - 1 do
  begin
    Inc(FIndex);
    if FDictionary.FItems[FIndex].HashCode <> EMPTY_HASH then
      Exit(True);
  end;
  Result := False;
end;

{**********************************************************************************************************}
constructor TALDictionary<TKey,TValue>.TKeyEnumerator.Create(const ADictionary: TALDictionary<TKey,TValue>);
begin
  inherited Create;
  FIndex := -1;
  FDictionary := ADictionary;
end;

{*********************************************************************}
function TALDictionary<TKey, TValue>.TKeyEnumerator.DoGetCurrent: TKey;
begin
  Result := GetCurrent;
end;

{**********************************************************************}
function TALDictionary<TKey, TValue>.TKeyEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

{******************************************************************}
function TALDictionary<TKey,TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  Result := FDictionary.FItems[FIndex].Key;
end;

{*******************************************************************}
function TALDictionary<TKey,TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  while FIndex < Length(FDictionary.FItems) - 1 do
  begin
    Inc(FIndex);
    if FDictionary.FItems[FIndex].HashCode <> EMPTY_HASH then
      Exit(True);
  end;
  Result := False;
end;

{************************************************************************************************************}
constructor TALDictionary<TKey,TValue>.TValueEnumerator.Create(const ADictionary: TALDictionary<TKey,TValue>);
begin
  inherited Create;
  FIndex := -1;
  FDictionary := ADictionary;
end;

{*************************************************************************}
function TALDictionary<TKey, TValue>.TValueEnumerator.DoGetCurrent: TValue;
begin
  Result := GetCurrent;
end;

{************************************************************************}
function TALDictionary<TKey, TValue>.TValueEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

{**********************************************************************}
function TALDictionary<TKey,TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := FDictionary.FItems[FIndex].Value;
end;

{*********************************************************************}
function TALDictionary<TKey,TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  while FIndex < Length(FDictionary.FItems) - 1 do
  begin
    Inc(FIndex);
    if FDictionary.FItems[FIndex].HashCode <> EMPTY_HASH then
      Exit(True);
  end;
  Result := False;
end;

{**************************************************************************************************************}
constructor TALDictionary<TKey, TValue>.TValueCollection.Create(const ADictionary: TALDictionary<TKey, TValue>);
begin
  inherited Create;
  FDictionary := ADictionary;
end;

{*****************************************************************************************}
function TALDictionary<TKey, TValue>.TValueCollection.DoGetEnumerator: TEnumerator<TValue>;
begin
  Result := GetEnumerator;
end;

{************************************************************************************************}
function TALDictionary<TKey, TValue>.TValueCollection.ToArrayImpl(Count: Integer): TArray<TValue>;
var
  Value: TValue;
begin
  // We assume our caller has passed correct Count
  SetLength(Result, Count);
  Count := 0;
  for Value in Self do
  begin
    Result[Count] := Value;
    Inc(Count);
  end;
end;

{**********************************************************************}
function TALDictionary<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := FDictionary.Count;
end;

{************************************************************************************}
function TALDictionary<TKey, TValue>.TValueCollection.GetEnumerator: TValueEnumerator;
begin
  Result := TValueEnumerator.Create(FDictionary);
end;

{****************************************************************************}
function TALDictionary<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
begin
  Result := ToArrayImpl(FDictionary.Count);
end;

{************************************************************}
constructor TALDictionary<TKey, TValue>.TKeyCollection.Create(
  const ADictionary: TALDictionary<TKey, TValue>);
begin
  inherited Create;
  FDictionary := ADictionary;
end;

{*************************************************************************************}
function TALDictionary<TKey, TValue>.TKeyCollection.DoGetEnumerator: TEnumerator<TKey>;
begin
  Result := GetEnumerator;
end;

{********************************************************************************************}
function TALDictionary<TKey, TValue>.TKeyCollection.ToArrayImpl(Count: Integer): TArray<TKey>;
var
  Value: TKey;
begin
  // We assume our caller has passed correct Count
  SetLength(Result, Count);
  Count := 0;
  for Value in Self do
  begin
    Result[Count] := Value;
    Inc(Count);
  end;
end;

{********************************************************************}
function TALDictionary<TKey, TValue>.TKeyCollection.GetCount: Integer;
begin
  Result := FDictionary.Count;
end;

{********************************************************************************}
function TALDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: TKeyEnumerator;
begin
  Result := TKeyEnumerator.Create(FDictionary);
end;

{************************************************************************}
function TALDictionary<TKey, TValue>.TKeyCollection.ToArray: TArray<TKey>;
begin
  Result := ToArrayImpl(FDictionary.Count);
end;

{*****************************************************************************************************}
procedure TALObjectDictionary<TKey,TValue>.KeyNotify(const Key: TKey; Action: TCollectionNotification);
begin
  inherited;
  if (Action = cnRemoved) and (doOwnsKeys in FOwnerships) then
    ALFreeAndNil(PObject(@Key)^); // >> will call disposeOF if necessary
end;

{***********************************************************************************************************}
procedure TALObjectDictionary<TKey,TValue>.ValueNotify(const Value: TValue; Action: TCollectionNotification);
begin
  inherited;
  if (Action = cnRemoved) and (doOwnsValues in FOwnerships) then
    ALFreeAndNil(PObject(@Value)^); // >> will call disposeOF if necessary
end;

{************************************************************************************}
constructor TALObjectDictionary<TKey,TValue>.Create(Ownerships: TDictionaryOwnerships;
  ACapacity: Integer = 0);
begin
  Create(Ownerships, ACapacity, nil);
end;

{************************************************************************************}
constructor TALObjectDictionary<TKey,TValue>.Create(Ownerships: TDictionaryOwnerships;
  const AComparer: IEqualityComparer<TKey>);
begin
  Create(Ownerships, 0, AComparer);
end;

{************************************************************************************}
constructor TALObjectDictionary<TKey,TValue>.Create(Ownerships: TDictionaryOwnerships;
  ACapacity: Integer; const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create(ACapacity, AComparer);
  if doOwnsKeys in Ownerships then
  begin
    if (TypeInfo(TKey) = nil) or (PTypeInfo(TypeInfo(TKey))^.Kind <> tkClass) then
      raise EInvalidCast.CreateRes(@SInvalidCast);
  end;

  if doOwnsValues in Ownerships then
  begin
    if (TypeInfo(TValue) = nil) or (PTypeInfo(TypeInfo(TValue))^.Kind <> tkClass) then
      raise EInvalidCast.CreateRes(@SInvalidCast);
  end;
  FOwnerships := Ownerships;
end;

end.
