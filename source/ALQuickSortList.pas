{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALQuickSortList
Version:      4.00

Description:  TALIntegerList or TALDoubleList that work exactly
              like TstringList but with integer or Double.

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

History :     16/06/2012: Add xe2 Support

Link :

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
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

Uses {$IF CompilerVersion >= 23} {Delphi XE2}
     System.Classes,
     System.RTLConsts,
     System.Generics.Defaults,
     System.Generics.Collections;
     {$ELSE}
     Classes;
     {$IFEND}

{$if CompilerVersion<=18.5}
//http://stackoverflow.com/questions/7630781/delphi-2007-and-xe2-using-nativeint
type
  NativeInt = Integer;
  NativeUInt = Cardinal;
{$ifend}     

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
  end;

  {--------------------------------------}
  {$IF CompilerVersion >= 23} {Delphi XE2}
  TALDictionary<TKey,TValue> = Class(TDictionary<TKey,TValue>)
  public
    procedure SetCapacity(ACapacity: Integer);
    function TryAdd(const Key: TKey; const Value: TValue): boolean;
  End;
  {$IFEND}

  {--------------------------------------}
  {$IF CompilerVersion >= 23} {Delphi XE2}
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
  {$IFEND}

resourcestring
  SALDuplicateItem = 'List does not allow duplicates';
  SALListCapacityError = 'List capacity out of bounds (%d)';
  SALListCountError = 'List count out of bounds (%d)';
  SALListIndexError = 'List index out of bounds (%d)';
  SALSortedListError = 'Operation not allowed on sorted list';

implementation

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     System.SysUtils,
     System.SysConst,
     System.TypInfo,
     {$IFEND}
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
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    ALMove(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
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
  if Index < FCount then
    ALMove(FList[Index + 1], FList[Index],
      (FCount - Index) * SizeOf(Pointer));
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
    Dispose(aPALIntegerListItem);
    raise;
  end;
end;

{***************************************************************************}
function TALIntegerList.CompareItems(const Index1, Index2: integer): Integer;
begin
  result := PALIntegerListItem(Get(Index1))^.FInteger - PALIntegerListItem(Get(Index2))^.FInteger;
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
  InsertObject(Index, index, nil);
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
    Dispose(aPALIntegerListItem);
    raise;
  end;
end;

{***********************************************************************}
procedure TALIntegerList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if OwnsObjects then PALIntegerListItem(Ptr).FObject.Free;
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

  {--------------------------------------------------}
  Function _CompareCardinal(D1,D2: Cardinal): Integer;
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
  InsertObject(Index, index, nil);
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
    Dispose(aPALCardinalListItem);
    raise;
  end;
end;

{************************************************************************}
procedure TALCardinalList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if OwnsObjects then PALCardinalListItem(Ptr).FObject.Free;
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
    Dispose(aPALInt64ListItem);
    raise;
  end;
end;

{*************************************************************************}
function TALInt64List.CompareItems(const Index1, Index2: integer): Integer;
var aInt64: Int64;
begin
  aInt64 := PALInt64ListItem(Get(Index1))^.FInt64 - PALInt64ListItem(Get(Index2))^.FInt64;
  if aInt64 < 0 then result := -1
  else if aInt64 > 0 then result := 1
  else result := 0;
end;

{*******************************************************************}
function TALInt64List.Find(item: Int64; var Index: Integer): Boolean;
var L, H, I, C: Integer;

  {--------------------------------------------}
  Function _CompareInt64(D1,D2: Int64): Integer;
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
    C := _CompareInt64(GetItem(I),item);
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
  InsertObject(Index, index, nil);
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
    Dispose(aPALInt64ListItem);
    raise;
  end;
end;

{*********************************************************************}
procedure TALInt64List.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if OwnsObjects then PALInt64ListItem(Ptr).FObject.Free;
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
    Dispose(aPALDoubleListItem);
    raise;
  end;
end;

{**************************************************************************}
function TALDoubleList.CompareItems(const Index1, Index2: integer): Integer;
var aDouble: Double;
begin
  aDouble := PALDoubleListItem(Get(Index1))^.FDouble - PALDoubleListItem(Get(Index2))^.FDouble;
  if adouble < 0 then result := -1
  else if adouble > 0 then result := 1
  else result := 0;
end;

{*********************************************************************}
function TALDoubleList.Find(item: Double; var Index: Integer): Boolean;
var L, H, I, C: Integer;

  {----------------------------------------------}
  Function _CompareDouble(D1,D2: Double): Integer;
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
    C := _CompareDouble(GetItem(I),item);
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
    if Result = FCount then Result := -1;
  end
  else if not Find(Item, Result) then Result := -1;
end;

{*****************************************************************}
procedure TALDoubleList.Insert(Index: Integer; const Item: Double);
begin
  InsertObject(Index, index, nil);
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
    Dispose(aPALDoubleListItem);
    raise;
  end;
end;

{**********************************************************************}
procedure TALDoubleList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if OwnsObjects then PALDoubleListItem(Ptr).FObject.Free;
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

{$IF CompilerVersion >= 23} {Delphi XE2}

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

{*****************************************************************************************************}
procedure TALObjectDictionary<TKey,TValue>.KeyNotify(const Key: TKey; Action: TCollectionNotification);
begin
  inherited;
  if (Action = cnRemoved) and (doOwnsKeys in FOwnerships) then
    {$IF CompilerVersion >= 25} {Delphi XE4}
    PObject(@Key)^.DisposeOf;
    {$else}
    PObject(@Key)^.free;
    {$ifend}
end;

{***********************************************************************************************************}
procedure TALObjectDictionary<TKey,TValue>.ValueNotify(const Value: TValue; Action: TCollectionNotification);
begin
  inherited;
  if (Action = cnRemoved) and (doOwnsValues in FOwnerships) then
    {$IF CompilerVersion >= 25} {Delphi XE4}
    PObject(@Value)^.DisposeOf;
    {$else}
    PObject(@Value)^.free;
    {$ifend}
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

{$IFEND}


end.