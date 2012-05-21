{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALQuickSortList
Version:      3.50

Description:  TALIntegerList or TALDoubleList that work exactly
              like TstringList but with integer or Double.

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

History :

Link :

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  voting on http://www.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALQuickSortList;

interface

uses Classes,
     ALAVLBinaryTree;

Type

  {----------------------------------------------------------------------------------}
  TALQuickSortListCompare = function(List: TObject; Index1, Index2: Integer): Integer;

  {-----------------------------------}
  TALBaseQuickSortList = class(TObject)
  private
    FList: PPointerList;
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
    property  List: PPointerList read FList;
  public
    Constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
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
    function  GetItem(Index: Integer): Integer;
    procedure SetItem(Index: Integer; const Item: Integer);
    function  GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const item: integer; AObject: TObject);
    function  CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    function  IndexOf(Item: Integer): Integer;
    function  IndexOfObject(AObject: TObject): Integer;
    function  Add(const Item: integer): Integer;
    Function  AddObject(const Item: integer; AObject: TObject): Integer;
    function  Find(item: Integer; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const item: integer);
    procedure InsertObject(Index: Integer; const item: integer; AObject: TObject);
    property  Items[Index: Integer]: Integer read GetItem write SetItem; default;
    property  Objects[Index: Integer]: TObject read GetObject write PutObject;
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
    function  GetItem(Index: Integer): Cardinal;
    procedure SetItem(Index: Integer; const Item: Cardinal);
    function  GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const item: Cardinal; AObject: TObject);
    function  CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    function  IndexOf(Item: Cardinal): Integer;
    function  IndexOfObject(AObject: TObject): Integer;
    function  Add(const Item: Cardinal): Integer;
    Function  AddObject(const Item: Cardinal; AObject: TObject): Integer;
    function  Find(item: Cardinal; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const item: Cardinal);
    procedure InsertObject(Index: Integer; const item: Cardinal; AObject: TObject);
    property  Items[Index: Integer]: Cardinal read GetItem write SetItem; default;
    property  Objects[Index: Integer]: TObject read GetObject write PutObject;
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
    function  GetItem(Index: Integer): Int64;
    procedure SetItem(Index: Integer; const Item: Int64);
    function  GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const item: Int64; AObject: TObject);
    function  CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    function  IndexOf(Item: Int64): Integer;
    function  IndexOfObject(AObject: TObject): Integer;
    function  Add(const Item: Int64): Integer;
    Function  AddObject(const Item: Int64; AObject: TObject): Integer;
    function  Find(item: Int64; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const item: Int64);
    procedure InsertObject(Index: Integer; const item: Int64; AObject: TObject);
    property  Items[Index: Integer]: Int64 read GetItem write SetItem; default;
    property  Objects[Index: Integer]: TObject read GetObject write PutObject;
  end;

  {-------------------------------------}
  PALDoubleListItem = ^TALDoubleListItem;
  TALDoubleListItem = record
    FDouble: Double;
    FObject: TObject;
  end;

  {------------------------------------------}
  TALDoubleList = class(TALBaseQuickSortList)
  private
    function  GetItem(Index: Integer): Double;
    procedure SetItem(Index: Integer; const Item: Double);
    function  GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const item: Double; AObject: TObject);
    function  CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    function  IndexOf(Item: Double): Integer;
    function  IndexOfObject(AObject: TObject): Integer;
    function  Add(const Item: Double): Integer;
    Function  AddObject(const Item: Double; AObject: TObject): Integer;
    function  Find(item: Double; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const item: Double);
    procedure InsertObject(Index: Integer; const item: Double; AObject: TObject);
    property  Items[Index: Integer]: Double read GetItem write SetItem; default;
    property  Objects[Index: Integer]: TObject read GetObject write PutObject;
  end;

  {-----------------------------}
  TALBaseAVLList = class(TObject)
  private
    FList: PPointerList;
    FCount: Integer;
    FCapacity: Integer;
    FDuplicates: TDuplicates;
  protected
    function  Get(Index: Integer): Pointer;
    procedure Grow;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    procedure InsertItem(Index: Integer; Item: Pointer);
    property  List: PPointerList read FList;
  public
    Constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    function  Expand: TALBaseAVLList;
    property  Capacity: Integer read FCapacity write SetCapacity;
    property  Count: Integer read FCount write SetCount;
    property  Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

  {-----------------------------------------------------------------}
  TALAVLInt64ListBinaryTreeNode = class(TALInt64KeyAVLBinaryTreeNode)
  Private
  Protected
  Public
    Obj: Tobject; // Object
    Idx: integer; // Index in the NodeList
    Constructor Create; Override;
  end;

  {-------------------------------------}
  TALInt64AVLList = class(TALBaseAVLList)
  private
    FAVLBinTree: TALInt64KeyAVLBinaryTree;
    function  GetItem(Index: Integer): Int64;
    function  GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure InsertItem(Index: Integer; const item: Int64; AObject: TObject);
  public
    Constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    function  IndexOf(Item: Int64): Integer;
    function  IndexOfObject(AObject: TObject): Integer;
    function  Add(const Item: Int64): Integer;
    Function  AddObject(const Item: Int64; AObject: TObject): Integer;
    function  Find(item: Int64; var Index: Integer): Boolean;
    property  Items[Index: Integer]: Int64 read GetItem; default;
    property  Objects[Index: Integer]: TObject read GetObject write PutObject;
  end;

implementation

uses SYSutils;

{ TQuickSortList }

{***********************************************************************************}
function AlBaseQuickSortListCompare(List: TObject; Index1, Index2: Integer): Integer;
Begin
  result := TALBaseQuickSortList(List).CompareItems(Index1, Index2);
end;

{**************************************}
constructor TALBaseQuickSortList.Create;
begin
  FList:= nil;
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
  if FCount = FCapacity then Grow;
  if Index < FCount then System.Move(FList^[Index],
                                     FList^[Index + 1],
                                     (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  Inc(FCount);
  if Item <> nil then
    Notify(Item, lnAdded);
end;

{********************************************************************}
procedure TALBaseQuickSortList.ExchangeItems(Index1, Index2: Integer);
var Item: Pointer;
begin
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

{****************************************************}
procedure TALBaseQuickSortList.Delete(Index: Integer);
var Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
  Temp := Get(Index);
  Dec(FCount);
  if Index < FCount then System.Move(FList^[Index + 1],
                                     FList^[Index],
                                     (FCount - Index) * SizeOf(Pointer));
  if Temp <> nil then Notify(Temp, lnDeleted);
end;

{***************************************************************}
procedure TALBaseQuickSortList.Exchange(Index1, Index2: Integer);
begin
  {Warning:	Do not call Exchange on a sorted list except to swap two identical
   strings with different associated objects. Exchange does not check whether
   the list is sorted, and can destroy the sort order of a sorted list.}
  if (Index1 < 0) or (Index1 >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index1]));
  if (Index2 < 0) or (Index2 >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index2]));
  ExchangeItems(Index1, Index2);
end;

{*******************************************************************}
procedure TALBaseQuickSortList.Insert(Index: Integer; Item: Pointer);
begin
  if Sorted then raise EListError.Create('Operation not allowed on sorted list');
  if (Index < 0) or (Index > FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
  InsertItem(Index, Item);
end;

{****************************************************************}
procedure TALBaseQuickSortList.Put(Index: Integer; Item: Pointer);
var Temp: Pointer;
begin
  if Sorted then raise EListError.Create('Operation not allowed on sorted list');
  if (Index < 0) or (Index >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
  if Item <> FList^[Index] then begin
    Temp := FList^[Index];
    FList^[Index] := Item;
    if Temp <> nil then Notify(Temp, lnDeleted);
    if Item <> nil then Notify(Item, lnAdded);
  end;
end;

{*********************************************************}
function TALBaseQuickSortList.Get(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
  Result := FList^[Index];
end;

{*********************************************************}
function TALBaseQuickSortList.Expand: TALBaseQuickSortList;
begin
  if FCount = FCapacity then Grow;
  Result := Self;
end;

{**********************************}
procedure TALBaseQuickSortList.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4
  else if FCapacity > 8 then Delta := 16
  else Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

{***************************************************************}
procedure TALBaseQuickSortList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then raise EListError.Create(format('List capacity out of bounds (%d)', [NewCapacity]));
  if NewCapacity <> FCapacity then begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

{*********************************************************}
procedure TALBaseQuickSortList.SetCount(NewCount: Integer);
var I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then raise EListError.Create(format('List count out of bounds (%d)', [NewCount]));
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else for I := FCount - 1 downto NewCount do Delete(I);
  FCount := NewCount;
end;

{*****************************************************************************}
procedure TALBaseQuickSortList.Notify(Ptr: Pointer; Action: TListNotification);
begin
//virtual
end;

{*******************************************************}
procedure TALBaseQuickSortList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

{*****************************************************************************************}
procedure TALBaseQuickSortList.QuickSort(L, R: Integer; SCompare: TALQuickSortListCompare);
var I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then begin
        ExchangeItems(I, J);
        if P = I then P := J
        else if P = J then P := I;
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
  if not Sorted and (FCount > 1) then QuickSort(0, FCount - 1, Compare);
end;

{**********************************}
procedure TALBaseQuickSortList.Sort;
begin
  CustomSort(AlBaseQuickSortListCompare);
end;

{********************************************************************************}
function TALBaseQuickSortList.CompareItems(const Index1, Index2: Integer): Integer;
begin
  Result := 0;
end;

{ TALIntegerList }

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
      dupError: raise EListError.Create('List does not allow duplicates');
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
  if Action = lnDeleted then dispose(ptr);
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
  if (Index < 0) or (Index >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
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
  if (Index < 0) or (Index >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
  PALIntegerListItem(Get(index))^.FObject := AObject;
end;

{ TALCardinalList }

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
      dupError: raise EListError.Create('List does not allow duplicates');
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
var aInt: Integer;
begin
  aInt := PALCardinalListItem(Get(Index1))^.FCardinal - PALCardinalListItem(Get(Index2))^.FCardinal;
  if aInt < 0 then result := -1
  else if aInt > 0 then result := 1
  else result := 0;
end;

{*************************************************************************}
function TALCardinalList.Find(item: Cardinal; var Index: Integer): Boolean;
var L, H, I, C: Integer;

  {---------------------------------------------------------}
  Function internalCompareCardinal(D1,D2: Cardinal): Integer;
  Var aInt: Integer;
  Begin
    aInt := D1 - D2;
    if aInt < 0 then result := -1
    else if aInt > 0 then result := 1
    else result := 0;
  end;

begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := internalCompareCardinal(GetItem(I),item);
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
  if Action = lnDeleted then dispose(ptr);
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
  if (Index < 0) or (Index >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
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
  if (Index < 0) or (Index >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
  PALCardinalListItem(Get(index))^.FObject := AObject;
end;

{ TALInt64List }

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
      dupError: raise EListError.Create('List does not allow duplicates');
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

  {---------------------------------------------------}
  Function internalCompareInt64(D1,D2: Int64): Integer;
  Var aInt64: Int64;
  Begin
    aInt64 := D1 - D2;
    if aInt64 < 0 then result := -1
    else if aInt64 > 0 then result := 1
    else result := 0;
  end;

begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := internalCompareInt64(GetItem(I),item);
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
  if Action = lnDeleted then dispose(ptr);
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
  if (Index < 0) or (Index >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
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
  if (Index < 0) or (Index >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
  PALInt64ListItem(Get(index))^.FObject := AObject;
end;

{ TALDoubleList }

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
      dupError: raise EListError.Create('List does not allow duplicates');
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

  {-----------------------------------------------------}
  Function internalCompareDouble(D1,D2: Double): Integer;
  Var aDouble: Double;
  Begin
    aDouble := D1 - D2;
    if adouble < 0 then result := -1
    else if adouble > 0 then result := 1
    else result := 0;
  end;

begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := internalCompareDouble(GetItem(I),item);
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
  if Action = lnDeleted then dispose(ptr);
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
  if (Index < 0) or (Index >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
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
  if (Index < 0) or (Index >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
  PALDoubleListItem(Get(index))^.FObject := AObject;
end;

{ TALBaseAVLList }

{********************************}
constructor TALBaseAVLList.Create;
begin
  FList:= nil;
  FCount:= 0;
  FCapacity:= 0;
  FDuplicates := dupIgnore;
end;

{********************************}
destructor TALBaseAVLList.Destroy;
begin
  Clear;
end;

{*****************************}
procedure TALBaseAVLList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

{*****************************************************************}
procedure TALBaseAVLList.InsertItem(Index: Integer; Item: Pointer);
begin
  if FCount = FCapacity then Grow;
  if Index < FCount then System.Move(FList^[Index],
                                     FList^[Index + 1],
                                     (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  Inc(FCount);
end;

{**********************************************}
procedure TALBaseAVLList.Delete(Index: Integer);
var Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
  Temp := Get(Index);
  Dec(FCount);
  if Index < FCount then System.Move(FList^[Index + 1],
                                     FList^[Index],
                                     (FCount - Index) * SizeOf(Pointer));
  if Temp <> nil then dispose(Temp);
end;

{***************************************************}
function TALBaseAVLList.Get(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
  Result := FList^[Index];
end;

{*********************************************}
function TALBaseAVLList.Expand: TALBaseAVLList;
begin
  if FCount = FCapacity then Grow;
  Result := Self;
end;

{****************************}
procedure TALBaseAVLList.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4
  else if FCapacity > 8 then Delta := 16
  else Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

{*********************************************************}
procedure TALBaseAVLList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then raise EListError.Create(format('List capacity out of bounds (%d)', [NewCapacity]));
  if NewCapacity <> FCapacity then begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

{***************************************************}
procedure TALBaseAVLList.SetCount(NewCount: Integer);
var I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then raise EListError.Create(format('List count out of bounds (%d)', [NewCount]));
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else for I := FCount - 1 downto NewCount do Delete(I);
  FCount := NewCount;
end;

{ TALAVLInt64ListBinaryTreeNode }

{***********************************************}
constructor TALAVLInt64ListBinaryTreeNode.Create;
begin
  inherited;
  Obj := nil;
  idx := -1;
end;

{ TALInt64AVLList }

{*********************************}
constructor TALInt64AVLList.Create;
begin
  inherited;
  FAVLBinTree:= TALInt64KeyAVLBinaryTree.Create;
end;

{*********************************}
destructor TALInt64AVLList.Destroy;
begin
  inherited;
  FAVLBinTree.free;
end;

{******************************}
procedure TALInt64AVLList.Clear;
begin
  FAVLBinTree.Clear;
  inherited;
end;

{*******************************************************}
function TALInt64AVLList.Add(const Item: Int64): Integer;
begin
  Result := AddObject(Item, nil);
end;

{*******************************************************************************}
function TALInt64AVLList.AddObject(const Item: Int64; AObject: TObject): Integer;
begin
  Result := FCount;
  InsertItem(Result, Item, AObject);
end;

{****************************************************************************************}
procedure TALInt64AVLList.InsertItem(Index: Integer; const item: Int64; AObject: TObject);
Var aPALInt64ListItem: PALInt64ListItem;
    aNode: TALAVLInt64ListBinaryTreeNode;
    i: integer;
begin

  aNode := TALAVLInt64ListBinaryTreeNode.Create;
  aNode.ID := item;
  aNode.Obj := AObject;
  aNode.Idx := Index;
  if not FAVLBinTree.AddNode(aNode) then begin
    aNode.free;
    case Duplicates of
      dupIgnore: Exit;
      else raise EListError.Create('List does not allow duplicates');
    end;
  end;

  New(aPALInt64ListItem);
  aPALInt64ListItem^.FInt64 := item;
  aPALInt64ListItem^.FObject := aNode;
  try
    inherited InsertItem(index,aPALInt64ListItem);
  except
    Dispose(aPALInt64ListItem);
    FAVLBinTree.DeleteNode(item);
    raise;
  end;

  for i := Index + 1 to Count - 1 do
    inc(TALAVLInt64ListBinaryTreeNode(objects[i]).Idx);

end;

{*******************************************************************}
function TALInt64AVLList.Find(item: Int64; var Index: Integer): Boolean;
var aNode: TALInt64KeyAVLBinaryTreeNode;
begin
  aNode := FAVLBinTree.FindNode(item);
  result := assigned(aNode);
  if result then Index := TALAVLInt64ListBinaryTreeNode(aNode).Idx;
end;

{******************************************************}
function TALInt64AVLList.GetItem(Index: Integer): Int64;
begin
  Result := PALInt64ListItem(Get(index))^.FInt64
end;

{*****************************************************}
function TALInt64AVLList.IndexOf(Item: Int64): Integer;
begin
  if not Find(Item, Result) then Result := -1;
end;

{***********************************************}
procedure TALInt64AVLList.Delete(Index: Integer);
var i: integer;
begin

  FAVLBinTree.DeleteNode(GetItem(Index));

  for i := Index + 1 to Count - 1 do
    dec(TALAVLInt64ListBinaryTreeNode(objects[i]).Idx);

  inherited Delete(Index);

end;

{**********************************************************}
function TALInt64AVLList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
  Result :=  TALAVLInt64ListBinaryTreeNode(PALInt64ListItem(Get(index))^.FObject).Obj;
end;

{****************************************************************}
function TALInt64AVLList.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to Count - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

{*****************************************************************}
procedure TALInt64AVLList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then raise EListError.Create(format('List index out of bounds (%d)', [Index]));
  TALAVLInt64ListBinaryTreeNode(PALInt64ListItem(Get(index))^.FObject).Obj := AObject;
end;

end.