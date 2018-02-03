{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

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
{ The Original Code is ArraySet.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclArraySets;

{$I jcl.inc}

interface

uses
  {$IFDEF HAS_UNITSCOPE}
  System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF SUPPORTS_GENERICS}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf, JclArrayLists, JclSynch;

type
  TJclIntfArraySet = class(TJclIntfArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclIntfContainer, IJclIntfFlatContainer, IJclIntfEqualityComparer, IJclIntfComparer,
    IJclIntfCollection, IJclIntfList, IJclIntfArray, IJclIntfSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    function BinarySearch(const AInterface: IInterface): Integer;
  public
    { IJclIntfCollection }
    function Add(const AInterface: IInterface): Boolean;
    function AddAll(const ACollection: IJclIntfCollection): Boolean;
    function Contains(const AInterface: IInterface): Boolean;
    { IJclIntfList }
    function Insert(Index: Integer; const AInterface: IInterface): Boolean;
    { IJclIntfSet }
    procedure Intersect(const ACollection: IJclIntfCollection);
    procedure Subtract(const ACollection: IJclIntfCollection);
    procedure Union(const ACollection: IJclIntfCollection);
  end;

  TJclAnsiStrArraySet = class(TJclAnsiStrArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclAnsiStrContainer, IJclAnsiStrFlatContainer, IJclAnsiStrEqualityComparer, IJclAnsiStrComparer, IJclStrBaseContainer,
    IJclAnsiStrCollection, IJclAnsiStrList, IJclAnsiStrArray, IJclAnsiStrSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    function BinarySearch(const AString: AnsiString): Integer;
  public
    { IJclAnsiStrCollection }
    function Add(const AString: AnsiString): Boolean; override;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Contains(const AString: AnsiString): Boolean; override;
    { IJclAnsiStrList }
    function Insert(Index: Integer; const AString: AnsiString): Boolean;
    { IJclAnsiStrSet }
    procedure Intersect(const ACollection: IJclAnsiStrCollection);
    procedure Subtract(const ACollection: IJclAnsiStrCollection);
    procedure Union(const ACollection: IJclAnsiStrCollection);
  end;

  TJclWideStrArraySet = class(TJclWideStrArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclWideStrContainer, IJclWideStrFlatContainer, IJclWideStrEqualityComparer, IJclWideStrComparer, IJclStrBaseContainer,
    IJclWideStrCollection, IJclWideStrList, IJclWideStrArray, IJclWideStrSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    function BinarySearch(const AString: WideString): Integer;
  public
    { IJclWideStrCollection }
    function Add(const AString: WideString): Boolean; override;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Contains(const AString: WideString): Boolean; override;
    { IJclWideStrList }
    function Insert(Index: Integer; const AString: WideString): Boolean;
    { IJclWideStrSet }
    procedure Intersect(const ACollection: IJclWideStrCollection);
    procedure Subtract(const ACollection: IJclWideStrCollection);
    procedure Union(const ACollection: IJclWideStrCollection);
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrArraySet = class(TJclUnicodeStrArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclUnicodeStrContainer, IJclUnicodeStrFlatContainer, IJclUnicodeStrEqualityComparer, IJclUnicodeStrComparer, IJclStrBaseContainer,
    IJclUnicodeStrCollection, IJclUnicodeStrList, IJclUnicodeStrArray, IJclUnicodeStrSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    function BinarySearch(const AString: UnicodeString): Integer;
  public
    { IJclUnicodeStrCollection }
    function Add(const AString: UnicodeString): Boolean; override;
    function AddAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function Contains(const AString: UnicodeString): Boolean; override;
    { IJclUnicodeStrList }
    function Insert(Index: Integer; const AString: UnicodeString): Boolean;
    { IJclUnicodeStrSet }
    procedure Intersect(const ACollection: IJclUnicodeStrCollection);
    procedure Subtract(const ACollection: IJclUnicodeStrCollection);
    procedure Union(const ACollection: IJclUnicodeStrCollection);
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrArraySet = TJclAnsiStrArraySet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrArraySet = TJclWideStrArraySet;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrArraySet = TJclUnicodeStrArraySet;
  {$ENDIF CONTAINER_UNICODESTR}

  TJclSingleArraySet = class(TJclSingleArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclSingleContainer, IJclSingleFlatContainer, IJclSingleEqualityComparer, IJclSingleComparer,
    IJclSingleCollection, IJclSingleList, IJclSingleArray, IJclSingleSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    function BinarySearch(const AValue: Single): Integer;
  public
    { IJclSingleCollection }
    function Add(const AValue: Single): Boolean;
    function AddAll(const ACollection: IJclSingleCollection): Boolean;
    function Contains(const AValue: Single): Boolean;
    { IJclSingleList }
    function Insert(Index: Integer; const AValue: Single): Boolean;
    { IJclSingleSet }
    procedure Intersect(const ACollection: IJclSingleCollection);
    procedure Subtract(const ACollection: IJclSingleCollection);
    procedure Union(const ACollection: IJclSingleCollection);
  end;

  TJclDoubleArraySet = class(TJclDoubleArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclDoubleContainer, IJclDoubleFlatContainer, IJclDoubleEqualityComparer, IJclDoubleComparer,
    IJclDoubleCollection, IJclDoubleList, IJclDoubleArray, IJclDoubleSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    function BinarySearch(const AValue: Double): Integer;
  public
    { IJclDoubleCollection }
    function Add(const AValue: Double): Boolean;
    function AddAll(const ACollection: IJclDoubleCollection): Boolean;
    function Contains(const AValue: Double): Boolean;
    { IJclDoubleList }
    function Insert(Index: Integer; const AValue: Double): Boolean;
    { IJclDoubleSet }
    procedure Intersect(const ACollection: IJclDoubleCollection);
    procedure Subtract(const ACollection: IJclDoubleCollection);
    procedure Union(const ACollection: IJclDoubleCollection);
  end;

  TJclExtendedArraySet = class(TJclExtendedArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclExtendedContainer, IJclExtendedFlatContainer, IJclExtendedEqualityComparer, IJclExtendedComparer,
    IJclExtendedCollection, IJclExtendedList, IJclExtendedArray, IJclExtendedSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    function BinarySearch(const AValue: Extended): Integer;
  public
    { IJclExtendedCollection }
    function Add(const AValue: Extended): Boolean;
    function AddAll(const ACollection: IJclExtendedCollection): Boolean;
    function Contains(const AValue: Extended): Boolean;
    { IJclExtendedList }
    function Insert(Index: Integer; const AValue: Extended): Boolean;
    { IJclExtendedSet }
    procedure Intersect(const ACollection: IJclExtendedCollection);
    procedure Subtract(const ACollection: IJclExtendedCollection);
    procedure Union(const ACollection: IJclExtendedCollection);
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatArraySet = TJclSingleArraySet;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatArraySet = TJclDoubleArraySet;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatArraySet = TJclExtendedArraySet;
  {$ENDIF MATH_EXTENDED_PRECISION}

  TJclIntegerArraySet = class(TJclIntegerArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclIntegerContainer, IJclIntegerFlatContainer, IJclIntegerEqualityComparer, IJclIntegerComparer,
    IJclIntegerCollection, IJclIntegerList, IJclIntegerArray, IJclIntegerSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    function BinarySearch(AValue: Integer): Integer;
  public
    { IJclIntegerCollection }
    function Add(AValue: Integer): Boolean;
    function AddAll(const ACollection: IJclIntegerCollection): Boolean;
    function Contains(AValue: Integer): Boolean;
    { IJclIntegerList }
    function Insert(Index: Integer; AValue: Integer): Boolean;
    { IJclIntegerSet }
    procedure Intersect(const ACollection: IJclIntegerCollection);
    procedure Subtract(const ACollection: IJclIntegerCollection);
    procedure Union(const ACollection: IJclIntegerCollection);
  end;

  TJclCardinalArraySet = class(TJclCardinalArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclCardinalContainer, IJclCardinalFlatContainer, IJclCardinalEqualityComparer, IJclCardinalComparer,
    IJclCardinalCollection, IJclCardinalList, IJclCardinalArray, IJclCardinalSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    function BinarySearch(AValue: Cardinal): Integer;
  public
    { IJclCardinalCollection }
    function Add(AValue: Cardinal): Boolean;
    function AddAll(const ACollection: IJclCardinalCollection): Boolean;
    function Contains(AValue: Cardinal): Boolean;
    { IJclCardinalList }
    function Insert(Index: Integer; AValue: Cardinal): Boolean;
    { IJclCardinalSet }
    procedure Intersect(const ACollection: IJclCardinalCollection);
    procedure Subtract(const ACollection: IJclCardinalCollection);
    procedure Union(const ACollection: IJclCardinalCollection);
  end;

  TJclInt64ArraySet = class(TJclInt64ArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclInt64Container, IJclInt64FlatContainer, IJclInt64EqualityComparer, IJclInt64Comparer,
    IJclInt64Collection, IJclInt64List, IJclInt64Array, IJclInt64Set)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    function BinarySearch(const AValue: Int64): Integer;
  public
    { IJclInt64Collection }
    function Add(const AValue: Int64): Boolean;
    function AddAll(const ACollection: IJclInt64Collection): Boolean;
    function Contains(const AValue: Int64): Boolean;
    { IJclInt64List }
    function Insert(Index: Integer; const AValue: Int64): Boolean;
    { IJclInt64Set }
    procedure Intersect(const ACollection: IJclInt64Collection);
    procedure Subtract(const ACollection: IJclInt64Collection);
    procedure Union(const ACollection: IJclInt64Collection);
  end;

  TJclPtrArraySet = class(TJclPtrArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclPtrContainer, IJclPtrFlatContainer, IJclPtrEqualityComparer, IJclPtrComparer,
    IJclPtrCollection, IJclPtrList, IJclPtrArray, IJclPtrSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    function BinarySearch(APtr: Pointer): Integer;
  public
    { IJclPtrCollection }
    function Add(APtr: Pointer): Boolean;
    function AddAll(const ACollection: IJclPtrCollection): Boolean;
    function Contains(APtr: Pointer): Boolean;
    { IJclPtrList }
    function Insert(Index: Integer; APtr: Pointer): Boolean;
    { IJclPtrSet }
    procedure Intersect(const ACollection: IJclPtrCollection);
    procedure Subtract(const ACollection: IJclPtrCollection);
    procedure Union(const ACollection: IJclPtrCollection);
  end;

  TJclArraySet = class(TJclArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclContainer, IJclFlatContainer, IJclEqualityComparer, IJclComparer, IJclObjectOwner,
    IJclCollection, IJclList, IJclArray, IJclSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    function BinarySearch(AObject: TObject): Integer;
  public
    { IJclCollection }
    function Add(AObject: TObject): Boolean;
    function AddAll(const ACollection: IJclCollection): Boolean;
    function Contains(AObject: TObject): Boolean;
    { IJclList }
    function Insert(Index: Integer; AObject: TObject): Boolean;
    { IJclSet }
    procedure Intersect(const ACollection: IJclCollection);
    procedure Subtract(const ACollection: IJclCollection);
    procedure Union(const ACollection: IJclCollection);
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN

  TJclArraySet<T> = class(TJclArrayList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclContainer<T>, IJclFlatContainer<T>, IJclEqualityComparer<T>, IJclComparer<T>, IJclItemOwner<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  private
    function BinarySearch(const AItem: T): Integer;
  public
    { IJclCollection<T> }
    function Add(const AItem: T): Boolean;
    function AddAll(const ACollection: IJclCollection<T>): Boolean;
    function Contains(const AItem: T): Boolean;
    { IJclList<T> }
    function Insert(Index: Integer; const AItem: T): Boolean;
    { IJclSet<T> }
    procedure Intersect(const ACollection: IJclCollection<T>);
    procedure Subtract(const ACollection: IJclCollection<T>);
    procedure Union(const ACollection: IJclCollection<T>);
  end;

  // E = External helper to compare items
  TJclArraySetE<T> = class(TJclArraySet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer, IJclContainer<T>, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  private
    FComparer: IJclComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AComparer: IJclComparer<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const AComparer: IJclComparer<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;
    { IJclEqualityComparer<T> }
    function ItemsCompare(const A, B: T): Integer; override;
    { IJclComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    property Comparer: IJclComparer<T> read FComparer write FComparer;
  end;

  // F = Function to compare items
  TJclArraySetF<T> = class(TJclArraySet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer, IJclContainer<T>, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACompare: TCompare<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const ACompare: TCompare<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;
  end;

  // I = Items can compare themselves to others
  TJclArraySetI<T: IComparable<T>> = class(TJclArraySet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer, IJclContainer<T>, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    { IJclEqualityComparer<T> }
    function ItemsCompare(const A, B: T): Integer; override;
    { IJclComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
  end;

  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

{$IFDEF BCB}
{$IFDEF WIN64}
  {$HPPEMIT '#ifdef MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT ' #undef MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT ' #define JclArraySets_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT '#endif'}

  {$HPPEMIT END '#ifdef JclArraySets_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #define MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #undef JclArraySets_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END '#endif'}
{$ENDIF WIN64}
{$ENDIF BCB}

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
  System.SysUtils;
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils;
  {$ENDIF ~HAS_UNITSCOPE}

//=== { TJclIntfArraySet } ====================================================

function TJclIntfArraySet.Add(const AInterface: IInterface): Boolean;
var
  Idx: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if Result then
    begin
      Idx := BinarySearch(AInterface);
      if Idx >= 0 then
        Result := not ItemsEqual(GetObject(Idx), AInterface) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AInterface);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArraySet.AddAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArraySet.BinarySearch(const AInterface: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetObject(CompPos), AInterface);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArraySet.Contains(const AInterface: IInterface): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AInterface);
    if Idx >= 0 then
      Result := ItemsEqual(GetObject(Idx), AInterface)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArraySet.Insert(Index: Integer; const AInterface: IInterface): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfArraySet.Intersect(const ACollection: IJclIntfCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclIntfArraySet.Subtract(const ACollection: IJclIntfCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclIntfArraySet.Union(const ACollection: IJclIntfCollection);
begin
  AddAll(ACollection);
end;

function TJclIntfArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclAnsiStrArraySet } ====================================================

function TJclAnsiStrArraySet.Add(const AString: AnsiString): Boolean;
var
  Idx: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if Result then
    begin
      Idx := BinarySearch(AString);
      if Idx >= 0 then
        Result := not ItemsEqual(GetString(Idx), AString) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AString);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrArraySet.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrArraySet.BinarySearch(const AString: AnsiString): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetString(CompPos), AString);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrArraySet.Contains(const AString: AnsiString): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AString);
    if Idx >= 0 then
      Result := ItemsEqual(GetString(Idx), AString)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrArraySet.Insert(Index: Integer; const AString: AnsiString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrArraySet.Intersect(const ACollection: IJclAnsiStrCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclAnsiStrArraySet.Subtract(const ACollection: IJclAnsiStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclAnsiStrArraySet.Union(const ACollection: IJclAnsiStrCollection);
begin
  AddAll(ACollection);
end;

function TJclAnsiStrArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclWideStrArraySet } ====================================================

function TJclWideStrArraySet.Add(const AString: WideString): Boolean;
var
  Idx: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if Result then
    begin
      Idx := BinarySearch(AString);
      if Idx >= 0 then
        Result := not ItemsEqual(GetString(Idx), AString) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AString);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrArraySet.AddAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrArraySet.BinarySearch(const AString: WideString): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetString(CompPos), AString);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrArraySet.Contains(const AString: WideString): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AString);
    if Idx >= 0 then
      Result := ItemsEqual(GetString(Idx), AString)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrArraySet.Insert(Index: Integer; const AString: WideString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrArraySet.Intersect(const ACollection: IJclWideStrCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclWideStrArraySet.Subtract(const ACollection: IJclWideStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclWideStrArraySet.Union(const ACollection: IJclWideStrCollection);
begin
  AddAll(ACollection);
end;

function TJclWideStrArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrArraySet } ====================================================

function TJclUnicodeStrArraySet.Add(const AString: UnicodeString): Boolean;
var
  Idx: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if Result then
    begin
      Idx := BinarySearch(AString);
      if Idx >= 0 then
        Result := not ItemsEqual(GetString(Idx), AString) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AString);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrArraySet.AddAll(const ACollection: IJclUnicodeStrCollection): Boolean;
var
  It: IJclUnicodeStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrArraySet.BinarySearch(const AString: UnicodeString): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetString(CompPos), AString);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrArraySet.Contains(const AString: UnicodeString): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AString);
    if Idx >= 0 then
      Result := ItemsEqual(GetString(Idx), AString)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrArraySet.Insert(Index: Integer; const AString: UnicodeString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclUnicodeStrArraySet.Intersect(const ACollection: IJclUnicodeStrCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclUnicodeStrArraySet.Subtract(const ACollection: IJclUnicodeStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclUnicodeStrArraySet.Union(const ACollection: IJclUnicodeStrCollection);
begin
  AddAll(ACollection);
end;

function TJclUnicodeStrArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

{$ENDIF SUPPORTS_UNICODE_STRING}

//=== { TJclSingleArraySet } ====================================================

function TJclSingleArraySet.Add(const AValue: Single): Boolean;
var
  Idx: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if Result then
    begin
      Idx := BinarySearch(AValue);
      if Idx >= 0 then
        Result := not ItemsEqual(GetValue(Idx), AValue) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AValue);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleArraySet.AddAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleArraySet.BinarySearch(const AValue: Single): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetValue(CompPos), AValue);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleArraySet.Contains(const AValue: Single): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AValue);
    if Idx >= 0 then
      Result := ItemsEqual(GetValue(Idx), AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleArraySet.Insert(Index: Integer; const AValue: Single): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclSingleArraySet.Intersect(const ACollection: IJclSingleCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclSingleArraySet.Subtract(const ACollection: IJclSingleCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclSingleArraySet.Union(const ACollection: IJclSingleCollection);
begin
  AddAll(ACollection);
end;

function TJclSingleArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclDoubleArraySet } ====================================================

function TJclDoubleArraySet.Add(const AValue: Double): Boolean;
var
  Idx: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if Result then
    begin
      Idx := BinarySearch(AValue);
      if Idx >= 0 then
        Result := not ItemsEqual(GetValue(Idx), AValue) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AValue);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleArraySet.AddAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleArraySet.BinarySearch(const AValue: Double): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetValue(CompPos), AValue);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleArraySet.Contains(const AValue: Double): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AValue);
    if Idx >= 0 then
      Result := ItemsEqual(GetValue(Idx), AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleArraySet.Insert(Index: Integer; const AValue: Double): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclDoubleArraySet.Intersect(const ACollection: IJclDoubleCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclDoubleArraySet.Subtract(const ACollection: IJclDoubleCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclDoubleArraySet.Union(const ACollection: IJclDoubleCollection);
begin
  AddAll(ACollection);
end;

function TJclDoubleArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclExtendedArraySet } ====================================================

function TJclExtendedArraySet.Add(const AValue: Extended): Boolean;
var
  Idx: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if Result then
    begin
      Idx := BinarySearch(AValue);
      if Idx >= 0 then
        Result := not ItemsEqual(GetValue(Idx), AValue) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AValue);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedArraySet.AddAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedArraySet.BinarySearch(const AValue: Extended): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetValue(CompPos), AValue);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedArraySet.Contains(const AValue: Extended): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AValue);
    if Idx >= 0 then
      Result := ItemsEqual(GetValue(Idx), AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedArraySet.Insert(Index: Integer; const AValue: Extended): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclExtendedArraySet.Intersect(const ACollection: IJclExtendedCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclExtendedArraySet.Subtract(const ACollection: IJclExtendedCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclExtendedArraySet.Union(const ACollection: IJclExtendedCollection);
begin
  AddAll(ACollection);
end;

function TJclExtendedArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclIntegerArraySet } ====================================================

function TJclIntegerArraySet.Add(AValue: Integer): Boolean;
var
  Idx: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if Result then
    begin
      Idx := BinarySearch(AValue);
      if Idx >= 0 then
        Result := not ItemsEqual(GetValue(Idx), AValue) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AValue);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerArraySet.AddAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerArraySet.BinarySearch(AValue: Integer): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetValue(CompPos), AValue);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerArraySet.Contains(AValue: Integer): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AValue);
    if Idx >= 0 then
      Result := ItemsEqual(GetValue(Idx), AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerArraySet.Insert(Index: Integer; AValue: Integer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntegerArraySet.Intersect(const ACollection: IJclIntegerCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclIntegerArraySet.Subtract(const ACollection: IJclIntegerCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclIntegerArraySet.Union(const ACollection: IJclIntegerCollection);
begin
  AddAll(ACollection);
end;

function TJclIntegerArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclCardinalArraySet } ====================================================

function TJclCardinalArraySet.Add(AValue: Cardinal): Boolean;
var
  Idx: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if Result then
    begin
      Idx := BinarySearch(AValue);
      if Idx >= 0 then
        Result := not ItemsEqual(GetValue(Idx), AValue) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AValue);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalArraySet.AddAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalArraySet.BinarySearch(AValue: Cardinal): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetValue(CompPos), AValue);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalArraySet.Contains(AValue: Cardinal): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AValue);
    if Idx >= 0 then
      Result := ItemsEqual(GetValue(Idx), AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalArraySet.Insert(Index: Integer; AValue: Cardinal): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclCardinalArraySet.Intersect(const ACollection: IJclCardinalCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclCardinalArraySet.Subtract(const ACollection: IJclCardinalCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclCardinalArraySet.Union(const ACollection: IJclCardinalCollection);
begin
  AddAll(ACollection);
end;

function TJclCardinalArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclInt64ArraySet } ====================================================

function TJclInt64ArraySet.Add(const AValue: Int64): Boolean;
var
  Idx: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if Result then
    begin
      Idx := BinarySearch(AValue);
      if Idx >= 0 then
        Result := not ItemsEqual(GetValue(Idx), AValue) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AValue);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64ArraySet.AddAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64ArraySet.BinarySearch(const AValue: Int64): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetValue(CompPos), AValue);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64ArraySet.Contains(const AValue: Int64): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AValue);
    if Idx >= 0 then
      Result := ItemsEqual(GetValue(Idx), AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64ArraySet.Insert(Index: Integer; const AValue: Int64): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclInt64ArraySet.Intersect(const ACollection: IJclInt64Collection);
begin
  RetainAll(ACollection);
end;

procedure TJclInt64ArraySet.Subtract(const ACollection: IJclInt64Collection);
begin
  RemoveAll(ACollection);
end;

procedure TJclInt64ArraySet.Union(const ACollection: IJclInt64Collection);
begin
  AddAll(ACollection);
end;

function TJclInt64ArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64ArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclPtrArraySet } ====================================================

function TJclPtrArraySet.Add(APtr: Pointer): Boolean;
var
  Idx: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(APtr, nil);
    if Result then
    begin
      Idx := BinarySearch(APtr);
      if Idx >= 0 then
        Result := not ItemsEqual(GetPointer(Idx), APtr) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, APtr);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrArraySet.AddAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrArraySet.BinarySearch(APtr: Pointer): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetPointer(CompPos), APtr);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrArraySet.Contains(APtr: Pointer): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(APtr);
    if Idx >= 0 then
      Result := ItemsEqual(GetPointer(Idx), APtr)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrArraySet.Insert(Index: Integer; APtr: Pointer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclPtrArraySet.Intersect(const ACollection: IJclPtrCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclPtrArraySet.Subtract(const ACollection: IJclPtrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclPtrArraySet.Union(const ACollection: IJclPtrCollection);
begin
  AddAll(ACollection);
end;

function TJclPtrArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclArraySet } ====================================================

function TJclArraySet.Add(AObject: TObject): Boolean;
var
  Idx: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if Result then
    begin
      Idx := BinarySearch(AObject);
      if Idx >= 0 then
        Result := not ItemsEqual(GetObject(Idx), AObject) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AObject);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArraySet.AddAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArraySet.BinarySearch(AObject: TObject): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetObject(CompPos), AObject);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArraySet.Contains(AObject: TObject): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AObject);
    if Idx >= 0 then
      Result := ItemsEqual(GetObject(Idx), AObject)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArraySet.Insert(Index: Integer; AObject: TObject): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclArraySet.Intersect(const ACollection: IJclCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclArraySet.Subtract(const ACollection: IJclCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclArraySet.Union(const ACollection: IJclCollection);
begin
  AddAll(ACollection);
end;

function TJclArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArraySet.Create(Size, False);
  AssignPropertiesTo(Result);
end;


{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

//=== { TJclArraySet<T> } ====================================================

function TJclArraySet<T>.Add(const AItem: T): Boolean;
var
  Idx: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if Result then
    begin
      Idx := BinarySearch(AItem);
      if Idx >= 0 then
        Result := not ItemsEqual(GetItem(Idx), AItem) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AItem);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArraySet<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArraySet<T>.BinarySearch(const AItem: T): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetItem(CompPos), AItem);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArraySet<T>.Contains(const AItem: T): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AItem);
    if Idx >= 0 then
      Result := ItemsEqual(GetItem(Idx), AItem)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArraySet<T>.Insert(Index: Integer; const AItem: T): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclArraySet<T>.Intersect(const ACollection: IJclCollection<T>);
begin
  RetainAll(ACollection);
end;

procedure TJclArraySet<T>.Subtract(const ACollection: IJclCollection<T>);
begin
  RemoveAll(ACollection);
end;

procedure TJclArraySet<T>.Union(const ACollection: IJclCollection<T>);
begin
  AddAll(ACollection);
end;

//=== { TJclArraySetE<T> } ===================================================

constructor TJclArraySetE<T>.Create(const AComparer: IJclComparer<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FComparer := AComparer;
end;

constructor TJclArraySetE<T>.Create(const AComparer: IJclComparer<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FComparer := AComparer;
end;

procedure TJclArraySetE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclArraySetE<T> then
    TJclArraySetE<T>(Dest).FComparer := Comparer;
end;

function TJclArraySetE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArraySetE<T>.Create(Comparer, Size, False);
  AssignPropertiesTo(Result);
end;

function TJclArraySetE<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Comparer <> nil then
    Result := Comparer.Compare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclArraySetE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Comparer <> nil then
    Result := Comparer.Compare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclArraySetF<T> } ===================================================

constructor TJclArraySetF<T>.Create(const ACompare: TCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  SetCompare(ACompare);
end;

constructor TJclArraySetF<T>.Create(const ACompare: TCompare<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  SetCompare(ACompare);
end;

function TJclArraySetF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArraySetF<T>.Create(Compare, Size, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclArraySetI<T> } ===================================================

function TJclArraySetI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArraySetI<T>.Create(Size, False);
  AssignPropertiesTo(Result);
end;

function TJclArraySetI<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := A.CompareTo(B);
end;

function TJclArraySetI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := A.CompareTo(B) = 0;
end;

//DOM-IGNORE-END
{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

