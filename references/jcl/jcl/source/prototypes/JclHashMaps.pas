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
{ The Original Code is HashMap.pas.                                                                }
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

unit JclHashMaps;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  JclAlgorithms,
  JclBase, JclSynch,
  JclContainerIntf, JclAbstractContainers, JclArrayLists, JclArraySets;
{$I containers\JclContainerCommon.imp}
{$I containers\JclHashMaps.imp}
{$I containers\JclHashMaps.int}
{$I containers\JclAlgorithms.int}
{$I containers\JclAlgorithms.imp}
type
(*$JPPLOOP ALLMAPINDEX ALLMAPCOUNT
  {$JPPEXPANDMACRO JCLHASHMAPTYPESINT(,,,,)}

  {$JPPEXPANDMACRO JCLHASHMAPINT(,,,,,,,,,,,,,,)}

*)
  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN

  TJclHashEntry<TKey,TValue> = record
    Key: TKey;
    Value: TValue;
  end;

  TJclBucket<TKey,TValue> = class
  public
    type
      THashEntryArray = array of TJclHashEntry<TKey,TValue>;
  public
    Size: Integer;
    Entries: THashEntryArray;
    {$JPPUNDEF GENERIC}{$JPPDEFINE REFCOUNTED}{$JPPEXPANDMACRO MOVEARRAYINT(MoveArray,THashEntryArray,)}
  end;

  (*$JPPEXPANDMACRO JCLHASHMAPINT(TBucket,TJclHashMap<TKey\,TValue>,TJclAbstractContainerBase,IJclMap<TKey\,TValue>,IJclSet<TKey>,IJclCollection<TKey>,IJclCollection<TValue>, IJclPairOwner<TKey\, TValue>\,,
protected
  type
    TBucket = TJclBucket<TKey\,TValue>;
private
  FOwnsKeys: Boolean;
  FOwnsValues: Boolean;
protected
  function Hash(const AKey: TKey): Integer; virtual; abstract;
  function KeysEqual(const A\, B: TKey): Boolean; virtual; abstract;
  function ValuesEqual(const A\, B: TValue): Boolean; virtual; abstract;
  function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; virtual; abstract;
  function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; virtual; abstract;
  function CreateEmptyKeyList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TKey>; virtual; abstract;
public
  { IJclPairOwner }
  function FreeKey(var Key: TKey): TKey;
  function FreeValue(var Value: TValue): TValue;
  function GetOwnsKeys: Boolean;
  function GetOwnsValues: Boolean;
  property OwnsKeys: Boolean read FOwnsKeys;
  property OwnsValues: Boolean read FOwnsValues;,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,const ,TKey,const ,TValue)*)

  // E = external helper to compare and hash items
  // KeyComparer is used only when getting KeySet
  // GetHashCode and Equals methods of KeyEqualityComparer are used
  // GetHashCode of ValueEqualityComparer is not used
  TJclHashMapE<TKey, TValue> = class(TJclHashMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer, IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    type
      TArrayList = TJclArrayListE<TValue>;
      TArraySet = TJclArraySetE<TKey>;
      TArrayKeyList = TJclArrayListE<TKey>;
  private
    FKeyEqualityComparer: IJclEqualityComparer<TKey>;
    FKeyHashConverter: IJclHashConverter<TKey>;
    FKeyComparer: IJclComparer<TKey>;
    FValueEqualityComparer: IJclEqualityComparer<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    function CreateEmptyKeyList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TKey>; override;
  public
    constructor Create(const AKeyEqualityComparer: IJclEqualityComparer<TKey>;
      const AKeyHashConverter: IJclHashConverter<TKey>; const AValueEqualityComparer: IJclEqualityComparer<TValue>;
      const AKeyComparer: IJclComparer<TKey>; ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyEqualityComparer: IJclEqualityComparer<TKey> read FKeyEqualityComparer write FKeyEqualityComparer;
    property KeyHashConverter: IJclHashConverter<TKey> read FKeyHashConverter write FKeyHashConverter;
    property KeyComparer: IJclComparer<TKey> read FKeyComparer write FKeyComparer;
    property ValueEqualityComparer: IJclEqualityComparer<TValue> read FValueEqualityComparer write FValueEqualityComparer;
  end;

  // F = Functions to compare and hash items
  // KeyComparer is used only when getting KeySet
  TJclHashMapF<TKey, TValue> = class(TJclHashMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer, IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    type
      TArrayList = TJclArrayListF<TValue>;
      TArraySet = TJclArraySetF<TKey>;
      TArrayKeyList = TJclArrayListF<TKey>;
  private
    FKeyEqualityCompare: TEqualityCompare<TKey>;
    FKeyHash: THashConvert<TKey>;
    FKeyCompare: TCompare<TKey>;
    FValueEqualityCompare: TEqualityCompare<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    function CreateEmptyKeyList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TKey>; override;
  public
    constructor Create(AKeyEqualityCompare: TEqualityCompare<TKey>; AKeyHash: THashConvert<TKey>;
      AValueEqualityCompare: TEqualityCompare<TValue>; AKeyCompare: TCompare<TKey>;
      ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyEqualityCompare: TEqualityCompare<TKey> read FKeyEqualityCompare write FKeyEqualityCompare;
    property KeyCompare: TCompare<TKey> read FKeyCompare write FKeyCompare;
    property KeyHash: THashConvert<TKey> read FKeyHash write FKeyHash;
    property ValueEqualityCompare: TEqualityCompare<TValue> read FValueEqualityCompare write FValueEqualityCompare;
  end;

  // I = items can compare themselves to an other, items can create hash value from themselves
  TJclHashMapI<TKey: IComparable<TKey>, IEquatable<TKey>, IHashable; TValue: IEquatable<TValue>> = class(TJclHashMap<TKey, TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclBaseContainer,
    IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    type
      TArrayList = TJclArrayListI<TValue>;
      TArraySet = TJclArraySetI<TKey>;
      TArrayKeyList = TJclArrayListI<TKey>;
  protected
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    function CreateEmptyKeyList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TKey>; override;
  end;

  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

{$IFDEF BCB}
{$IFDEF WIN64}
  {$HPPEMIT '#ifdef MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT ' #undef MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT ' #define JclHashMaps_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT '#endif'}

  {$HPPEMIT END '#ifdef JclHashMaps_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #define MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #undef JclHashMaps_MANAGED_INTERFACE_OPERATORS'}
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
  System.SysUtils,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  JclResources;

(*$JPPLOOP TRUEMAPINDEX TRUEMAPCOUNT
{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(,,,)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(,,,,,,,,,,,,,,,,,,)}

*)
{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

//=== { TJclBucket<TKey, TValue> } ==========================================

{$JPPUNDEF GENERIC}{$JPPDEFINE REFCOUNTED}{$JPPEXPANDMACRO MOVEARRAYIMP(MoveArray,THashEntryArray,,TJclBucket<TKey\, TValue>.,)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclHashMap<TKey\, TValue>,TBucket,IJclMap<TKey\, TValue>,IJclSet<TKey>,IJclCollection<TKey>,IJclIterator<TKey>,IJclCollection<TValue>,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,

  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;,const ,TKey,Default(TKey),const ,TValue,Default(TValue),CreateEmptyArraySet(FSize, False),CreateEmptyKeyList(FSize, False),CreateEmptyArrayList(FSize, False))}

function TJclHashMap<TKey, TValue>.FreeKey(var Key: TKey): TKey;
begin
  if FOwnsKeys then
  begin
    Result := Default(TKey);
    FreeAndNil(Key);
  end
  else
  begin
    Result := Key;
    Key := Default(TKey);
  end;
end;

function TJclHashMap<TKey, TValue>.FreeValue(var Value: TValue): TValue;
begin
  if FOwnsValues then
  begin
    Result := Default(TValue);
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := Default(TValue);
  end;
end;

function TJclHashMap<TKey, TValue>.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

function TJclHashMap<TKey, TValue>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

//=== { TJclHashMapE<TKey, TValue> } =========================================

constructor TJclHashMapE<TKey, TValue>.Create(const AKeyEqualityComparer: IJclEqualityComparer<TKey>;
  const AKeyHashConverter: IJclHashConverter<TKey>; const AValueEqualityComparer: IJclEqualityComparer<TValue>;
  const AKeyComparer: IJclComparer<TKey>; ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsKeys, AOwnsValues);
  FKeyEqualityComparer := AKeyEqualityComparer;
  FKeyHashConverter := AKeyHashConverter;
  FValueEqualityComparer := AValueEqualityComparer;
  FKeyComparer := AKeyComparer;
end;

procedure TJclHashMapE<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclHashMapE<TKey, TValue>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashMapE<TKey, TValue> then
  begin
    ADest := TJclHashMapE<TKey, TValue>(Dest);
    ADest.FKeyEqualityComparer := FKeyEqualityComparer;
    ADest.FKeyHashConverter := FKeyHashConverter;
    ADest.FValueEqualityComparer := FValueEqualityComparer;
    ADest.FKeyComparer := FKeyComparer;
  end;
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TArrayList.Create(ValueEqualityComparer, ACapacity, AOwnsObjects);
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TArraySet.Create(KeyComparer, ACapacity, AOwnsObjects);
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyKeyList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TKey>;
begin
  Result := TArrayKeyList.Create(KeyEqualityComparer, ACapacity, AOwnsObjects);
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapE<TKey, TValue>.Create(KeyEqualityComparer, KeyHashConverter, ValueEqualityComparer,
    KeyComparer, FCapacity, False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMapE<TKey, TValue>.Hash(const AKey: TKey): Integer;
begin
  if KeyEqualityComparer = nil then
    raise EJclNoHashConverterError.Create;
  Result := KeyHashConverter.Hash(AKey);
end;

function TJclHashMapE<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  if KeyEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := KeyEqualityComparer.ItemsEqual(A, B);
end;

function TJclHashMapE<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
begin
  if ValueEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := ValueEqualityComparer.ItemsEqual(A, B);
end;

//=== { TJclHashMapF<TKey, TValue> } =========================================

constructor TJclHashMapF<TKey, TValue>.Create(AKeyEqualityCompare: TEqualityCompare<TKey>;
  AKeyHash: THashConvert<TKey>; AValueEqualityCompare: TEqualityCompare<TValue>; AKeyCompare: TCompare<TKey>;
  ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsKeys, AOwnsValues);
  FKeyEqualityCompare := AKeyEqualityCompare;
  FKeyHash := AKeyHash;
  FValueEqualityCompare := AValueEqualityCompare;
  FKeyCompare := AKeyCompare;
end;

procedure TJclHashMapF<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclHashMapF<TKey, TValue>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashMapF<TKey, TValue> then
  begin
    ADest := TJclHashMapF<TKey, TValue>(Dest);
    ADest.FKeyEqualityCompare := FKeyEqualityCompare;
    ADest.FKeyHash := FKeyHash;
    ADest.FValueEqualityCompare := FValueEqualityCompare;
    ADest.FKeyCompare := FKeyCompare;
  end;
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TArrayList.Create(ValueEqualityCompare, ACapacity, AOwnsObjects);
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TArraySet.Create(KeyCompare, ACapacity, AOwnsObjects);
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyKeyList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TKey>;
begin
  Result := TArrayKeyList.Create(KeyEqualityCompare, ACapacity, AOwnsObjects);
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapF<TKey, TValue>.Create(KeyEqualityCompare, KeyHash, ValueEqualityCompare, KeyCompare, FCapacity,
    False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMapF<TKey, TValue>.Hash(const AKey: TKey): Integer;
begin
  if not Assigned(KeyHash) then
    raise EJclNoHashConverterError.Create;
  Result := KeyHash(AKey);
end;

function TJclHashMapF<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  if not Assigned(KeyEqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := KeyEqualityCompare(A, B);
end;

function TJclHashMapF<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
begin
  if not Assigned(ValueEqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := ValueEqualityCompare(A, B);
end;

//=== { TJclHashMapI<TKey, TValue> } =========================================

function TJclHashMapI<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TArrayList.Create(ACapacity, AOwnsObjects);
end;

function TJclHashMapI<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TArraySet.Create(ACapacity, AOwnsObjects);
end;

function TJclHashMapI<TKey, TValue>.CreateEmptyKeyList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TKey>;
begin
  Result := TArrayKeyList.Create(ACapacity, AOwnsObjects);
end;

function TJclHashMapI<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapI<TKey, TValue>.Create(FCapacity, False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMapI<TKey, TValue>.Hash(const AKey: TKey): Integer;
begin
  Result := AKey.GetHashCode;
end;

function TJclHashMapI<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  Result := A.Equals(B);
end;

function TJclHashMapI<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
begin
  Result := A.Equals(B);
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

