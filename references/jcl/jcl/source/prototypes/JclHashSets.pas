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
{ The Original Code is HashSet.pas.                                                                }
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

unit JclHashSets;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils, System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils, Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  JclAlgorithms,
  JclBase, JclAbstractContainers, JclContainerIntf, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclHashSets.imp}
{$I containers\JclHashSets.int}
{$I containers\JclAlgorithms.int}
{$I containers\JclAlgorithms.imp}
type
  TItrStart = (isFirst, isLast);

(*$JPPLOOP ALLTYPEINDEX ALLTYPECOUNT
  {$JPPEXPANDMACRO JCLHASHSETTYPEINT(,)}

  {$JPPEXPANDMACRO JCLHASHSETINT(,,,,,,,,,,,,,,,,)}

  {$JPPEXPANDMACRO JCLHASHSETITRINT(,,,,,,,)}

*)
  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN

  TJclHashSetBucket<T> = class
  public
    type
      TDynArray = array of T;
  public
    Size: Integer;
    Entries: TDynArray;
    {$JPPDEFINE GENERIC}{$JPPEXPANDMACRO MOVEARRAYINT(MoveArray,TDynArray,)}
  end;

  {$JPPEXPANDMACRO JCLHASHSETINT(TJclHashSet<T>,TJclAbstractContainer<T>,IJclContainer<T>,IJclFlatContainer<T>,TJclHashSetBucket<T>,IJclCollection<T>,IJclSet<T>,IJclIterator<T>,IJclEqualityComparer<T>,IJclHashConverter<T>, IJclItemOwner<T>\,,,,const ,AItem,T,; AOwnsItems: Boolean)}

  {$JPPEXPANDMACRO JCLHASHSETITRINT(TJclHashSetIterator<T>,IJclIterator<T>,TJclHashSet<T>,const ,AItem,T,GetItem,SetItem)}

  // E = External helper to compare items for equality
  TJclHashSetE<T> = class(TJclHashSet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer, IJclContainer<T>,
    IJclEqualityComparer<T>, IJclHashConverter<T>, IJclItemOwner<T>, IJclCollection<T>, IJclSet<T>)
  private
    FEqualityComparer: IJclEqualityComparer<T>;
    FHashConverter: IJclHashConverter<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityComparer: IJclEqualityComparer<T>; const AHashConverter: IJclHashConverter<T>;
      ACapacity: Integer; AOwnsItems: Boolean); overload;
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    property EqualityComparer: IJclEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
    { IJclHashConverter<T> }
    function Hash(const AItem: T): Integer; override;
    property HashConverter: IJclHashConverter<T> read FHashConverter write FHashConverter;
  end;

  // F = Function to compare items for equality
  TJclHashSetF<T> = class(TJclHashSet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer, IJclContainer<T>,
    IJclEqualityComparer<T>, IJclHashConverter<T>, IJclItemOwner<T>,
    IJclCollection<T>, IJclSet<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const AHashConvert: THashConvert<T>;
      ACapacity: Integer; AOwnsItems: Boolean); overload;
  end;

  // I = Items can compare themselves to others
  TJclHashSetI<T: IEquatable<T>, IHashable> = class(TJclHashSet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer, IJclContainer<T>,
    IJclEqualityComparer<T>, IJclHashConverter<T>, IJclItemOwner<T>,
    IJclCollection<T>, IJclSet<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclHashConverter<T> }
    function Hash(const AItem: T): Integer; override;
  end;

  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

{$IFDEF BCB}
{$IFDEF WIN64}
  {$HPPEMIT '#ifdef MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT ' #undef MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT ' #define JclHashSets_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT '#endif'}

  {$HPPEMIT END '#ifdef JclHashSets_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #define MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #undef JclHashSets_MANAGED_INTERFACE_OPERATORS'}
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

(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO JCLHASHSETIMP(,,,,,,,,,,,,)}

{$JPPEXPANDMACRO JCLHASHSETITRIMP(,,,,,,,,,)}
*)
{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

//=== { TJclHashSetBucket<T> } =================================================

{$JPPDEFINE GENERIC}{$JPPEXPANDMACRO MOVEARRAYIMP(MoveArray,TDynArray,Default(T),TJclHashSetBucket<T>.,)}

{$JPPEXPANDMACRO JCLHASHSETIMP(TJclHashSet<T>,TJclHashSetBucket<T>,; AOwnsItems: Boolean,AOwnsItems,IJclCollection<T>,TJclHashSetIterator<T>,IJclIterator<T>,Bucket.MoveArray,const ,AItem,T,Default(T),FreeItem)}

{$JPPEXPANDMACRO JCLHASHSETITRIMP(TJclHashSetIterator<T>,TJclHashSet<T>,TJclHashSetBucket<T>,IJclIterator<T>,const ,AItem,T,Default(T),GetItem,SetItem)}

//=== { TJclHashSetE<T> } ====================================================

constructor TJclHashSetE<T>.Create(const AEqualityComparer: IJclEqualityComparer<T>;
  const AHashConverter: IJclHashConverter<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
  FHashConverter := AHashConverter;
end;

procedure TJclHashSetE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclHashSetE<T>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashSetE<T> then
  begin
    ADest := TJclHashSetE<T>(Dest);
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FHashConverter := FHashConverter;
  end;
end;

function TJclHashSetE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashSetE<T>.Create(EqualityComparer, HashConverter, FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclHashSetE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.ItemsEqual(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclHashSetE<T>.Hash(const AItem: T): Integer;
begin
  if HashConverter <> nil then
    Result := HashConverter.Hash(AItem)
  else
    Result := inherited Hash(AItem);
end;

//=== { TJclHashSetF<T> } ====================================================

constructor TJclHashSetF<T>.Create(const AEqualityCompare: TEqualityCompare<T>;
  const AHashConvert: THashConvert<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
  SetHashConvert(AHashConvert);
end;

function TJclHashSetF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashSetF<T>.Create(EqualityCompare, HashConvert, FSize, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclHashSetI<T> } ====================================================

function TJclHashSetI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashSetI<T>.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclHashSetI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
    Result := A.Equals(B);
end;

function TJclHashSetI<T>.Hash(const AItem: T): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AItem)
  else
    Result := AItem.GetHashCode;
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

