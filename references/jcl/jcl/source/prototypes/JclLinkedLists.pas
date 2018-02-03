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
{ The Original Code is LinkedList.pas.                                                             }
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

unit JclLinkedLists;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF SUPPORTS_GENERICS}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  {$IFDEF HAS_UNITSCOPE}
  System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase, JclAbstractContainers, JclContainerIntf, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclLinkedLists.imp}
{$I containers\JclLinkedLists.int}
type
  TItrStart = (isFirst, isLast);

(*$JPPLOOP ALLTYPEINDEX ALLTYPECOUNT
  {$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT(,)}

  {$JPPEXPANDMACRO JCLLINKEDLISTINT(,,,,,,,,,,,,,,,,,)}

  {$JPPEXPANDMACRO JCLLINKEDLISTITRINT(,,,,,,,,,,)}

*)
  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN

  (*$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT(TJclLinkedListItem<T>,T)*)

  TJclLinkedListIterator<T> = class;

  (*$JPPEXPANDMACRO JCLLINKEDLISTINT(TLinkedListItem,TJclLinkedList<T>,TJclAbstractContainer<T>,IJclContainer<T>,IJclFlatContainer<T>,IJclCollection<T>,IJclList<T>,IJclIterator<T>,IJclEqualityComparer<T>,IJclItemOwner<T>\,,
protected
  type
    TLinkedListItem = TJclLinkedListItem<T>;
    TLinkedListIterator = TJclLinkedListIterator<T>;,,; AOwnsItems: Boolean,const ,AItem,T,GetItem,SetItem)*)

  (*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TJclLinkedListIterator<T>,IJclIterator<T>,IJclList<T>,IJclEqualityComparer<T>,TJclLinkedList<T>.TLinkedListItem,const ,AItem,T,Default(T),GetItem,SetItem)*)

  // E = External helper to compare items
  // GetHashCode is never called
  TJclLinkedListE<T> = class(TJclLinkedList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclContainer<T>, IJclCollection<T>, IJclList<T>, IJclEqualityComparer<T>,
    IJclItemOwner<T>)
  private
    FEqualityComparer: IJclEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityComparer: IJclEqualityComparer<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean);
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    property EqualityComparer: IJclEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclLinkedListF<T> = class(TJclLinkedList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclContainer<T>, IJclCollection<T>, IJclList<T>, IJclEqualityComparer<T>,
    IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean);
  end;

  // I = Items can compare themselves to an other
  TJclLinkedListI<T: IEquatable<T>> = class(TJclLinkedList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclContainer<T>, IJclCollection<T>, IJclList<T>, IJclEqualityComparer<T>,
    IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
  end;

  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

{$IFDEF BCB}
{$IFDEF WIN64}
  {$HPPEMIT '#ifdef MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT ' #undef MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT ' #define JclLinkedLists_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT '#endif'}

  {$HPPEMIT END '#ifdef JclLinkedLists_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #define MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #undef JclLinkedLists_MANAGED_INTERFACE_OPERATORS'}
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

(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(,,,,,,,,,,,,,,)}

{$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(,,,,,,,,,,,)}

*)
{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclLinkedList<T>,TLinkedListItem,IJclCollection<T>,IJclList<T>,IJclIterator<T>,TLinkedListIterator,; AOwnsItems: Boolean,AOwnsItems,const ,AItem,T,Default(T),GetItem,SetItem,FreeItem)}

(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TJclLinkedListIterator<T>,IJclIterator<T>,IJclList<T>,IJclEqualityComparer<T>,TJclLinkedList<T>.TLinkedListItem,const ,AItem,T,Default(T),GetItem,SetItem,(FownList as IJclItemOwner<T>).FreeItem(FCursor.Value);)*)

//=== { TJclLinkedListE<T> } =================================================

constructor TJclLinkedListE<T>.Create(const AEqualityComparer: IJclEqualityComparer<T>;
  const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclLinkedListE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclLinkedListE<T> then
    TJclLinkedListE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclLinkedListE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedListE<T>.Create(EqualityComparer, nil, False);
  AssignPropertiesTo(Result);
end;

function TJclLinkedListE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.ItemsEqual(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclLinkedListF<T> } =================================================

constructor TJclLinkedListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>;
  const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
end;

function TJclLinkedListF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedListF<T>.Create(EqualityCompare, nil, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclLinkedListI<T> } =================================================

function TJclLinkedListI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedListI<T>.Create(nil, False);
  AssignPropertiesTo(Result);
end;

function TJclLinkedListI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
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

