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
{ The Original Code is JclTrees.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet. Portions created by                }
{ Florent Ouchet are Copyright (C) Florent Ouchet <outchy att users dott sourceforge dott net      }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
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

unit JclTrees;

interface

{$I jcl.inc}

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
  JclBase, JclAbstractContainers, JclContainerIntf, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclTrees.imp}
{$I containers\JclTrees.int}
type
  TItrStart = (isFirst, isLast, isRoot);

(*$JPPLOOP ALLTYPEINDEX ALLTYPECOUNT
  {$JPPEXPANDMACRO JCLTREETYPESINT(,,,,)}

  {$JPPEXPANDMACRO JCLTREEINT(,,,,,,,,,,,,,,,,,)}

  {$JPPEXPANDMACRO JCLTREEITRINT(,,,,,,,,,,,,,)}

*)
  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN

  {$JPPEXPANDMACRO JCLTREETYPESINT(TJclTreeNode<T>,IJclEqualityComparer<T>,const ,AItem,T)}

  TJclPreOrderTreeIterator<T> = class;
  TJclPostOrderTreeIterator<T> = class;

  {$JPPEXPANDMACRO JCLTREEINT(TTreeNode,TJclTree<T>,TJclAbstractContainer<T>,IJclContainer<T>,IJclFlatContainer<T>,IJclEqualityComparer<T>,IJclCollection<T>,IJclTree<T>,IJclIterator<T>,IJclTreeIterator<T>, IJclItemOwner<T>\,,
protected
  type
    TTreeNode = TJclTreeNode<T>;
    TPreOrderTreeIterator = TJclPreOrderTreeIterator<T>;
    TPostOrderTreeIterator = TJclPostOrderTreeIterator<T>;,,AOwnsItems: Boolean,const ,AItem,T,Default(T))}

  {$JPPEXPANDMACRO JCLTREEITRINT(TJclTreeIterator<T>,TJclPreOrderTreeIterator<T>,TJclPostOrderTreeIterator<T>,TJclTree<T>.TTreeNode,TJclTree<T>,IJclIterator<T>,IJclTreeIterator<T>,IJclEqualityComparer<T>,const ,AItem,T,Default(T),GetItem,SetItem)}

  // E = External helper to compare items for equality
  TJclTreeE<T> = class(TJclTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclContainer<T>, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FEqualityComparer: IJclEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityComparer: IJclEqualityComparer<T>; AOwnsItems: Boolean);
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    property EqualityComparer: IJclEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclTreeF<T> = class(TJclTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclContainer<T>, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
  end;

  // I = Items can compare themselves to an other for equality
  TJclTreeI<T: IEquatable<T>> = class(TJclTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclContainer<T>, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclTree<T>)
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
  {$HPPEMIT ' #define JclTrees_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT '#endif'}

  {$HPPEMIT END '#ifdef JclTrees_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #define MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #undef JclTrees_MANAGED_INTERFACE_OPERATORS'}
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
{$JPPEXPANDMACRO JCLTREETYPESIMP(,,,,)}

{$JPPEXPANDMACRO JCLTREEIMP(,,,,,,,,,,,,,,)}

{$JPPEXPANDMACRO JCLTREEITRIMP(,,,,,,,,,,,,,,)}

*)
{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

{$JPPEXPANDMACRO JCLTREETYPESIMP(TJclTreeNode<T>,IJclEqualityComparer<T>,const ,AItem,T)}

{$JPPEXPANDMACRO JCLTREEIMP(TTreeNode,TJclTree<T>,TPreOrderTreeIterator,TPostOrderTreeIterator,IJclCollection<T>,IJclIterator<T>,IJclTreeIterator<T>,IJclEqualityComparer<T>,AOwnsItems: Boolean,AOwnsItems,const ,AItem,T,Default(T),FreeItem)}

{$JPPEXPANDMACRO JCLTREEITRIMP(TJclTreeIterator<T>,TJclPreOrderTreeIterator<T>,TJclPostOrderTreeIterator<T>,TJclTreeNode<T>,TJclTree<T>,IJclIterator<T>,IJclTreeIterator<T>,IJclEqualityComparer<T>,const ,AItem,T,Default(T),GetItem,SetItem,FreeItem)}

//=== { TJclTreeE<T> } =======================================================

constructor TJclTreeE<T>.Create(const AEqualityComparer: IJclEqualityComparer<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclTreeE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclTreeE<T> then
    TJclTreeE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclTreeE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTreeE<T>.Create(EqualityComparer, False);
  AssignPropertiesTo(Result);
end;

function TJclTreeE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.ItemsEqual(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclTreeF<T> } =======================================================

constructor TJclTreeF<T>.Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  SetCompare(ACompare);
end;

function TJclTreeF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTreeF<T>.Create(Compare, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclTreeI<T> } =======================================================

function TJclTreeI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTreeI<T>.Create(False);
  AssignPropertiesTo(Result);
end;

function TJclTreeI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
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
