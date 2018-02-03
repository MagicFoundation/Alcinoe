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
{ The Original Code is BucketArraySets.pas.                                                        }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet. Portions created by                }
{ Florent Ouchet are Copyright (C) Florent Ouchet (ouchet dott florent att laposte dott net)       }
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

unit JclBucketArraySets;

{$I jcl.inc}

interface

uses
  Classes,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF SUPPORTS_GENERICS}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf, JclBucketArrayLists, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclBucketArraySets.int}
{$I containers\JclBucketArraySets.imp}
type
(*$JPPEXPANDMACRO JCLBUCKETARRAYSETINT(TJclIntfBucketArraySet,TJclIntfBucketArrayList,IJclIntfCollection,IJclIntfList,IJclIntfArray,IJclIntfSet, IJclIntfEqualityComparer\, IJclIntfComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AInterface,IInterface)*)

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETINT(TJclAnsiStrBucketArraySet,TJclAnsiStrBucketArrayList,IJclAnsiStrCollection,IJclAnsiStrList,IJclAnsiStrArray,IJclAnsiStrSet, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrFlatContainer\, IJclAnsiStrEqualityComparer\, IJclAnsiStrComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,const ,AString,AnsiString)*)

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETINT(TJclWideStrBucketArraySet,TJclWideStrBucketArrayList,IJclWideStrCollection,IJclWideStrList,IJclWideStrArray,IJclWideStrSet, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrFlatContainer\, IJclWideStrEqualityComparer\, IJclWideStrComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,const ,AString,WideString)*)

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLBUCKETARRAYSETINT(TJclUnicodeStrBucketArraySet,TJclUnicodeStrBucketArrayList,IJclUnicodeStrCollection,IJclUnicodeStrList,IJclUnicodeStrArray,IJclUnicodeStrSet, IJclStrContainer\, IJclUnicodeStrContainer\, IJclUnicodeStrFlatContainer\, IJclUnicodeStrEqualityComparer\, IJclUnicodeStrComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,const ,AString,UnicodeString)*)
{$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrBucketArraySet = TJclAnsiStrBucketArraySet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrBucketArraySet = TJclWideStrBucketArraySet;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrBucketArraySet = TJclUnicodeStrBucketArraySet;
  {$ENDIF CONTAINER_UNICODESTR}

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETINT(TJclSingleBucketArraySet,TJclSingleBucketArrayList,IJclSingleCollection,IJclSingleList,IJclSingleArray,IJclSingleSet, IJclSingleContainer\, IJclSingleEqualityComparer\, IJclSingleComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Single)*)

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETINT(TJclDoubleBucketArraySet,TJclDoubleBucketArrayList,IJclDoubleCollection,IJclDoubleList,IJclDoubleArray,IJclDoubleSet, IJclDoubleContainer\, IJclDoubleEqualityComparer\, IJclDoubleComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Double)*)

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETINT(TJclExtendedBucketArraySet,TJclExtendedBucketArrayList,IJclExtendedCollection,IJclExtendedList,IJclExtendedArray,IJclExtendedSet, IJclExtendedContainer\, IJclExtendedEqualityComparer\, IJclExtendedComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Extended)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatBucketArraySet = TJclExtendedBucketArraySet;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatBucketArraySet = TJclDoubleBucketArraySet;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatBucketArraySet = TJclSingleBucketArraySet;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETINT(TJclIntegerBucketArraySet,TJclIntegerBucketArrayList,IJclIntegerCollection,IJclIntegerList,IJclIntegerArray,IJclIntegerSet, IJclIntegerEqualityComparer\, IJclIntegerComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,AValue,Integer)*)

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETINT(TJclCardinalBucketArraySet,TJclCardinalBucketArrayList,IJclCardinalCollection,IJclCardinalList,IJclCardinalArray,IJclCardinalSet, IJclCardinalEqualityComparer\, IJclCardinalComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,AValue,Cardinal)*)

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETINT(TJclInt64BucketArraySet,TJclInt64BucketArrayList,IJclInt64Collection,IJclInt64List,IJclInt64Array,IJclInt64Set, IJclInt64EqualityComparer\, IJclInt64Comparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Int64)*)

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETINT(TJclPtrBucketArraySet,TJclPtrBucketArrayList,IJclPtrCollection,IJclPtrList,IJclPtrArray,IJclPtrSet, IJclPtrEqualityComparer\, IJclPtrComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,APtr,Pointer)*)

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETINT(TJclBucketArraySet,TJclBucketArrayList,IJclCollection,IJclList,IJclArray,IJclSet, IJclObjectOwner\, IJclEqualityComparer\, IJclComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,AObject,TObject)*)

  {$IFDEF _SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLBUCKETARRAYSETINT(TJclBucketArraySet<T>,TJclBucketArrayList<T>,IJclCollection<T>,IJclList<T>,IJclArray<T>,IJclSet<T>, IJclItemOwner<T>\, IJclEqualityComparer<T>\, IJclComparer<T>\,,,,const ,AItem,T)*)

  // E = External helper to compare items
  TJclBucketArraySetE<T> = class(TJclBucketArraySet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  private
    FComparer: IJclComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function ItemsCompare(const A, B: T): Integer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AComparer: IJclComparer<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const AComparer: IJclComparer<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;

    property Comparer: IJclComparer<T> read FComparer write FComparer;
  end;

  // F = Function to compare items
  TJclBucketArraySetF<T> = class(TJclBucketArraySet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACompare: TCompare<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const ACompare: TCompare<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;
  end;

  // I = Items can compare themselves to others
  TJclBucketArraySetI<T: IComparable<T>> = class(TJclBucketArraySet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  protected
    function ItemsCompare(const A, B: T): Integer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  end;

  {$ENDIF _SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils;

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETIMP(TJclIntfBucketArraySet,IJclIntfCollection,IJclIntfIterator,const ,AInterface,IInterface,nil,GetObject)*)

function TJclIntfBucketArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfBucketArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETIMP(TJclAnsiStrBucketArraySet,IJclAnsiStrCollection,IJclAnsiStrIterator,const ,AString,AnsiString,'',GetString)*)

function TJclAnsiStrBucketArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrBucketArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETIMP(TJclWideStrBucketArraySet,IJclWideStrCollection,IJclWideStrIterator,const ,AString,WideString,'',GetString)*)

function TJclWideStrBucketArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrBucketArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLBUCKETARRAYSETIMP(TJclUnicodeStrBucketArraySet,IJclUnicodeStrCollection,IJclUnicodeStrIterator,const ,AString,UnicodeString,'',GetString)*)

function TJclUnicodeStrBucketArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrBucketArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETIMP(TJclSingleBucketArraySet,IJclSingleCollection,IJclSingleIterator,const ,AValue,Single,0.0,GetValue)*)

function TJclSingleBucketArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleBucketArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETIMP(TJclDoubleBucketArraySet,IJclDoubleCollection,IJclDoubleIterator,const ,AValue,Double,0.0,GetValue)*)

function TJclDoubleBucketArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleBucketArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETIMP(TJclExtendedBucketArraySet,IJclExtendedCollection,IJclExtendedIterator,const ,AValue,Extended,0.0,GetValue)*)

function TJclExtendedBucketArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedBucketArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETIMP(TJclIntegerBucketArraySet,IJclIntegerCollection,IJclIntegerIterator,,AValue,Integer,0,GetValue)*)

function TJclIntegerBucketArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerBucketArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETIMP(TJclCardinalBucketArraySet,IJclCardinalCollection,IJclCardinalIterator,,AValue,Cardinal,0,GetValue)*)

function TJclCardinalBucketArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalBucketArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETIMP(TJclInt64BucketArraySet,IJclInt64Collection,IJclInt64Iterator,const ,AValue,Int64,0,GetValue)*)

function TJclInt64BucketArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64BucketArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETIMP(TJclPtrBucketArraySet,IJclPtrCollection,IJclPtrIterator,,APtr,Pointer,nil,GetPointer)*)

function TJclPtrBucketArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrBucketArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLBUCKETARRAYSETIMP(TJclBucketArraySet,IJclCollection,IJclIterator,,AObject,TObject,nil,GetObject)*)

function TJclBucketArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBucketArraySet.Create(Size, False);
  AssignPropertiesTo(Result);
end;

{$IFDEF _SUPPORTS_GENERICS}
{$JPPEXPANDMACRO JCLBUCKETARRAYSETIMP(TJclBucketArraySet<T>,IJclCollection<T>,IJclIterator<T>,const ,AItem,T,Default(T),GetItem)}

//=== { TJclBucketArraySetE<T> } ===================================================

constructor TJclBucketArraySetE<T>.Create(const AComparer: IJclComparer<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FComparer := AComparer;
end;

constructor TJclBucketArraySetE<T>.Create(const AComparer: IJclComparer<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FComparer := AComparer;
end;

procedure TJclBucketArraySetE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclBucketArraySetE<T> then
    TJclBucketArraySetE<T>(Dest).FComparer := Comparer;
end;

function TJclBucketArraySetE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBucketArraySetE<T>.Create(Comparer, Size, False);
  AssignPropertiesTo(Result);
end;

function TJclBucketArraySetE<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Comparer <> nil then
    Result := Comparer.Compare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclBucketArraySetE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Comparer <> nil then
    Result := Comparer.Compare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclBucketArraySetF<T> } ===================================================

constructor TJclBucketArraySetF<T>.Create(const ACompare: TCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  SetCompare(ACompare);
end;

constructor TJclBucketArraySetF<T>.Create(const ACompare: TCompare<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  SetCompare(ACompare);
end;

function TJclBucketArraySetF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBucketArraySetF<T>.Create(Compare, Size, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclBucketArraySetI<T> } ===================================================

function TJclBucketArraySetI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBucketArraySetI<T>.Create(Size, False);
  AssignPropertiesTo(Result);
end;

function TJclBucketArraySetI<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := A.CompareTo(B);
end;

function TJclBucketArraySetI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := A.CompareTo(B) = 0;
end;

{$ENDIF _SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

