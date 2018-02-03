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
{ The Original Code is JclBucketArrayLists.pas.                                                    }
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

unit JclBucketArrayLists;

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
  JclBase, JclAbstractContainers, JclContainerIntf, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclBucketArrayLists.int}
{$I containers\JclBucketArrayLists.imp}
type
  TItrStart = (isFirst, isLast);

{$JPPEXPANDMACRO JCLBUCKETARRAYTYPESINT(TJclIntfBucket,TDynIInterfaceArray,,TDynIntfBucketArray)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTINT(TJclIntfBucketArrayList,TJclIntfAbstractContainer,IJclIntfCollection,IJclIntfList,IJclIntfArray,IJclIntfIterator,TJclIntfBucket,TDynIntfBucketArray, IJclIntfEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AInterface,IInterface,GetObject,SetObject)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRINT(TJclIntfBucketArrayIterator,IJclIntfIterator,TJclIntfBucketArrayList,const ,AInterface,IInterface,GetObject,SetObject)}

{$JPPEXPANDMACRO JCLBUCKETARRAYTYPESINT(TJclAnsiStrBucket,TDynAnsiStringArray,,TDynAnsiStrBucketArray)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTINT(TJclAnsiStrBucketArrayList,TJclAnsiStrAbstractCollection,IJclAnsiStrCollection,IJclAnsiStrList,IJclAnsiStrArray,IJclAnsiStrIterator,TJclAnsiStrBucket,TDynAnsiStrBucketArray, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrFlatContainer\, IJclAnsiStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,,const ,AString,AnsiString,GetString,SetString)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRINT(TJclAnsiStrBucketArrayIterator,IJclAnsiStrIterator,TJclAnsiStrBucketArrayList,const ,AString,AnsiString,GetString,SetString)}

{$JPPEXPANDMACRO JCLBUCKETARRAYTYPESINT(TJclWideStrBucket,TDynWideStringArray,,TDynWideStrBucketArray)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTINT(TJclWideStrBucketArrayList,TJclWideStrAbstractCollection,IJclWideStrCollection,IJclWideStrList,IJclWideStrArray,IJclWideStrIterator,TJclWideStrBucket,TDynWideStrBucketArray, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrFlatContainer\, IJclWideStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,,const ,AString,WideString,GetString,SetString)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRINT(TJclWideStrBucketArrayIterator,IJclWideStrIterator,TJclWideStrBucketArrayList,const ,AString,WideString,GetString,SetString)}

{$IFDEF SUPPORTS_UNICODE_STRING}
{$JPPEXPANDMACRO JCLBUCKETARRAYTYPESINT(TJclUnicodeStrBucket,TDynUnicodeStringArray,,TDynUnicodeStrBucketArray)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTINT(TJclUnicodeStrBucketArrayList,TJclUnicodeStrAbstractCollection,IJclUnicodeStrCollection,IJclUnicodeStrList,IJclUnicodeStrArray,IJclUnicodeStrIterator,TJclUnicodeStrBucket,TDynUnicodeStrBucketArray, IJclStrContainer\, IJclUnicodeStrContainer\, IJclUnicodeStrFlatContainer\, IJclUnicodeStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,,const ,AString,UnicodeString,GetString,SetString)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRINT(TJclUnicodeStrBucketArrayIterator,IJclUnicodeStrIterator,TJclUnicodeStrBucketArrayList,const ,AString,UnicodeString,GetString,SetString)}
{$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrBucketArrayList = TJclAnsiStrBucketArrayList;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrBucketArrayList = TJclWideStrBucketArrayList;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrBucketArrayList = TJclUnicodeStrBucketArrayList;
  {$ENDIF CONTAINER_UNICODESTR}

{$JPPEXPANDMACRO JCLBUCKETARRAYTYPESINT(TJclSingleBucket,TDynSingleArray,,TDynSingleBucketArray)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTINT(TJclSingleBucketArrayList,TJclSingleAbstractContainer,IJclSingleCollection,IJclSingleList,IJclSingleArray,IJclSingleIterator,TJclSingleBucket,TDynSingleBucketArray, IJclSingleContainer\, IJclSingleEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Single,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRINT(TJclSingleBucketArrayIterator,IJclSingleIterator,TJclSingleBucketArrayList,const ,AValue,Single,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYTYPESINT(TJclDoubleBucket,TDynDoubleArray,,TDynDoubleBucketArray)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTINT(TJclDoubleBucketArrayList,TJclDoubleAbstractContainer,IJclDoubleCollection,IJclDoubleList,IJclDoubleArray,IJclDoubleIterator,TJclDoubleBucket,TDynDoubleBucketArray, IJclDoubleContainer\, IJclDoubleEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Double,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRINT(TJclDoubleBucketArrayIterator,IJclDoubleIterator,TJclDoubleBucketArrayList,const ,AValue,Double,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYTYPESINT(TJclExtendedBucket,TDynExtendedArray,,TDynExtendedBucketArray)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTINT(TJclExtendedBucketArrayList,TJclExtendedAbstractContainer,IJclExtendedCollection,IJclExtendedList,IJclExtendedArray,IJclExtendedIterator,TJclExtendedBucket,TDynExtendedBucketArray, IJclExtendedContainer\, IJclExtendedEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Extended,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRINT(TJclExtendedBucketArrayIterator,IJclExtendedIterator,TJclExtendedBucketArrayList,const ,AValue,Extended,GetValue,SetValue)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatBucketArrayList = TJclExtendedBucketArrayList;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatBucketArrayList = TJclDoubleBucketArrayList;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatBucketArrayList = TJclSingleBucketArrayList;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO JCLBUCKETARRAYTYPESINT(TJclIntegerBucket,TDynIntegerArray,,TDynIntegerBucketArray)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTINT(TJclIntegerBucketArrayList,TJclIntegerAbstractContainer,IJclIntegerCollection,IJclIntegerList,IJclIntegerArray,IJclIntegerIterator,TJclIntegerBucket,TDynIntegerBucketArray, IJclIntegerEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,AValue,Integer,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRINT(TJclIntegerBucketArrayIterator,IJclIntegerIterator,TJclIntegerBucketArrayList,,AValue,Integer,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYTYPESINT(TJclCardinalBucket,TDynCardinalArray,,TDynCardinalBucketArray)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTINT(TJclCardinalBucketArrayList,TJclCardinalAbstractContainer,IJclCardinalCollection,IJclCardinalList,IJclCardinalArray,IJclCardinalIterator,TJclCardinalBucket,TDynCardinalBucketArray, IJclCardinalEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,AValue,Cardinal,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRINT(TJclCardinalBucketArrayIterator,IJclCardinalIterator,TJclCardinalBucketArrayList,,AValue,Cardinal,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYTYPESINT(TJclInt64Bucket,TDynInt64Array,,TDynInt64BucketArray)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTINT(TJclInt64BucketArrayList,TJclInt64AbstractContainer,IJclInt64Collection,IJclInt64List,IJclInt64Array,IJclInt64Iterator,TJclInt64Bucket,TDynInt64BucketArray, IJclInt64EqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Int64,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRINT(TJclInt64BucketArrayIterator,IJclInt64Iterator,TJclInt64BucketArrayList,const ,AValue,Int64,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYTYPESINT(TJclPtrBucket,TDynPointerArray,,TDynPtrBucketArray)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTINT(TJclPtrBucketArrayList,TJclPtrAbstractContainer,IJclPtrCollection,IJclPtrList,IJclPtrArray,IJclPtrIterator,TJclPtrBucket,TDynPtrBucketArray, IJclPtrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,APtr,Pointer,GetPointer,SetPointer)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRINT(TJclPtrBucketArrayIterator,IJclPtrIterator,TJclPtrBucketArrayList,,APtr,Pointer,GetPointer,SetPointer)}

{$JPPEXPANDMACRO JCLBUCKETARRAYTYPESINT(TJclBucket,TDynObjectArray,,TDynBucketArray)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTINT(TJclBucketArrayList,TJclAbstractContainer,IJclCollection,IJclList,IJclArray,IJclIterator,TJclBucket,TDynBucketArray, IJclObjectOwner\, IJclEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,; AOwnsObjects: Boolean,,AObject,TObject,GetObject,SetObject)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRINT(TJclBucketArrayIterator,IJclIterator,TJclBucketArrayList,,AObject,TObject,GetObject,SetObject)}

  {$IFDEF _SUPPORTS_GENERICS}
{$JPPEXPANDMACRO JCLBUCKETARRAYTYPESINT(TJclBucket<T>,TDynArray,
  protected
    type
      TDynArray = array of T;,TDynBucketArray<T>)}

  TJclBucketArrayIterator<T> = class;

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTINT(TJclBucketArrayList<T>,TJclAbstractContainer<T>,IJclCollection<T>,IJclList<T>,IJclArray<T>,IJclIterator<T>,TJclBucket<T>,TDynBucketArray<T>, IJclItemOwner<T>\, IJclEqualityComparer<T>\,,
  protected
    type
      TBucketArrayIterator = TJclBucketArrayIterator<T>;
    procedure MoveArray(var List: TJclBucket<T>.TDynArray; FromIndex, ToIndex, Count: Integer);,,; AOwnsItems: Boolean,const ,AItem,T,GetItem,SetItem)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRINT(TJclBucketArrayIterator<T>,IJclIterator<T>,TJclBucketArrayList<T>,const ,AItem,T,GetItem,SetItem)}

  // E = External helper to compare items for equality
  // GetHashCode is not used
  TJclBucketArrayListE<T> = class(TJclBucketArrayList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>)
  private
    FEqualityComparer: IJclEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityComparer: IJclEqualityComparer<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const AEqualityComparer: IJclEqualityComparer<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;

    property EqualityComparer: IJclEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclBucketArrayListF<T> = class(TJclBucketArrayList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;
  end;

  // I = Items can compare themselves to others
  TJclBucketArrayListI<T: IEquatable<T>> = class(TJclBucketArrayList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>)
  protected
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  end;

  {$ENDIF SUPPORTS_GENERICS}

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

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTIMP(TJclIntfBucketArrayList,,,IJclIntfCollection,IJclIntfIterator,TJclIntfBucketArrayIterator,IJclIntfList,TJclIntfBucket,const ,AInterface,GetObject,SetObject,FreeObject,IInterface,nil)}

function TJclIntfBucketArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfBucketArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRIMP(TJclIntfBucketArrayIterator,IJclIntfIterator,TJclIntfBucketArrayList,TJclIntfBucket,const ,AInterface,IInterface,GetObject,SetObject)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTIMP(TJclAnsiStrBucketArrayList,,,IJclAnsiStrCollection,IJclAnsiStrIterator,TJclAnsiStrBucketArrayIterator,IJclAnsiStrList,TJclAnsiStrBucket,const ,AString,GetString,SetString,FreeString,AnsiString,'')}

function TJclAnsiStrBucketArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrBucketArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRIMP(TJclAnsiStrBucketArrayIterator,IJclAnsiStrIterator,TJclAnsiStrBucketArrayList,TJclAnsiStrBucket,const ,AString,AnsiString,GetString,SetString)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTIMP(TJclWideStrBucketArrayList,,,IJclWideStrCollection,IJclWideStrIterator,TJclWideStrBucketArrayIterator,IJclWideStrList,TJclWideStrBucket,const ,AString,GetString,SetString,FreeString,WideString,'')}

function TJclWideStrBucketArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrBucketArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRIMP(TJclWideStrBucketArrayIterator,IJclWideStrIterator,TJclWideStrBucketArrayList,TJclWideStrBucket,const ,AString,WideString,GetString,SetString)}

{$IFDEF SUPPORTS_UNICODE_STRING}
{$JPPEXPANDMACRO JCLBUCKETARRAYLISTIMP(TJclUnicodeStrBucketArrayList,,,IJclUnicodeStrCollection,IJclUnicodeStrIterator,TJclUnicodeStrBucketArrayIterator,IJclUnicodeStrList,TJclUnicodeStrBucket,const ,AString,GetString,SetString,FreeString,UnicodeString,'')}

function TJclUnicodeStrBucketArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrBucketArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRIMP(TJclUnicodeStrBucketArrayIterator,IJclUnicodeStrIterator,TJclUnicodeStrBucketArrayList,TJclUnicodeStrBucket,const ,AString,UnicodeString,GetString,SetString)}
{$ENDIF SUPPORTS_UNICODE_STRING}


{$JPPEXPANDMACRO JCLBUCKETARRAYLISTIMP(TJclSingleBucketArrayList,,,IJclSingleCollection,IJclSingleIterator,TJclSingleBucketArrayIterator,IJclSingleList,TJclSingleBucket,const ,AValue,GetValue,SetValue,FreeSingle,Single,0.0)}

function TJclSingleBucketArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleBucketArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRIMP(TJclSingleBucketArrayIterator,IJclSingleIterator,TJclSingleBucketArrayList,TJclSingleBucket,const ,AValue,Single,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTIMP(TJclDoubleBucketArrayList,,,IJclDoubleCollection,IJclDoubleIterator,TJclDoubleBucketArrayIterator,IJclDoubleList,TJclDoubleBucket,const ,AValue,GetValue,SetValue,FreeDouble,Double,0.0)}

function TJclDoubleBucketArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleBucketArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRIMP(TJclDoubleBucketArrayIterator,IJclDoubleIterator,TJclDoubleBucketArrayList,TJclDoubleBucket,const ,AValue,Double,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTIMP(TJclExtendedBucketArrayList,,,IJclExtendedCollection,IJclExtendedIterator,TJclExtendedBucketArrayIterator,IJclExtendedList,TJclExtendedBucket,const ,AValue,GetValue,SetValue,FreeExtended,Extended,0.0)}

function TJclExtendedBucketArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedBucketArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRIMP(TJclExtendedBucketArrayIterator,IJclExtendedIterator,TJclExtendedBucketArrayList,TJclExtendedBucket,const ,AValue,Extended,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTIMP(TJclIntegerBucketArrayList,,,IJclIntegerCollection,IJclIntegerIterator,TJclIntegerBucketArrayIterator,IJclIntegerList,TJclIntegerBucket,,AValue,GetValue,SetValue,FreeInteger,Integer,0)}

function TJclIntegerBucketArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerBucketArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRIMP(TJclIntegerBucketArrayIterator,IJclIntegerIterator,TJclIntegerBucketArrayList,TJclIntegerBucket,,AValue,Integer,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTIMP(TJclCardinalBucketArrayList,,,IJclCardinalCollection,IJclCardinalIterator,TJclCardinalBucketArrayIterator,IJclCardinalList,TJclCardinalBucket,,AValue,GetValue,SetValue,FreeCardinal,Cardinal,0)}

function TJclCardinalBucketArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalBucketArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRIMP(TJclCardinalBucketArrayIterator,IJclCardinalIterator,TJclCardinalBucketArrayList,TJclCardinalBucket,,AValue,Cardinal,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTIMP(TJclInt64BucketArrayList,,,IJclInt64Collection,IJclInt64Iterator,TJclInt64BucketArrayIterator,IJclInt64List,TJclInt64Bucket,const ,AValue,GetValue,SetValue,FreeInt64,Int64,0)}

function TJclInt64BucketArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64BucketArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRIMP(TJclInt64BucketArrayIterator,IJclInt64Iterator,TJclInt64BucketArrayList,TJclInt64Bucket,const ,AValue,Int64,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTIMP(TJclPtrBucketArrayList,,,IJclPtrCollection,IJclPtrIterator,TJclPtrBucketArrayIterator,IJclPtrList,TJclPtrBucket,,APtr,GetPointer,SetPointer,FreePointer,Pointer,nil)}

function TJclPtrBucketArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrBucketArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRIMP(TJclPtrBucketArrayIterator,IJclPtrIterator,TJclPtrBucketArrayList,TJclPtrBucket,,APtr,Pointer,GetPointer,SetPointer)}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTIMP(TJclBucketArrayList,; AOwnsObjects: Boolean,AOwnsObjects,IJclCollection,IJclIterator,TJclBucketArrayIterator,IJclList,TJclBucket,,AObject,GetObject,SetObject,FreeObject,TObject,nil)}

function TJclBucketArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBucketArrayList.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRIMP(TJclBucketArrayIterator,IJclIterator,TJclBucketArrayList,TJclBucket,,AObject,TObject,GetObject,SetObject)}

{$IFDEF _SUPPORTS_GENERICS}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTIMP(TJclBucketArrayList<T>,; AOwnsItems: Boolean,AOwnsItems,IJclCollection<T>,IJclIterator<T>,TBucketArrayIterator,IJclList<T>,TJclBucket<T>,const ,AItem,GetItem,SetItem,FreeItem,T,Default(T))}

{$JPPEXPANDMACRO JCLBUCKETARRAYLISTITRIMP(TJclBucketArrayIterator<T>,IJclIterator<T>,TJclBucketArrayList<T>,TJclBucket<T>,const ,AItem,T,GetItem,SetItem)}

procedure TJclBucketArrayList<T>.MoveArray(var List: TJclBucket<T>.TDynArray; FromIndex, ToIndex, Count: Integer);
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];

    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
        List[FromIndex + I] := Default(T)
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := Default(T);
  end
  else
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];

    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
        List[FromIndex + I] := Default(T)
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := Default(T);
  end; 
end;

//=== { TJclBucketArrayListE<T> } ==================================================

constructor TJclBucketArrayListE<T>.Create(const AEqualityComparer: IJclEqualityComparer<T>; ACapacity: Integer;
  AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

constructor TJclBucketArrayListE<T>.Create(const AEqualityComparer: IJclEqualityComparer<T>;
  const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclBucketArrayListE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclBucketArrayListE<T> then
    TJclBucketArrayListE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclBucketArrayListE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBucketArrayListE<T>.Create(EqualityComparer, FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclBucketArrayListE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.ItemsEqual(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclBucketArrayListF<T> } ==================================================

constructor TJclBucketArrayListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
end;

constructor TJclBucketArrayListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
end;

function TJclBucketArrayListF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBucketArrayListF<T>.Create(EqualityCompare, FSize, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclBucketArrayListI<T> } ==================================================

function TJclBucketArrayListI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBucketArrayListI<T>.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclBucketArrayListI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := A.Equals(B);
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

