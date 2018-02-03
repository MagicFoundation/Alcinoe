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


type
  TItrStart = (isFirst, isLast);

  TJclIntfHashSetBucket = class
  public
    Size: Integer;
    Entries: TDynIInterfaceArray;
  end;

  TJclIntfHashSet = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclIntfContainer, IJclIntfFlatContainer, IJclIntfEqualityComparer, IJclIntfHashConverter,
    IJclIntfCollection, IJclIntfSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FBuckets: array of TJclIntfHashSetBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCollection }
    function Add(const AInterface: IInterface): Boolean;
    function AddAll(const ACollection: IJclIntfCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclIntfCollection): Boolean;
    function Contains(const AInterface: IInterface): Boolean;
    function ContainsAll(const ACollection: IJclIntfCollection): Boolean;
    function Extract(const AInterface: IInterface): Boolean;
    function ExtractAll(const ACollection: IJclIntfCollection): Boolean;
    function First: IJclIntfIterator;
    function IsEmpty: Boolean;
    function Last: IJclIntfIterator;
    function Remove(const AInterface: IInterface): Boolean;
    function RemoveAll(const ACollection: IJclIntfCollection): Boolean;
    function RetainAll(const ACollection: IJclIntfCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIntfIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntfSet }
    procedure Intersect(const ACollection: IJclIntfCollection);
    procedure Subtract(const ACollection: IJclIntfCollection);
    procedure Union(const ACollection: IJclIntfCollection);
  end;

  TJclIntfHashSetIterator = class(TJclAbstractIterator, IJclIntfIterator)
  protected
    FBucketIndex: Integer;
    FItemIndex: Integer;
    FStart: TItrStart;
    FOwnHashSet: TJclIntfHashSet;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnHashSet: TJclIntfHashSet; ABucketIndex: Integer; AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
    { IJclIntfIterator }
    function Add(const AInterface: IInterface): Boolean;
    procedure Extract;
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AInterface: IInterface): Boolean;
    function IteratorEquals(const AIterator: IJclIntfIterator): Boolean;
    function Next: IInterface;
    function NextIndex: Integer;
    function Previous: IInterface;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetObject(const AInterface: IInterface);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: IInterface read GetObject;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  TJclAnsiStrHashSetBucket = class
  public
    Size: Integer;
    Entries: TDynAnsiStringArray;
  end;

  TJclAnsiStrHashSet = class(TJclAnsiStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclAnsiStrContainer, IJclAnsiStrFlatContainer, IJclAnsiStrEqualityComparer, IJclAnsiStrHashConverter, IJclStrBaseContainer,
    IJclAnsiStrCollection, IJclAnsiStrSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FBuckets: array of TJclAnsiStrHashSetBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrCollection }
    function Add(const AString: AnsiString): Boolean; override;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    procedure Clear; override;
    function CollectionEquals(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Contains(const AString: AnsiString): Boolean; override;
    function ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Extract(const AString: AnsiString): Boolean; override;
    function ExtractAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function First: IJclAnsiStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclAnsiStrIterator; override;
    function Remove(const AString: AnsiString): Boolean; override;
    function RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclAnsiStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclAnsiStrSet }
    procedure Intersect(const ACollection: IJclAnsiStrCollection);
    procedure Subtract(const ACollection: IJclAnsiStrCollection);
    procedure Union(const ACollection: IJclAnsiStrCollection);
  end;

  TJclAnsiStrHashSetIterator = class(TJclAbstractIterator, IJclAnsiStrIterator)
  protected
    FBucketIndex: Integer;
    FItemIndex: Integer;
    FStart: TItrStart;
    FOwnHashSet: TJclAnsiStrHashSet;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnHashSet: TJclAnsiStrHashSet; ABucketIndex: Integer; AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
    { IJclAnsiStrIterator }
    function Add(const AString: AnsiString): Boolean;
    procedure Extract;
    function GetString: AnsiString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: AnsiString): Boolean;
    function IteratorEquals(const AIterator: IJclAnsiStrIterator): Boolean;
    function Next: AnsiString;
    function NextIndex: Integer;
    function Previous: AnsiString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetString(const AString: AnsiString);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: AnsiString read GetString;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  TJclWideStrHashSetBucket = class
  public
    Size: Integer;
    Entries: TDynWideStringArray;
  end;

  TJclWideStrHashSet = class(TJclWideStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclWideStrContainer, IJclWideStrFlatContainer, IJclWideStrEqualityComparer, IJclWideStrHashConverter, IJclStrBaseContainer,
    IJclWideStrCollection, IJclWideStrSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FBuckets: array of TJclWideStrHashSetBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrCollection }
    function Add(const AString: WideString): Boolean; override;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean; override;
    procedure Clear; override;
    function CollectionEquals(const ACollection: IJclWideStrCollection): Boolean; override;
    function Contains(const AString: WideString): Boolean; override;
    function ContainsAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Extract(const AString: WideString): Boolean; override;
    function ExtractAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function First: IJclWideStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclWideStrIterator; override;
    function Remove(const AString: WideString): Boolean; override;
    function RemoveAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclWideStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclWideStrSet }
    procedure Intersect(const ACollection: IJclWideStrCollection);
    procedure Subtract(const ACollection: IJclWideStrCollection);
    procedure Union(const ACollection: IJclWideStrCollection);
  end;

  TJclWideStrHashSetIterator = class(TJclAbstractIterator, IJclWideStrIterator)
  protected
    FBucketIndex: Integer;
    FItemIndex: Integer;
    FStart: TItrStart;
    FOwnHashSet: TJclWideStrHashSet;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnHashSet: TJclWideStrHashSet; ABucketIndex: Integer; AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
    { IJclWideStrIterator }
    function Add(const AString: WideString): Boolean;
    procedure Extract;
    function GetString: WideString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: WideString): Boolean;
    function IteratorEquals(const AIterator: IJclWideStrIterator): Boolean;
    function Next: WideString;
    function NextIndex: Integer;
    function Previous: WideString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetString(const AString: WideString);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: WideString read GetString;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrHashSetBucket = class
  public
    Size: Integer;
    Entries: TDynUnicodeStringArray;
  end;

  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrHashSet = class(TJclUnicodeStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclUnicodeStrContainer, IJclUnicodeStrFlatContainer, IJclUnicodeStrEqualityComparer, IJclUnicodeStrHashConverter, IJclStrBaseContainer,
    IJclUnicodeStrCollection, IJclUnicodeStrSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FBuckets: array of TJclUnicodeStrHashSetBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclUnicodeStrCollection }
    function Add(const AString: UnicodeString): Boolean; override;
    function AddAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    procedure Clear; override;
    function CollectionEquals(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function Contains(const AString: UnicodeString): Boolean; override;
    function ContainsAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function Extract(const AString: UnicodeString): Boolean; override;
    function ExtractAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function First: IJclUnicodeStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclUnicodeStrIterator; override;
    function Remove(const AString: UnicodeString): Boolean; override;
    function RemoveAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclUnicodeStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclUnicodeStrSet }
    procedure Intersect(const ACollection: IJclUnicodeStrCollection);
    procedure Subtract(const ACollection: IJclUnicodeStrCollection);
    procedure Union(const ACollection: IJclUnicodeStrCollection);
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrHashSetIterator = class(TJclAbstractIterator, IJclUnicodeStrIterator)
  protected
    FBucketIndex: Integer;
    FItemIndex: Integer;
    FStart: TItrStart;
    FOwnHashSet: TJclUnicodeStrHashSet;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnHashSet: TJclUnicodeStrHashSet; ABucketIndex: Integer; AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
    { IJclUnicodeStrIterator }
    function Add(const AString: UnicodeString): Boolean;
    procedure Extract;
    function GetString: UnicodeString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: UnicodeString): Boolean;
    function IteratorEquals(const AIterator: IJclUnicodeStrIterator): Boolean;
    function Next: UnicodeString;
    function NextIndex: Integer;
    function Previous: UnicodeString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetString(const AString: UnicodeString);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: UnicodeString read GetString;
    {$ENDIF SUPPORTS_FOR_IN}
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrHashSetBucket = TJclAnsiStrHashSetBucket;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrHashSetBucket = TJclWideStrHashSetBucket;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrHashSetBucket = TJclUnicodeStrHashSetBucket;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrHashSet = TJclAnsiStrHashSet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrHashSet = TJclWideStrHashSet;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrHashSet = TJclUnicodeStrHashSet;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrHashSetIterator = TJclAnsiStrHashSetIterator;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrHashSetIterator = TJclWideStrHashSetIterator;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrHashSetIterator = TJclUnicodeStrHashSetIterator;
  {$ENDIF CONTAINER_UNICODESTR}

  TJclSingleHashSetBucket = class
  public
    Size: Integer;
    Entries: TDynSingleArray;
  end;

  TJclSingleHashSet = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclSingleContainer, IJclSingleFlatContainer, IJclSingleEqualityComparer, IJclSingleHashConverter,
    IJclSingleCollection, IJclSingleSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FBuckets: array of TJclSingleHashSetBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclSingleCollection }
    function Add(const AValue: Single): Boolean;
    function AddAll(const ACollection: IJclSingleCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclSingleCollection): Boolean;
    function Contains(const AValue: Single): Boolean;
    function ContainsAll(const ACollection: IJclSingleCollection): Boolean;
    function Extract(const AValue: Single): Boolean;
    function ExtractAll(const ACollection: IJclSingleCollection): Boolean;
    function First: IJclSingleIterator;
    function IsEmpty: Boolean;
    function Last: IJclSingleIterator;
    function Remove(const AValue: Single): Boolean;
    function RemoveAll(const ACollection: IJclSingleCollection): Boolean;
    function RetainAll(const ACollection: IJclSingleCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclSingleIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclSingleSet }
    procedure Intersect(const ACollection: IJclSingleCollection);
    procedure Subtract(const ACollection: IJclSingleCollection);
    procedure Union(const ACollection: IJclSingleCollection);
  end;

  TJclSingleHashSetIterator = class(TJclAbstractIterator, IJclSingleIterator)
  protected
    FBucketIndex: Integer;
    FItemIndex: Integer;
    FStart: TItrStart;
    FOwnHashSet: TJclSingleHashSet;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnHashSet: TJclSingleHashSet; ABucketIndex: Integer; AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
    { IJclSingleIterator }
    function Add(const AValue: Single): Boolean;
    procedure Extract;
    function GetValue: Single;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Single): Boolean;
    function IteratorEquals(const AIterator: IJclSingleIterator): Boolean;
    function Next: Single;
    function NextIndex: Integer;
    function Previous: Single;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Single);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Single read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  TJclDoubleHashSetBucket = class
  public
    Size: Integer;
    Entries: TDynDoubleArray;
  end;

  TJclDoubleHashSet = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclDoubleContainer, IJclDoubleFlatContainer, IJclDoubleEqualityComparer, IJclDoubleHashConverter,
    IJclDoubleCollection, IJclDoubleSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FBuckets: array of TJclDoubleHashSetBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclDoubleCollection }
    function Add(const AValue: Double): Boolean;
    function AddAll(const ACollection: IJclDoubleCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclDoubleCollection): Boolean;
    function Contains(const AValue: Double): Boolean;
    function ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
    function Extract(const AValue: Double): Boolean;
    function ExtractAll(const ACollection: IJclDoubleCollection): Boolean;
    function First: IJclDoubleIterator;
    function IsEmpty: Boolean;
    function Last: IJclDoubleIterator;
    function Remove(const AValue: Double): Boolean;
    function RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
    function RetainAll(const ACollection: IJclDoubleCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclDoubleIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclDoubleSet }
    procedure Intersect(const ACollection: IJclDoubleCollection);
    procedure Subtract(const ACollection: IJclDoubleCollection);
    procedure Union(const ACollection: IJclDoubleCollection);
  end;

  TJclDoubleHashSetIterator = class(TJclAbstractIterator, IJclDoubleIterator)
  protected
    FBucketIndex: Integer;
    FItemIndex: Integer;
    FStart: TItrStart;
    FOwnHashSet: TJclDoubleHashSet;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnHashSet: TJclDoubleHashSet; ABucketIndex: Integer; AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
    { IJclDoubleIterator }
    function Add(const AValue: Double): Boolean;
    procedure Extract;
    function GetValue: Double;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Double): Boolean;
    function IteratorEquals(const AIterator: IJclDoubleIterator): Boolean;
    function Next: Double;
    function NextIndex: Integer;
    function Previous: Double;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Double);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Double read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  TJclExtendedHashSetBucket = class
  public
    Size: Integer;
    Entries: TDynExtendedArray;
  end;

  TJclExtendedHashSet = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclExtendedContainer, IJclExtendedFlatContainer, IJclExtendedEqualityComparer, IJclExtendedHashConverter,
    IJclExtendedCollection, IJclExtendedSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FBuckets: array of TJclExtendedHashSetBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclExtendedCollection }
    function Add(const AValue: Extended): Boolean;
    function AddAll(const ACollection: IJclExtendedCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclExtendedCollection): Boolean;
    function Contains(const AValue: Extended): Boolean;
    function ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
    function Extract(const AValue: Extended): Boolean;
    function ExtractAll(const ACollection: IJclExtendedCollection): Boolean;
    function First: IJclExtendedIterator;
    function IsEmpty: Boolean;
    function Last: IJclExtendedIterator;
    function Remove(const AValue: Extended): Boolean;
    function RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
    function RetainAll(const ACollection: IJclExtendedCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclExtendedIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclExtendedSet }
    procedure Intersect(const ACollection: IJclExtendedCollection);
    procedure Subtract(const ACollection: IJclExtendedCollection);
    procedure Union(const ACollection: IJclExtendedCollection);
  end;

  TJclExtendedHashSetIterator = class(TJclAbstractIterator, IJclExtendedIterator)
  protected
    FBucketIndex: Integer;
    FItemIndex: Integer;
    FStart: TItrStart;
    FOwnHashSet: TJclExtendedHashSet;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnHashSet: TJclExtendedHashSet; ABucketIndex: Integer; AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
    { IJclExtendedIterator }
    function Add(const AValue: Extended): Boolean;
    procedure Extract;
    function GetValue: Extended;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Extended): Boolean;
    function IteratorEquals(const AIterator: IJclExtendedIterator): Boolean;
    function Next: Extended;
    function NextIndex: Integer;
    function Previous: Extended;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Extended);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Extended read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatHashSetBucket = TJclSingleHashSetBucket;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatHashSetBucket = TJclDoubleHashSetBucket;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatHashSetBucket = TJclExtendedHashSetBucket;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatHashSet = TJclSingleHashSet;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatHashSet = TJclDoubleHashSet;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatHashSet = TJclExtendedHashSet;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatHashSetIterator = TJclSingleHashSetIterator;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatHashSetIterator = TJclDoubleHashSetIterator;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatHashSetIterator = TJclExtendedHashSetIterator;
  {$ENDIF MATH_EXTENDED_PRECISION}

  TJclIntegerHashSetBucket = class
  public
    Size: Integer;
    Entries: TDynIntegerArray;
  end;

  TJclIntegerHashSet = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclIntegerContainer, IJclIntegerFlatContainer, IJclIntegerEqualityComparer, IJclIntegerHashConverter,
    IJclIntegerCollection, IJclIntegerSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FBuckets: array of TJclIntegerHashSetBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntegerCollection }
    function Add(AValue: Integer): Boolean;
    function AddAll(const ACollection: IJclIntegerCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclIntegerCollection): Boolean;
    function Contains(AValue: Integer): Boolean;
    function ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
    function Extract(AValue: Integer): Boolean;
    function ExtractAll(const ACollection: IJclIntegerCollection): Boolean;
    function First: IJclIntegerIterator;
    function IsEmpty: Boolean;
    function Last: IJclIntegerIterator;
    function Remove(AValue: Integer): Boolean;
    function RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
    function RetainAll(const ACollection: IJclIntegerCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIntegerIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntegerSet }
    procedure Intersect(const ACollection: IJclIntegerCollection);
    procedure Subtract(const ACollection: IJclIntegerCollection);
    procedure Union(const ACollection: IJclIntegerCollection);
  end;

  TJclIntegerHashSetIterator = class(TJclAbstractIterator, IJclIntegerIterator)
  protected
    FBucketIndex: Integer;
    FItemIndex: Integer;
    FStart: TItrStart;
    FOwnHashSet: TJclIntegerHashSet;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnHashSet: TJclIntegerHashSet; ABucketIndex: Integer; AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
    { IJclIntegerIterator }
    function Add(AValue: Integer): Boolean;
    procedure Extract;
    function GetValue: Integer;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AValue: Integer): Boolean;
    function IteratorEquals(const AIterator: IJclIntegerIterator): Boolean;
    function Next: Integer;
    function NextIndex: Integer;
    function Previous: Integer;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(AValue: Integer);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Integer read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  TJclCardinalHashSetBucket = class
  public
    Size: Integer;
    Entries: TDynCardinalArray;
  end;

  TJclCardinalHashSet = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclCardinalContainer, IJclCardinalFlatContainer, IJclCardinalEqualityComparer, IJclCardinalHashConverter,
    IJclCardinalCollection, IJclCardinalSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FBuckets: array of TJclCardinalHashSetBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCardinalCollection }
    function Add(AValue: Cardinal): Boolean;
    function AddAll(const ACollection: IJclCardinalCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclCardinalCollection): Boolean;
    function Contains(AValue: Cardinal): Boolean;
    function ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
    function Extract(AValue: Cardinal): Boolean;
    function ExtractAll(const ACollection: IJclCardinalCollection): Boolean;
    function First: IJclCardinalIterator;
    function IsEmpty: Boolean;
    function Last: IJclCardinalIterator;
    function Remove(AValue: Cardinal): Boolean;
    function RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
    function RetainAll(const ACollection: IJclCardinalCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclCardinalIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCardinalSet }
    procedure Intersect(const ACollection: IJclCardinalCollection);
    procedure Subtract(const ACollection: IJclCardinalCollection);
    procedure Union(const ACollection: IJclCardinalCollection);
  end;

  TJclCardinalHashSetIterator = class(TJclAbstractIterator, IJclCardinalIterator)
  protected
    FBucketIndex: Integer;
    FItemIndex: Integer;
    FStart: TItrStart;
    FOwnHashSet: TJclCardinalHashSet;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnHashSet: TJclCardinalHashSet; ABucketIndex: Integer; AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
    { IJclCardinalIterator }
    function Add(AValue: Cardinal): Boolean;
    procedure Extract;
    function GetValue: Cardinal;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AValue: Cardinal): Boolean;
    function IteratorEquals(const AIterator: IJclCardinalIterator): Boolean;
    function Next: Cardinal;
    function NextIndex: Integer;
    function Previous: Cardinal;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(AValue: Cardinal);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Cardinal read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  TJclInt64HashSetBucket = class
  public
    Size: Integer;
    Entries: TDynInt64Array;
  end;

  TJclInt64HashSet = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclInt64Container, IJclInt64FlatContainer, IJclInt64EqualityComparer, IJclInt64HashConverter,
    IJclInt64Collection, IJclInt64Set)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FBuckets: array of TJclInt64HashSetBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclInt64Collection }
    function Add(const AValue: Int64): Boolean;
    function AddAll(const ACollection: IJclInt64Collection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclInt64Collection): Boolean;
    function Contains(const AValue: Int64): Boolean;
    function ContainsAll(const ACollection: IJclInt64Collection): Boolean;
    function Extract(const AValue: Int64): Boolean;
    function ExtractAll(const ACollection: IJclInt64Collection): Boolean;
    function First: IJclInt64Iterator;
    function IsEmpty: Boolean;
    function Last: IJclInt64Iterator;
    function Remove(const AValue: Int64): Boolean;
    function RemoveAll(const ACollection: IJclInt64Collection): Boolean;
    function RetainAll(const ACollection: IJclInt64Collection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclInt64Iterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclInt64Set }
    procedure Intersect(const ACollection: IJclInt64Collection);
    procedure Subtract(const ACollection: IJclInt64Collection);
    procedure Union(const ACollection: IJclInt64Collection);
  end;

  TJclInt64HashSetIterator = class(TJclAbstractIterator, IJclInt64Iterator)
  protected
    FBucketIndex: Integer;
    FItemIndex: Integer;
    FStart: TItrStart;
    FOwnHashSet: TJclInt64HashSet;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnHashSet: TJclInt64HashSet; ABucketIndex: Integer; AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
    { IJclInt64Iterator }
    function Add(const AValue: Int64): Boolean;
    procedure Extract;
    function GetValue: Int64;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Int64): Boolean;
    function IteratorEquals(const AIterator: IJclInt64Iterator): Boolean;
    function Next: Int64;
    function NextIndex: Integer;
    function Previous: Int64;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Int64);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Int64 read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  TJclPtrHashSetBucket = class
  public
    Size: Integer;
    Entries: TDynPointerArray;
  end;

  TJclPtrHashSet = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclPtrContainer, IJclPtrFlatContainer, IJclPtrEqualityComparer, IJclPtrHashConverter,
    IJclPtrCollection, IJclPtrSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FBuckets: array of TJclPtrHashSetBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclPtrCollection }
    function Add(APtr: Pointer): Boolean;
    function AddAll(const ACollection: IJclPtrCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclPtrCollection): Boolean;
    function Contains(APtr: Pointer): Boolean;
    function ContainsAll(const ACollection: IJclPtrCollection): Boolean;
    function Extract(APtr: Pointer): Boolean;
    function ExtractAll(const ACollection: IJclPtrCollection): Boolean;
    function First: IJclPtrIterator;
    function IsEmpty: Boolean;
    function Last: IJclPtrIterator;
    function Remove(APtr: Pointer): Boolean;
    function RemoveAll(const ACollection: IJclPtrCollection): Boolean;
    function RetainAll(const ACollection: IJclPtrCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclPtrIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclPtrSet }
    procedure Intersect(const ACollection: IJclPtrCollection);
    procedure Subtract(const ACollection: IJclPtrCollection);
    procedure Union(const ACollection: IJclPtrCollection);
  end;

  TJclPtrHashSetIterator = class(TJclAbstractIterator, IJclPtrIterator)
  protected
    FBucketIndex: Integer;
    FItemIndex: Integer;
    FStart: TItrStart;
    FOwnHashSet: TJclPtrHashSet;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnHashSet: TJclPtrHashSet; ABucketIndex: Integer; AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
    { IJclPtrIterator }
    function Add(APtr: Pointer): Boolean;
    procedure Extract;
    function GetPointer: Pointer;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(APtr: Pointer): Boolean;
    function IteratorEquals(const AIterator: IJclPtrIterator): Boolean;
    function Next: Pointer;
    function NextIndex: Integer;
    function Previous: Pointer;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetPointer(APtr: Pointer);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Pointer read GetPointer;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  TJclHashSetBucket = class
  public
    Size: Integer;
    Entries: TDynObjectArray;
  end;

  TJclHashSet = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclContainer, IJclFlatContainer, IJclEqualityComparer, IJclHashConverter, IJclObjectOwner,
    IJclCollection, IJclSet)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FBuckets: array of TJclHashSetBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsObjects: Boolean); overload;
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCollection }
    function Add(AObject: TObject): Boolean;
    function AddAll(const ACollection: IJclCollection): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclCollection): Boolean;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(const ACollection: IJclCollection): Boolean;
    function Extract(AObject: TObject): Boolean;
    function ExtractAll(const ACollection: IJclCollection): Boolean;
    function First: IJclIterator;
    function IsEmpty: Boolean;
    function Last: IJclIterator;
    function Remove(AObject: TObject): Boolean;
    function RemoveAll(const ACollection: IJclCollection): Boolean;
    function RetainAll(const ACollection: IJclCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclSet }
    procedure Intersect(const ACollection: IJclCollection);
    procedure Subtract(const ACollection: IJclCollection);
    procedure Union(const ACollection: IJclCollection);
  end;

  TJclHashSetIterator = class(TJclAbstractIterator, IJclIterator)
  protected
    FBucketIndex: Integer;
    FItemIndex: Integer;
    FStart: TItrStart;
    FOwnHashSet: TJclHashSet;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnHashSet: TJclHashSet; ABucketIndex: Integer; AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
    { IJclIterator }
    function Add(AObject: TObject): Boolean;
    procedure Extract;
    function GetObject: TObject;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AObject: TObject): Boolean;
    function IteratorEquals(const AIterator: IJclIterator): Boolean;
    function Next: TObject;
    function NextIndex: Integer;
    function Previous: TObject;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetObject(AObject: TObject);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: TObject read GetObject;
    {$ENDIF SUPPORTS_FOR_IN}
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN

  TJclHashSetBucket<T> = class
  public
    type
      TDynArray = array of T;
  public
    Size: Integer;
    Entries: TDynArray;
    procedure MoveArray(var List: TDynArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclHashSet<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclContainer<T>, IJclFlatContainer<T>, IJclEqualityComparer<T>, IJclHashConverter<T>, IJclItemOwner<T>,
    IJclCollection<T>, IJclSet<T>)
  private
    FBuckets: array of TJclHashSetBucket<T>;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsItems: Boolean); overload;
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCollection<T> }
    function Add(const AItem: T): Boolean;
    function AddAll(const ACollection: IJclCollection<T>): Boolean;
    procedure Clear;
    function CollectionEquals(const ACollection: IJclCollection<T>): Boolean;
    function Contains(const AItem: T): Boolean;
    function ContainsAll(const ACollection: IJclCollection<T>): Boolean;
    function Extract(const AItem: T): Boolean;
    function ExtractAll(const ACollection: IJclCollection<T>): Boolean;
    function First: IJclIterator<T>;
    function IsEmpty: Boolean;
    function Last: IJclIterator<T>;
    function Remove(const AItem: T): Boolean;
    function RemoveAll(const ACollection: IJclCollection<T>): Boolean;
    function RetainAll(const ACollection: IJclCollection<T>): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIterator<T>;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclSet<T> }
    procedure Intersect(const ACollection: IJclCollection<T>);
    procedure Subtract(const ACollection: IJclCollection<T>);
    procedure Union(const ACollection: IJclCollection<T>);
  end;

  TJclHashSetIterator<T> = class(TJclAbstractIterator, IJclIterator<T>)
  protected
    FBucketIndex: Integer;
    FItemIndex: Integer;
    FStart: TItrStart;
    FOwnHashSet: TJclHashSet<T>;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnHashSet: TJclHashSet<T>; ABucketIndex: Integer; AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
    { IJclIterator<T> }
    function Add(const AItem: T): Boolean;
    procedure Extract;
    function GetItem: T;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AItem: T): Boolean;
    function IteratorEquals(const AIterator: IJclIterator<T>): Boolean;
    function Next: T;
    function NextIndex: Integer;
    function Previous: T;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetItem(const AItem: T);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: T read GetItem;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

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

//=== { TJclIntfHashSet } ====================================================

constructor TJclIntfHashSet.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntfHashSet.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntfHashSet.Add(const AInterface: IInterface): Boolean;
var
  Index: Integer;
  Bucket: TJclIntfHashSetBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if FAllowDefaultElements or (not ItemsEqual(AInterface, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(AInterface), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if ItemsEqual(Bucket.Entries[I], AInterface) then
            Exit;
      end
      else
      begin
        Bucket := TJclIntfHashSetBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size] := AInterface;
        Inc(Bucket.Size);
        Inc(FSize);
        Result := True;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.AddAll(const ACollection: IJclIntfCollection): Boolean;
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
    Result := True;
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

procedure TJclIntfHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfHashSetBucket;
  ADest: TJclIntfHashSet;
  ACollection: IJclIntfCollection;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfHashSet then
    begin
      ADest := TJclIntfHashSet(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfHashSetBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
            NewBucket.Entries[J] := SelfBucket.Entries[J];
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfCollection, ACollection) then
    begin
      ACollection.Clear;
      ACollection.AddAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashSet.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfHashSet then
    TJclIntfHashSet(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntfHashSet.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
          FreeObject(Bucket.Entries[J]);
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.CollectionEquals(const ACollection: IJclIntfCollection): Boolean;
var
  I, J: Integer;
  It: IJclIntfIterator;
  Bucket: TJclIntfHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if not ItemsEqual(Bucket.Entries[J], It.Next) then
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.Contains(const AInterface: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AInterface), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AInterface) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.Extract(const AInterface: IInterface): Boolean;
var
  Bucket: TJclIntfHashSetBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AInterface), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AInterface) then
        begin
          Result := True;
          Bucket.Entries[I] := nil;
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.ExtractAll(const ACollection: IJclIntfCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.First: IJclIntfIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclIntfHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := 0;
    ABucket := nil;
    while ABucketIndex < FCapacity do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Inc(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := 0
    else
      AItemIndex := -1;
    Result := TJclIntfHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isFirst);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfHashSet.GetEnumerator: IJclIntfIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

procedure TJclIntfHashSet.Intersect(const ACollection: IJclIntfCollection);
begin
  RetainAll(ACollection);
end;

function TJclIntfHashSet.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfHashSet.Last: IJclIntfIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclIntfHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := FCapacity - 1;
    ABucket := nil;
    while ABucketIndex >= 0 do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Dec(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := ABucket.Size - 1
    else
      AItemIndex := -1;
    Result := TJclIntfHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isLast);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashSet.Pack;
var
  I: Integer;
  Bucket: TJclIntfHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.Remove(const AInterface: IInterface): Boolean;
var
  Extracted: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AInterface);
    if Result then
    begin
      Extracted := AInterface;
      FreeObject(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.RetainAll(const ACollection: IJclIntfCollection): Boolean;
var
  I, J, NewCapacity: Integer;
  Bucket: TJclIntfHashSetBucket;
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
    Result := True;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := Bucket.Size - 1 downto 0 do
          if not ACollection.Contains(Bucket.Entries[I]) then
        begin
          Bucket.Entries[J] := FreeObject(Bucket.Entries[J]);
          if J < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, J + 1, J, Bucket.Size - J - 1);
          Dec(Bucket.Size);
          Dec(FSize);
        end;

        NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
        if NewCapacity < Length(Bucket.Entries) then
          SetLength(Bucket.Entries, NewCapacity);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashSet.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.Size: Integer;
begin
  Result := FSize;
end;

procedure TJclIntfHashSet.Subtract(const ACollection: IJclIntfCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclIntfHashSet.Union(const ACollection: IJclIntfCollection);
begin
  AddAll(ACollection);
end;


function TJclIntfHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfHashSet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclIntfHashSetIterator } ============================================

constructor TJclIntfHashSetIterator.Create(AOwnHashSet: TJclIntfHashSet;
  ABucketIndex, AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnHashSet := AOwnHashSet;
  FBucketIndex := ABucketIndex;
  FItemIndex := AItemIndex;
  FStart := AStart;
end;

function TJclIntfHashSetIterator.Add(const AInterface: IInterface): Boolean;
begin
  Result := FOwnHashSet.Add(AInterface);
end;

procedure TJclIntfHashSetIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclIntfHashSetIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclIntfHashSetIterator then
  begin
    ADest := TJclIntfHashSetIterator(Dest);
    ADest.FBucketIndex := FBucketIndex;
    ADest.FItemIndex := FItemIndex;
    ADest.FOwnHashSet := FOwnHashSet;
    ADest.FStart := FStart;
  end;
end;

function TJclIntfHashSetIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclIntfHashSetIterator.Create(FOwnHashSet, FBucketIndex, FItemIndex, Valid, FStart);
end;

procedure TJclIntfHashSetIterator.Extract;
var
  AInterface: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    AInterface := GetObject;
    Valid := False;
    FOwnHashSet.Extract(AInterface);
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSetIterator.GetObject: IInterface;
var
  ABucket: TJclIntfHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := nil;
    ABucket := FOwnHashSet.FBuckets[FBucketIndex - 1];
    if (ABucket <> nil) and (FItemIndex < ABucket.Size) then
      Result := ABucket.Entries[FItemIndex]
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSetIterator.HasNext: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclIntfHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex < FOwnHashSet.FCapacity do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex < (ABucket.Size - 1)) or
          ((not SkipCurrent) and (AItemIndex < ABucket.Size)) then
          Exit;
      end;
      AItemIndex := 0;
      Inc(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSetIterator.HasPrevious: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclIntfHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex >= 0 do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex > 0) or
          ((not SkipCurrent) and (AItemIndex >= 0)) then
          Exit;
      end;
      AItemIndex := MaxInt;
      Dec(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSetIterator.Insert(const AInterface: IInterface): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntfHashSetIterator.IteratorEquals(const AIterator: IJclIntfIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclIntfHashSetIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclIntfHashSetIterator then
  begin
    ItrObj := TJclIntfHashSetIterator(Obj);
    Result := (FOwnHashSet = ItrObj.FOwnHashSet) and (FBucketIndex = ItrObj.FBucketIndex) and (FItemIndex = ItrObj.FItemIndex) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfHashSetIterator.MoveNext: Boolean;
var
  ABucket: TJclIntfHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := (FBucketIndex >= 0) and (FItemIndex >= 0) and
              (FBucketIndex < FOwnHashSet.FCapacity);
    if Result then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      Result := (ABucket <> nil) and (FItemIndex < ABucket.Size);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntfHashSetIterator.Next: IInterface;
var
  ABucket: TJclIntfHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := nil;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSetIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntfHashSetIterator.Previous: IInterface;
var
  ABucket: TJclIntfHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex >= 0 do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if (ABucket <> nil) and (FItemIndex < 0) then
          FItemIndex := ABucket.Size - 1;
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex >= 0) and (FItemIndex < ABucket.Size) then
            Break;
          if (FItemIndex > 0) and (FItemIndex < ABucket.Size) then
          begin
            Dec(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := -1;
        Dec(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := nil;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSetIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfHashSetIterator.Remove;
begin

end;

procedure TJclIntfHashSetIterator.Reset;
var
  ABucket: TJclIntfHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          FBucketIndex := 0;
          ABucket := nil;
          while FBucketIndex < FOwnHashSet.FCapacity do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Inc(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := 0
          else
            FItemIndex := -1;
        end;
      isLast:
        begin
          FBucketIndex := FOwnHashSet.FCapacity - 1;
          ABucket := nil;
          while FBucketIndex >= 0 do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Dec(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := ABucket.Size - 1
          else
            FItemIndex := -1;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashSetIterator.SetObject(const AInterface: IInterface);
begin
  raise EJclOperationNotSupportedError.Create;
end;
//=== { TJclAnsiStrHashSet } ====================================================

constructor TJclAnsiStrHashSet.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclAnsiStrHashSet.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclAnsiStrHashSet.Add(const AString: AnsiString): Boolean;
var
  Index: Integer;
  Bucket: TJclAnsiStrHashSetBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if FAllowDefaultElements or (not ItemsEqual(AString, '')) then
    begin
      Index := FHashToRangeFunction(Hash(AString), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if ItemsEqual(Bucket.Entries[I], AString) then
            Exit;
      end
      else
      begin
        Bucket := TJclAnsiStrHashSetBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size] := AString;
        Inc(Bucket.Size);
        Inc(FSize);
        Result := True;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
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
    Result := True;
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

procedure TJclAnsiStrHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclAnsiStrHashSetBucket;
  ADest: TJclAnsiStrHashSet;
  ACollection: IJclAnsiStrCollection;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclAnsiStrHashSet then
    begin
      ADest := TJclAnsiStrHashSet(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclAnsiStrHashSetBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
            NewBucket.Entries[J] := SelfBucket.Entries[J];
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclAnsiStrCollection, ACollection) then
    begin
      ACollection.Clear;
      ACollection.AddAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashSet.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrHashSet then
    TJclAnsiStrHashSet(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclAnsiStrHashSet.Clear;
var
  I, J: Integer;
  Bucket: TJclAnsiStrHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
          FreeString(Bucket.Entries[J]);
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.CollectionEquals(const ACollection: IJclAnsiStrCollection): Boolean;
var
  I, J: Integer;
  It: IJclAnsiStrIterator;
  Bucket: TJclAnsiStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if not ItemsEqual(Bucket.Entries[J], It.Next) then
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.Contains(const AString: AnsiString): Boolean;
var
  I: Integer;
  Bucket: TJclAnsiStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AString), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AString) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.Extract(const AString: AnsiString): Boolean;
var
  Bucket: TJclAnsiStrHashSetBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AString), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AString) then
        begin
          Result := True;
          Bucket.Entries[I] := '';
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.ExtractAll(const ACollection: IJclAnsiStrCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.First: IJclAnsiStrIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclAnsiStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := 0;
    ABucket := nil;
    while ABucketIndex < FCapacity do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Inc(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := 0
    else
      AItemIndex := -1;
    Result := TJclAnsiStrHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isFirst);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrHashSet.GetEnumerator: IJclAnsiStrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

procedure TJclAnsiStrHashSet.Intersect(const ACollection: IJclAnsiStrCollection);
begin
  RetainAll(ACollection);
end;

function TJclAnsiStrHashSet.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrHashSet.Last: IJclAnsiStrIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclAnsiStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := FCapacity - 1;
    ABucket := nil;
    while ABucketIndex >= 0 do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Dec(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := ABucket.Size - 1
    else
      AItemIndex := -1;
    Result := TJclAnsiStrHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isLast);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashSet.Pack;
var
  I: Integer;
  Bucket: TJclAnsiStrHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.Remove(const AString: AnsiString): Boolean;
var
  Extracted: AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AString);
    if Result then
    begin
      Extracted := AString;
      FreeString(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  I, J, NewCapacity: Integer;
  Bucket: TJclAnsiStrHashSetBucket;
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
    Result := True;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := Bucket.Size - 1 downto 0 do
          if not ACollection.Contains(Bucket.Entries[I]) then
        begin
          Bucket.Entries[J] := FreeString(Bucket.Entries[J]);
          if J < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, J + 1, J, Bucket.Size - J - 1);
          Dec(Bucket.Size);
          Dec(FSize);
        end;

        NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
        if NewCapacity < Length(Bucket.Entries) then
          SetLength(Bucket.Entries, NewCapacity);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashSet.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.Size: Integer;
begin
  Result := FSize;
end;

procedure TJclAnsiStrHashSet.Subtract(const ACollection: IJclAnsiStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclAnsiStrHashSet.Union(const ACollection: IJclAnsiStrCollection);
begin
  AddAll(ACollection);
end;


function TJclAnsiStrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrHashSet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclAnsiStrHashSetIterator } ============================================

constructor TJclAnsiStrHashSetIterator.Create(AOwnHashSet: TJclAnsiStrHashSet;
  ABucketIndex, AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnHashSet := AOwnHashSet;
  FBucketIndex := ABucketIndex;
  FItemIndex := AItemIndex;
  FStart := AStart;
end;

function TJclAnsiStrHashSetIterator.Add(const AString: AnsiString): Boolean;
begin
  Result := FOwnHashSet.Add(AString);
end;

procedure TJclAnsiStrHashSetIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclAnsiStrHashSetIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclAnsiStrHashSetIterator then
  begin
    ADest := TJclAnsiStrHashSetIterator(Dest);
    ADest.FBucketIndex := FBucketIndex;
    ADest.FItemIndex := FItemIndex;
    ADest.FOwnHashSet := FOwnHashSet;
    ADest.FStart := FStart;
  end;
end;

function TJclAnsiStrHashSetIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclAnsiStrHashSetIterator.Create(FOwnHashSet, FBucketIndex, FItemIndex, Valid, FStart);
end;

procedure TJclAnsiStrHashSetIterator.Extract;
var
  AString: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    AString := GetString;
    Valid := False;
    FOwnHashSet.Extract(AString);
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSetIterator.GetString: AnsiString;
var
  ABucket: TJclAnsiStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := '';
    ABucket := FOwnHashSet.FBuckets[FBucketIndex - 1];
    if (ABucket <> nil) and (FItemIndex < ABucket.Size) then
      Result := ABucket.Entries[FItemIndex]
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSetIterator.HasNext: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclAnsiStrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex < FOwnHashSet.FCapacity do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex < (ABucket.Size - 1)) or
          ((not SkipCurrent) and (AItemIndex < ABucket.Size)) then
          Exit;
      end;
      AItemIndex := 0;
      Inc(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSetIterator.HasPrevious: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclAnsiStrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex >= 0 do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex > 0) or
          ((not SkipCurrent) and (AItemIndex >= 0)) then
          Exit;
      end;
      AItemIndex := MaxInt;
      Dec(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSetIterator.Insert(const AString: AnsiString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclAnsiStrHashSetIterator.IteratorEquals(const AIterator: IJclAnsiStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclAnsiStrHashSetIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclAnsiStrHashSetIterator then
  begin
    ItrObj := TJclAnsiStrHashSetIterator(Obj);
    Result := (FOwnHashSet = ItrObj.FOwnHashSet) and (FBucketIndex = ItrObj.FBucketIndex) and (FItemIndex = ItrObj.FItemIndex) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrHashSetIterator.MoveNext: Boolean;
var
  ABucket: TJclAnsiStrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := (FBucketIndex >= 0) and (FItemIndex >= 0) and
              (FBucketIndex < FOwnHashSet.FCapacity);
    if Result then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      Result := (ABucket <> nil) and (FItemIndex < ABucket.Size);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclAnsiStrHashSetIterator.Next: AnsiString;
var
  ABucket: TJclAnsiStrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := '';
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSetIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclAnsiStrHashSetIterator.Previous: AnsiString;
var
  ABucket: TJclAnsiStrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex >= 0 do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if (ABucket <> nil) and (FItemIndex < 0) then
          FItemIndex := ABucket.Size - 1;
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex >= 0) and (FItemIndex < ABucket.Size) then
            Break;
          if (FItemIndex > 0) and (FItemIndex < ABucket.Size) then
          begin
            Dec(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := -1;
        Dec(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := '';
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSetIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrHashSetIterator.Remove;
begin

end;

procedure TJclAnsiStrHashSetIterator.Reset;
var
  ABucket: TJclAnsiStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          FBucketIndex := 0;
          ABucket := nil;
          while FBucketIndex < FOwnHashSet.FCapacity do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Inc(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := 0
          else
            FItemIndex := -1;
        end;
      isLast:
        begin
          FBucketIndex := FOwnHashSet.FCapacity - 1;
          ABucket := nil;
          while FBucketIndex >= 0 do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Dec(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := ABucket.Size - 1
          else
            FItemIndex := -1;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashSetIterator.SetString(const AString: AnsiString);
begin
  raise EJclOperationNotSupportedError.Create;
end;
//=== { TJclWideStrHashSet } ====================================================

constructor TJclWideStrHashSet.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclWideStrHashSet.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclWideStrHashSet.Add(const AString: WideString): Boolean;
var
  Index: Integer;
  Bucket: TJclWideStrHashSetBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if FAllowDefaultElements or (not ItemsEqual(AString, '')) then
    begin
      Index := FHashToRangeFunction(Hash(AString), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if ItemsEqual(Bucket.Entries[I], AString) then
            Exit;
      end
      else
      begin
        Bucket := TJclWideStrHashSetBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size] := AString;
        Inc(Bucket.Size);
        Inc(FSize);
        Result := True;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.AddAll(const ACollection: IJclWideStrCollection): Boolean;
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
    Result := True;
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

procedure TJclWideStrHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclWideStrHashSetBucket;
  ADest: TJclWideStrHashSet;
  ACollection: IJclWideStrCollection;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclWideStrHashSet then
    begin
      ADest := TJclWideStrHashSet(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclWideStrHashSetBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
            NewBucket.Entries[J] := SelfBucket.Entries[J];
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclWideStrCollection, ACollection) then
    begin
      ACollection.Clear;
      ACollection.AddAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashSet.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrHashSet then
    TJclWideStrHashSet(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclWideStrHashSet.Clear;
var
  I, J: Integer;
  Bucket: TJclWideStrHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
          FreeString(Bucket.Entries[J]);
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.CollectionEquals(const ACollection: IJclWideStrCollection): Boolean;
var
  I, J: Integer;
  It: IJclWideStrIterator;
  Bucket: TJclWideStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if not ItemsEqual(Bucket.Entries[J], It.Next) then
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.Contains(const AString: WideString): Boolean;
var
  I: Integer;
  Bucket: TJclWideStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AString), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AString) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.Extract(const AString: WideString): Boolean;
var
  Bucket: TJclWideStrHashSetBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AString), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AString) then
        begin
          Result := True;
          Bucket.Entries[I] := '';
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.ExtractAll(const ACollection: IJclWideStrCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.First: IJclWideStrIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclWideStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := 0;
    ABucket := nil;
    while ABucketIndex < FCapacity do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Inc(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := 0
    else
      AItemIndex := -1;
    Result := TJclWideStrHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isFirst);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrHashSet.GetEnumerator: IJclWideStrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

procedure TJclWideStrHashSet.Intersect(const ACollection: IJclWideStrCollection);
begin
  RetainAll(ACollection);
end;

function TJclWideStrHashSet.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrHashSet.Last: IJclWideStrIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclWideStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := FCapacity - 1;
    ABucket := nil;
    while ABucketIndex >= 0 do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Dec(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := ABucket.Size - 1
    else
      AItemIndex := -1;
    Result := TJclWideStrHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isLast);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashSet.Pack;
var
  I: Integer;
  Bucket: TJclWideStrHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.Remove(const AString: WideString): Boolean;
var
  Extracted: WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AString);
    if Result then
    begin
      Extracted := AString;
      FreeString(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.RetainAll(const ACollection: IJclWideStrCollection): Boolean;
var
  I, J, NewCapacity: Integer;
  Bucket: TJclWideStrHashSetBucket;
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
    Result := True;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := Bucket.Size - 1 downto 0 do
          if not ACollection.Contains(Bucket.Entries[I]) then
        begin
          Bucket.Entries[J] := FreeString(Bucket.Entries[J]);
          if J < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, J + 1, J, Bucket.Size - J - 1);
          Dec(Bucket.Size);
          Dec(FSize);
        end;

        NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
        if NewCapacity < Length(Bucket.Entries) then
          SetLength(Bucket.Entries, NewCapacity);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashSet.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.Size: Integer;
begin
  Result := FSize;
end;

procedure TJclWideStrHashSet.Subtract(const ACollection: IJclWideStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclWideStrHashSet.Union(const ACollection: IJclWideStrCollection);
begin
  AddAll(ACollection);
end;


function TJclWideStrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrHashSet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclWideStrHashSetIterator } ============================================

constructor TJclWideStrHashSetIterator.Create(AOwnHashSet: TJclWideStrHashSet;
  ABucketIndex, AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnHashSet := AOwnHashSet;
  FBucketIndex := ABucketIndex;
  FItemIndex := AItemIndex;
  FStart := AStart;
end;

function TJclWideStrHashSetIterator.Add(const AString: WideString): Boolean;
begin
  Result := FOwnHashSet.Add(AString);
end;

procedure TJclWideStrHashSetIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclWideStrHashSetIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclWideStrHashSetIterator then
  begin
    ADest := TJclWideStrHashSetIterator(Dest);
    ADest.FBucketIndex := FBucketIndex;
    ADest.FItemIndex := FItemIndex;
    ADest.FOwnHashSet := FOwnHashSet;
    ADest.FStart := FStart;
  end;
end;

function TJclWideStrHashSetIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclWideStrHashSetIterator.Create(FOwnHashSet, FBucketIndex, FItemIndex, Valid, FStart);
end;

procedure TJclWideStrHashSetIterator.Extract;
var
  AString: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    AString := GetString;
    Valid := False;
    FOwnHashSet.Extract(AString);
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSetIterator.GetString: WideString;
var
  ABucket: TJclWideStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := '';
    ABucket := FOwnHashSet.FBuckets[FBucketIndex - 1];
    if (ABucket <> nil) and (FItemIndex < ABucket.Size) then
      Result := ABucket.Entries[FItemIndex]
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSetIterator.HasNext: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclWideStrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex < FOwnHashSet.FCapacity do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex < (ABucket.Size - 1)) or
          ((not SkipCurrent) and (AItemIndex < ABucket.Size)) then
          Exit;
      end;
      AItemIndex := 0;
      Inc(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSetIterator.HasPrevious: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclWideStrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex >= 0 do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex > 0) or
          ((not SkipCurrent) and (AItemIndex >= 0)) then
          Exit;
      end;
      AItemIndex := MaxInt;
      Dec(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSetIterator.Insert(const AString: WideString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclWideStrHashSetIterator.IteratorEquals(const AIterator: IJclWideStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclWideStrHashSetIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclWideStrHashSetIterator then
  begin
    ItrObj := TJclWideStrHashSetIterator(Obj);
    Result := (FOwnHashSet = ItrObj.FOwnHashSet) and (FBucketIndex = ItrObj.FBucketIndex) and (FItemIndex = ItrObj.FItemIndex) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrHashSetIterator.MoveNext: Boolean;
var
  ABucket: TJclWideStrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := (FBucketIndex >= 0) and (FItemIndex >= 0) and
              (FBucketIndex < FOwnHashSet.FCapacity);
    if Result then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      Result := (ABucket <> nil) and (FItemIndex < ABucket.Size);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclWideStrHashSetIterator.Next: WideString;
var
  ABucket: TJclWideStrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := '';
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSetIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclWideStrHashSetIterator.Previous: WideString;
var
  ABucket: TJclWideStrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex >= 0 do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if (ABucket <> nil) and (FItemIndex < 0) then
          FItemIndex := ABucket.Size - 1;
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex >= 0) and (FItemIndex < ABucket.Size) then
            Break;
          if (FItemIndex > 0) and (FItemIndex < ABucket.Size) then
          begin
            Dec(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := -1;
        Dec(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := '';
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSetIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrHashSetIterator.Remove;
begin

end;

procedure TJclWideStrHashSetIterator.Reset;
var
  ABucket: TJclWideStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          FBucketIndex := 0;
          ABucket := nil;
          while FBucketIndex < FOwnHashSet.FCapacity do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Inc(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := 0
          else
            FItemIndex := -1;
        end;
      isLast:
        begin
          FBucketIndex := FOwnHashSet.FCapacity - 1;
          ABucket := nil;
          while FBucketIndex >= 0 do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Dec(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := ABucket.Size - 1
          else
            FItemIndex := -1;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashSetIterator.SetString(const AString: WideString);
begin
  raise EJclOperationNotSupportedError.Create;
end;
{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrHashSet } ====================================================

constructor TJclUnicodeStrHashSet.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclUnicodeStrHashSet.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclUnicodeStrHashSet.Add(const AString: UnicodeString): Boolean;
var
  Index: Integer;
  Bucket: TJclUnicodeStrHashSetBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if FAllowDefaultElements or (not ItemsEqual(AString, '')) then
    begin
      Index := FHashToRangeFunction(Hash(AString), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if ItemsEqual(Bucket.Entries[I], AString) then
            Exit;
      end
      else
      begin
        Bucket := TJclUnicodeStrHashSetBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size] := AString;
        Inc(Bucket.Size);
        Inc(FSize);
        Result := True;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSet.AddAll(const ACollection: IJclUnicodeStrCollection): Boolean;
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
    Result := True;
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

procedure TJclUnicodeStrHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclUnicodeStrHashSetBucket;
  ADest: TJclUnicodeStrHashSet;
  ACollection: IJclUnicodeStrCollection;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclUnicodeStrHashSet then
    begin
      ADest := TJclUnicodeStrHashSet(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclUnicodeStrHashSetBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
            NewBucket.Entries[J] := SelfBucket.Entries[J];
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclUnicodeStrCollection, ACollection) then
    begin
      ACollection.Clear;
      ACollection.AddAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrHashSet.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclUnicodeStrHashSet then
    TJclUnicodeStrHashSet(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclUnicodeStrHashSet.Clear;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
          FreeString(Bucket.Entries[J]);
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSet.CollectionEquals(const ACollection: IJclUnicodeStrCollection): Boolean;
var
  I, J: Integer;
  It: IJclUnicodeStrIterator;
  Bucket: TJclUnicodeStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if not ItemsEqual(Bucket.Entries[J], It.Next) then
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSet.Contains(const AString: UnicodeString): Boolean;
var
  I: Integer;
  Bucket: TJclUnicodeStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AString), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AString) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSet.ContainsAll(const ACollection: IJclUnicodeStrCollection): Boolean;
var
  It: IJclUnicodeStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSet.Extract(const AString: UnicodeString): Boolean;
var
  Bucket: TJclUnicodeStrHashSetBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AString), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AString) then
        begin
          Result := True;
          Bucket.Entries[I] := '';
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSet.ExtractAll(const ACollection: IJclUnicodeStrCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSet.First: IJclUnicodeStrIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclUnicodeStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := 0;
    ABucket := nil;
    while ABucketIndex < FCapacity do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Inc(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := 0
    else
      AItemIndex := -1;
    Result := TJclUnicodeStrHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isFirst);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclUnicodeStrHashSet.GetEnumerator: IJclUnicodeStrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

procedure TJclUnicodeStrHashSet.Intersect(const ACollection: IJclUnicodeStrCollection);
begin
  RetainAll(ACollection);
end;

function TJclUnicodeStrHashSet.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclUnicodeStrHashSet.Last: IJclUnicodeStrIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclUnicodeStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := FCapacity - 1;
    ABucket := nil;
    while ABucketIndex >= 0 do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Dec(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := ABucket.Size - 1
    else
      AItemIndex := -1;
    Result := TJclUnicodeStrHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isLast);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrHashSet.Pack;
var
  I: Integer;
  Bucket: TJclUnicodeStrHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSet.Remove(const AString: UnicodeString): Boolean;
var
  Extracted: UnicodeString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AString);
    if Result then
    begin
      Extracted := AString;
      FreeString(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSet.RemoveAll(const ACollection: IJclUnicodeStrCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSet.RetainAll(const ACollection: IJclUnicodeStrCollection): Boolean;
var
  I, J, NewCapacity: Integer;
  Bucket: TJclUnicodeStrHashSetBucket;
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
    Result := True;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := Bucket.Size - 1 downto 0 do
          if not ACollection.Contains(Bucket.Entries[I]) then
        begin
          Bucket.Entries[J] := FreeString(Bucket.Entries[J]);
          if J < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, J + 1, J, Bucket.Size - J - 1);
          Dec(Bucket.Size);
          Dec(FSize);
        end;

        NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
        if NewCapacity < Length(Bucket.Entries) then
          SetLength(Bucket.Entries, NewCapacity);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrHashSet.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSet.Size: Integer;
begin
  Result := FSize;
end;

procedure TJclUnicodeStrHashSet.Subtract(const ACollection: IJclUnicodeStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclUnicodeStrHashSet.Union(const ACollection: IJclUnicodeStrCollection);
begin
  AddAll(ACollection);
end;


function TJclUnicodeStrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrHashSet.Create(Size);
  AssignPropertiesTo(Result);
end;

{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrHashSetIterator } ============================================

constructor TJclUnicodeStrHashSetIterator.Create(AOwnHashSet: TJclUnicodeStrHashSet;
  ABucketIndex, AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnHashSet := AOwnHashSet;
  FBucketIndex := ABucketIndex;
  FItemIndex := AItemIndex;
  FStart := AStart;
end;

function TJclUnicodeStrHashSetIterator.Add(const AString: UnicodeString): Boolean;
begin
  Result := FOwnHashSet.Add(AString);
end;

procedure TJclUnicodeStrHashSetIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclUnicodeStrHashSetIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclUnicodeStrHashSetIterator then
  begin
    ADest := TJclUnicodeStrHashSetIterator(Dest);
    ADest.FBucketIndex := FBucketIndex;
    ADest.FItemIndex := FItemIndex;
    ADest.FOwnHashSet := FOwnHashSet;
    ADest.FStart := FStart;
  end;
end;

function TJclUnicodeStrHashSetIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclUnicodeStrHashSetIterator.Create(FOwnHashSet, FBucketIndex, FItemIndex, Valid, FStart);
end;

procedure TJclUnicodeStrHashSetIterator.Extract;
var
  AString: UnicodeString;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    AString := GetString;
    Valid := False;
    FOwnHashSet.Extract(AString);
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSetIterator.GetString: UnicodeString;
var
  ABucket: TJclUnicodeStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := '';
    ABucket := FOwnHashSet.FBuckets[FBucketIndex - 1];
    if (ABucket <> nil) and (FItemIndex < ABucket.Size) then
      Result := ABucket.Entries[FItemIndex]
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSetIterator.HasNext: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclUnicodeStrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex < FOwnHashSet.FCapacity do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex < (ABucket.Size - 1)) or
          ((not SkipCurrent) and (AItemIndex < ABucket.Size)) then
          Exit;
      end;
      AItemIndex := 0;
      Inc(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSetIterator.HasPrevious: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclUnicodeStrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex >= 0 do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex > 0) or
          ((not SkipCurrent) and (AItemIndex >= 0)) then
          Exit;
      end;
      AItemIndex := MaxInt;
      Dec(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSetIterator.Insert(const AString: UnicodeString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclUnicodeStrHashSetIterator.IteratorEquals(const AIterator: IJclUnicodeStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclUnicodeStrHashSetIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclUnicodeStrHashSetIterator then
  begin
    ItrObj := TJclUnicodeStrHashSetIterator(Obj);
    Result := (FOwnHashSet = ItrObj.FOwnHashSet) and (FBucketIndex = ItrObj.FBucketIndex) and (FItemIndex = ItrObj.FItemIndex) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclUnicodeStrHashSetIterator.MoveNext: Boolean;
var
  ABucket: TJclUnicodeStrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := (FBucketIndex >= 0) and (FItemIndex >= 0) and
              (FBucketIndex < FOwnHashSet.FCapacity);
    if Result then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      Result := (ABucket <> nil) and (FItemIndex < ABucket.Size);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclUnicodeStrHashSetIterator.Next: UnicodeString;
var
  ABucket: TJclUnicodeStrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := '';
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSetIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclUnicodeStrHashSetIterator.Previous: UnicodeString;
var
  ABucket: TJclUnicodeStrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex >= 0 do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if (ABucket <> nil) and (FItemIndex < 0) then
          FItemIndex := ABucket.Size - 1;
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex >= 0) and (FItemIndex < ABucket.Size) then
            Break;
          if (FItemIndex > 0) and (FItemIndex < ABucket.Size) then
          begin
            Dec(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := -1;
        Dec(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := '';
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashSetIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclUnicodeStrHashSetIterator.Remove;
begin

end;

procedure TJclUnicodeStrHashSetIterator.Reset;
var
  ABucket: TJclUnicodeStrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          FBucketIndex := 0;
          ABucket := nil;
          while FBucketIndex < FOwnHashSet.FCapacity do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Inc(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := 0
          else
            FItemIndex := -1;
        end;
      isLast:
        begin
          FBucketIndex := FOwnHashSet.FCapacity - 1;
          ABucket := nil;
          while FBucketIndex >= 0 do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Dec(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := ABucket.Size - 1
          else
            FItemIndex := -1;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrHashSetIterator.SetString(const AString: UnicodeString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$ENDIF SUPPORTS_UNICODE_STRING}
//=== { TJclSingleHashSet } ====================================================

constructor TJclSingleHashSet.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclSingleHashSet.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclSingleHashSet.Add(const AValue: Single): Boolean;
var
  Index: Integer;
  Bucket: TJclSingleHashSetBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if FAllowDefaultElements or (not ItemsEqual(AValue, 0.0)) then
    begin
      Index := FHashToRangeFunction(Hash(AValue), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if ItemsEqual(Bucket.Entries[I], AValue) then
            Exit;
      end
      else
      begin
        Bucket := TJclSingleHashSetBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size] := AValue;
        Inc(Bucket.Size);
        Inc(FSize);
        Result := True;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSet.AddAll(const ACollection: IJclSingleCollection): Boolean;
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
    Result := True;
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

procedure TJclSingleHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclSingleHashSetBucket;
  ADest: TJclSingleHashSet;
  ACollection: IJclSingleCollection;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclSingleHashSet then
    begin
      ADest := TJclSingleHashSet(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclSingleHashSetBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
            NewBucket.Entries[J] := SelfBucket.Entries[J];
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclSingleCollection, ACollection) then
    begin
      ACollection.Clear;
      ACollection.AddAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashSet.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclSingleHashSet then
    TJclSingleHashSet(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclSingleHashSet.Clear;
var
  I, J: Integer;
  Bucket: TJclSingleHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
          FreeSingle(Bucket.Entries[J]);
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSet.CollectionEquals(const ACollection: IJclSingleCollection): Boolean;
var
  I, J: Integer;
  It: IJclSingleIterator;
  Bucket: TJclSingleHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if not ItemsEqual(Bucket.Entries[J], It.Next) then
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSet.Contains(const AValue: Single): Boolean;
var
  I: Integer;
  Bucket: TJclSingleHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AValue), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AValue) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSet.ContainsAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSet.Extract(const AValue: Single): Boolean;
var
  Bucket: TJclSingleHashSetBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AValue), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AValue) then
        begin
          Result := True;
          Bucket.Entries[I] := 0.0;
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSet.ExtractAll(const ACollection: IJclSingleCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSet.First: IJclSingleIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclSingleHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := 0;
    ABucket := nil;
    while ABucketIndex < FCapacity do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Inc(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := 0
    else
      AItemIndex := -1;
    Result := TJclSingleHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isFirst);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSingleHashSet.GetEnumerator: IJclSingleIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

procedure TJclSingleHashSet.Intersect(const ACollection: IJclSingleCollection);
begin
  RetainAll(ACollection);
end;

function TJclSingleHashSet.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleHashSet.Last: IJclSingleIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclSingleHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := FCapacity - 1;
    ABucket := nil;
    while ABucketIndex >= 0 do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Dec(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := ABucket.Size - 1
    else
      AItemIndex := -1;
    Result := TJclSingleHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isLast);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashSet.Pack;
var
  I: Integer;
  Bucket: TJclSingleHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSet.Remove(const AValue: Single): Boolean;
var
  Extracted: Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AValue);
    if Result then
    begin
      Extracted := AValue;
      FreeSingle(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSet.RemoveAll(const ACollection: IJclSingleCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSet.RetainAll(const ACollection: IJclSingleCollection): Boolean;
var
  I, J, NewCapacity: Integer;
  Bucket: TJclSingleHashSetBucket;
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
    Result := True;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := Bucket.Size - 1 downto 0 do
          if not ACollection.Contains(Bucket.Entries[I]) then
        begin
          Bucket.Entries[J] := FreeSingle(Bucket.Entries[J]);
          if J < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, J + 1, J, Bucket.Size - J - 1);
          Dec(Bucket.Size);
          Dec(FSize);
        end;

        NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
        if NewCapacity < Length(Bucket.Entries) then
          SetLength(Bucket.Entries, NewCapacity);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashSet.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSet.Size: Integer;
begin
  Result := FSize;
end;

procedure TJclSingleHashSet.Subtract(const ACollection: IJclSingleCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclSingleHashSet.Union(const ACollection: IJclSingleCollection);
begin
  AddAll(ACollection);
end;


function TJclSingleHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleHashSet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclSingleHashSetIterator } ============================================

constructor TJclSingleHashSetIterator.Create(AOwnHashSet: TJclSingleHashSet;
  ABucketIndex, AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnHashSet := AOwnHashSet;
  FBucketIndex := ABucketIndex;
  FItemIndex := AItemIndex;
  FStart := AStart;
end;

function TJclSingleHashSetIterator.Add(const AValue: Single): Boolean;
begin
  Result := FOwnHashSet.Add(AValue);
end;

procedure TJclSingleHashSetIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclSingleHashSetIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclSingleHashSetIterator then
  begin
    ADest := TJclSingleHashSetIterator(Dest);
    ADest.FBucketIndex := FBucketIndex;
    ADest.FItemIndex := FItemIndex;
    ADest.FOwnHashSet := FOwnHashSet;
    ADest.FStart := FStart;
  end;
end;

function TJclSingleHashSetIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclSingleHashSetIterator.Create(FOwnHashSet, FBucketIndex, FItemIndex, Valid, FStart);
end;

procedure TJclSingleHashSetIterator.Extract;
var
  AValue: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    AValue := GetValue;
    Valid := False;
    FOwnHashSet.Extract(AValue);
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSetIterator.GetValue: Single;
var
  ABucket: TJclSingleHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0.0;
    ABucket := FOwnHashSet.FBuckets[FBucketIndex - 1];
    if (ABucket <> nil) and (FItemIndex < ABucket.Size) then
      Result := ABucket.Entries[FItemIndex]
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSetIterator.HasNext: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclSingleHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex < FOwnHashSet.FCapacity do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex < (ABucket.Size - 1)) or
          ((not SkipCurrent) and (AItemIndex < ABucket.Size)) then
          Exit;
      end;
      AItemIndex := 0;
      Inc(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSetIterator.HasPrevious: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclSingleHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex >= 0 do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex > 0) or
          ((not SkipCurrent) and (AItemIndex >= 0)) then
          Exit;
      end;
      AItemIndex := MaxInt;
      Dec(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSetIterator.Insert(const AValue: Single): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclSingleHashSetIterator.IteratorEquals(const AIterator: IJclSingleIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclSingleHashSetIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclSingleHashSetIterator then
  begin
    ItrObj := TJclSingleHashSetIterator(Obj);
    Result := (FOwnHashSet = ItrObj.FOwnHashSet) and (FBucketIndex = ItrObj.FBucketIndex) and (FItemIndex = ItrObj.FItemIndex) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSingleHashSetIterator.MoveNext: Boolean;
var
  ABucket: TJclSingleHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := (FBucketIndex >= 0) and (FItemIndex >= 0) and
              (FBucketIndex < FOwnHashSet.FCapacity);
    if Result then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      Result := (ABucket <> nil) and (FItemIndex < ABucket.Size);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSingleHashSetIterator.Next: Single;
var
  ABucket: TJclSingleHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := 0.0;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSetIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclSingleHashSetIterator.Previous: Single;
var
  ABucket: TJclSingleHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex >= 0 do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if (ABucket <> nil) and (FItemIndex < 0) then
          FItemIndex := ABucket.Size - 1;
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex >= 0) and (FItemIndex < ABucket.Size) then
            Break;
          if (FItemIndex > 0) and (FItemIndex < ABucket.Size) then
          begin
            Dec(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := -1;
        Dec(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := 0.0;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSetIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclSingleHashSetIterator.Remove;
begin

end;

procedure TJclSingleHashSetIterator.Reset;
var
  ABucket: TJclSingleHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          FBucketIndex := 0;
          ABucket := nil;
          while FBucketIndex < FOwnHashSet.FCapacity do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Inc(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := 0
          else
            FItemIndex := -1;
        end;
      isLast:
        begin
          FBucketIndex := FOwnHashSet.FCapacity - 1;
          ABucket := nil;
          while FBucketIndex >= 0 do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Dec(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := ABucket.Size - 1
          else
            FItemIndex := -1;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashSetIterator.SetValue(const AValue: Single);
begin
  raise EJclOperationNotSupportedError.Create;
end;
//=== { TJclDoubleHashSet } ====================================================

constructor TJclDoubleHashSet.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclDoubleHashSet.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclDoubleHashSet.Add(const AValue: Double): Boolean;
var
  Index: Integer;
  Bucket: TJclDoubleHashSetBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if FAllowDefaultElements or (not ItemsEqual(AValue, 0.0)) then
    begin
      Index := FHashToRangeFunction(Hash(AValue), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if ItemsEqual(Bucket.Entries[I], AValue) then
            Exit;
      end
      else
      begin
        Bucket := TJclDoubleHashSetBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size] := AValue;
        Inc(Bucket.Size);
        Inc(FSize);
        Result := True;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSet.AddAll(const ACollection: IJclDoubleCollection): Boolean;
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
    Result := True;
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

procedure TJclDoubleHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclDoubleHashSetBucket;
  ADest: TJclDoubleHashSet;
  ACollection: IJclDoubleCollection;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclDoubleHashSet then
    begin
      ADest := TJclDoubleHashSet(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclDoubleHashSetBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
            NewBucket.Entries[J] := SelfBucket.Entries[J];
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclDoubleCollection, ACollection) then
    begin
      ACollection.Clear;
      ACollection.AddAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashSet.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclDoubleHashSet then
    TJclDoubleHashSet(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclDoubleHashSet.Clear;
var
  I, J: Integer;
  Bucket: TJclDoubleHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
          FreeDouble(Bucket.Entries[J]);
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSet.CollectionEquals(const ACollection: IJclDoubleCollection): Boolean;
var
  I, J: Integer;
  It: IJclDoubleIterator;
  Bucket: TJclDoubleHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if not ItemsEqual(Bucket.Entries[J], It.Next) then
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSet.Contains(const AValue: Double): Boolean;
var
  I: Integer;
  Bucket: TJclDoubleHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AValue), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AValue) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSet.ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSet.Extract(const AValue: Double): Boolean;
var
  Bucket: TJclDoubleHashSetBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AValue), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AValue) then
        begin
          Result := True;
          Bucket.Entries[I] := 0.0;
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSet.ExtractAll(const ACollection: IJclDoubleCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSet.First: IJclDoubleIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclDoubleHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := 0;
    ABucket := nil;
    while ABucketIndex < FCapacity do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Inc(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := 0
    else
      AItemIndex := -1;
    Result := TJclDoubleHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isFirst);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclDoubleHashSet.GetEnumerator: IJclDoubleIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

procedure TJclDoubleHashSet.Intersect(const ACollection: IJclDoubleCollection);
begin
  RetainAll(ACollection);
end;

function TJclDoubleHashSet.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleHashSet.Last: IJclDoubleIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclDoubleHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := FCapacity - 1;
    ABucket := nil;
    while ABucketIndex >= 0 do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Dec(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := ABucket.Size - 1
    else
      AItemIndex := -1;
    Result := TJclDoubleHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isLast);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashSet.Pack;
var
  I: Integer;
  Bucket: TJclDoubleHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSet.Remove(const AValue: Double): Boolean;
var
  Extracted: Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AValue);
    if Result then
    begin
      Extracted := AValue;
      FreeDouble(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSet.RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSet.RetainAll(const ACollection: IJclDoubleCollection): Boolean;
var
  I, J, NewCapacity: Integer;
  Bucket: TJclDoubleHashSetBucket;
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
    Result := True;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := Bucket.Size - 1 downto 0 do
          if not ACollection.Contains(Bucket.Entries[I]) then
        begin
          Bucket.Entries[J] := FreeDouble(Bucket.Entries[J]);
          if J < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, J + 1, J, Bucket.Size - J - 1);
          Dec(Bucket.Size);
          Dec(FSize);
        end;

        NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
        if NewCapacity < Length(Bucket.Entries) then
          SetLength(Bucket.Entries, NewCapacity);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashSet.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSet.Size: Integer;
begin
  Result := FSize;
end;

procedure TJclDoubleHashSet.Subtract(const ACollection: IJclDoubleCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclDoubleHashSet.Union(const ACollection: IJclDoubleCollection);
begin
  AddAll(ACollection);
end;


function TJclDoubleHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleHashSet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclDoubleHashSetIterator } ============================================

constructor TJclDoubleHashSetIterator.Create(AOwnHashSet: TJclDoubleHashSet;
  ABucketIndex, AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnHashSet := AOwnHashSet;
  FBucketIndex := ABucketIndex;
  FItemIndex := AItemIndex;
  FStart := AStart;
end;

function TJclDoubleHashSetIterator.Add(const AValue: Double): Boolean;
begin
  Result := FOwnHashSet.Add(AValue);
end;

procedure TJclDoubleHashSetIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclDoubleHashSetIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclDoubleHashSetIterator then
  begin
    ADest := TJclDoubleHashSetIterator(Dest);
    ADest.FBucketIndex := FBucketIndex;
    ADest.FItemIndex := FItemIndex;
    ADest.FOwnHashSet := FOwnHashSet;
    ADest.FStart := FStart;
  end;
end;

function TJclDoubleHashSetIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclDoubleHashSetIterator.Create(FOwnHashSet, FBucketIndex, FItemIndex, Valid, FStart);
end;

procedure TJclDoubleHashSetIterator.Extract;
var
  AValue: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    AValue := GetValue;
    Valid := False;
    FOwnHashSet.Extract(AValue);
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSetIterator.GetValue: Double;
var
  ABucket: TJclDoubleHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0.0;
    ABucket := FOwnHashSet.FBuckets[FBucketIndex - 1];
    if (ABucket <> nil) and (FItemIndex < ABucket.Size) then
      Result := ABucket.Entries[FItemIndex]
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSetIterator.HasNext: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclDoubleHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex < FOwnHashSet.FCapacity do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex < (ABucket.Size - 1)) or
          ((not SkipCurrent) and (AItemIndex < ABucket.Size)) then
          Exit;
      end;
      AItemIndex := 0;
      Inc(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSetIterator.HasPrevious: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclDoubleHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex >= 0 do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex > 0) or
          ((not SkipCurrent) and (AItemIndex >= 0)) then
          Exit;
      end;
      AItemIndex := MaxInt;
      Dec(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSetIterator.Insert(const AValue: Double): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclDoubleHashSetIterator.IteratorEquals(const AIterator: IJclDoubleIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclDoubleHashSetIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclDoubleHashSetIterator then
  begin
    ItrObj := TJclDoubleHashSetIterator(Obj);
    Result := (FOwnHashSet = ItrObj.FOwnHashSet) and (FBucketIndex = ItrObj.FBucketIndex) and (FItemIndex = ItrObj.FItemIndex) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclDoubleHashSetIterator.MoveNext: Boolean;
var
  ABucket: TJclDoubleHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := (FBucketIndex >= 0) and (FItemIndex >= 0) and
              (FBucketIndex < FOwnHashSet.FCapacity);
    if Result then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      Result := (ABucket <> nil) and (FItemIndex < ABucket.Size);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclDoubleHashSetIterator.Next: Double;
var
  ABucket: TJclDoubleHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := 0.0;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSetIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclDoubleHashSetIterator.Previous: Double;
var
  ABucket: TJclDoubleHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex >= 0 do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if (ABucket <> nil) and (FItemIndex < 0) then
          FItemIndex := ABucket.Size - 1;
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex >= 0) and (FItemIndex < ABucket.Size) then
            Break;
          if (FItemIndex > 0) and (FItemIndex < ABucket.Size) then
          begin
            Dec(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := -1;
        Dec(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := 0.0;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSetIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclDoubleHashSetIterator.Remove;
begin

end;

procedure TJclDoubleHashSetIterator.Reset;
var
  ABucket: TJclDoubleHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          FBucketIndex := 0;
          ABucket := nil;
          while FBucketIndex < FOwnHashSet.FCapacity do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Inc(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := 0
          else
            FItemIndex := -1;
        end;
      isLast:
        begin
          FBucketIndex := FOwnHashSet.FCapacity - 1;
          ABucket := nil;
          while FBucketIndex >= 0 do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Dec(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := ABucket.Size - 1
          else
            FItemIndex := -1;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashSetIterator.SetValue(const AValue: Double);
begin
  raise EJclOperationNotSupportedError.Create;
end;
//=== { TJclExtendedHashSet } ====================================================

constructor TJclExtendedHashSet.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclExtendedHashSet.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclExtendedHashSet.Add(const AValue: Extended): Boolean;
var
  Index: Integer;
  Bucket: TJclExtendedHashSetBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if FAllowDefaultElements or (not ItemsEqual(AValue, 0.0)) then
    begin
      Index := FHashToRangeFunction(Hash(AValue), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if ItemsEqual(Bucket.Entries[I], AValue) then
            Exit;
      end
      else
      begin
        Bucket := TJclExtendedHashSetBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size] := AValue;
        Inc(Bucket.Size);
        Inc(FSize);
        Result := True;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSet.AddAll(const ACollection: IJclExtendedCollection): Boolean;
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
    Result := True;
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

procedure TJclExtendedHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclExtendedHashSetBucket;
  ADest: TJclExtendedHashSet;
  ACollection: IJclExtendedCollection;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclExtendedHashSet then
    begin
      ADest := TJclExtendedHashSet(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclExtendedHashSetBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
            NewBucket.Entries[J] := SelfBucket.Entries[J];
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclExtendedCollection, ACollection) then
    begin
      ACollection.Clear;
      ACollection.AddAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashSet.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclExtendedHashSet then
    TJclExtendedHashSet(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclExtendedHashSet.Clear;
var
  I, J: Integer;
  Bucket: TJclExtendedHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
          FreeExtended(Bucket.Entries[J]);
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSet.CollectionEquals(const ACollection: IJclExtendedCollection): Boolean;
var
  I, J: Integer;
  It: IJclExtendedIterator;
  Bucket: TJclExtendedHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if not ItemsEqual(Bucket.Entries[J], It.Next) then
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSet.Contains(const AValue: Extended): Boolean;
var
  I: Integer;
  Bucket: TJclExtendedHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AValue), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AValue) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSet.ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSet.Extract(const AValue: Extended): Boolean;
var
  Bucket: TJclExtendedHashSetBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AValue), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AValue) then
        begin
          Result := True;
          Bucket.Entries[I] := 0.0;
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSet.ExtractAll(const ACollection: IJclExtendedCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSet.First: IJclExtendedIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclExtendedHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := 0;
    ABucket := nil;
    while ABucketIndex < FCapacity do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Inc(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := 0
    else
      AItemIndex := -1;
    Result := TJclExtendedHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isFirst);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclExtendedHashSet.GetEnumerator: IJclExtendedIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

procedure TJclExtendedHashSet.Intersect(const ACollection: IJclExtendedCollection);
begin
  RetainAll(ACollection);
end;

function TJclExtendedHashSet.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedHashSet.Last: IJclExtendedIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclExtendedHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := FCapacity - 1;
    ABucket := nil;
    while ABucketIndex >= 0 do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Dec(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := ABucket.Size - 1
    else
      AItemIndex := -1;
    Result := TJclExtendedHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isLast);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashSet.Pack;
var
  I: Integer;
  Bucket: TJclExtendedHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSet.Remove(const AValue: Extended): Boolean;
var
  Extracted: Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AValue);
    if Result then
    begin
      Extracted := AValue;
      FreeExtended(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSet.RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSet.RetainAll(const ACollection: IJclExtendedCollection): Boolean;
var
  I, J, NewCapacity: Integer;
  Bucket: TJclExtendedHashSetBucket;
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
    Result := True;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := Bucket.Size - 1 downto 0 do
          if not ACollection.Contains(Bucket.Entries[I]) then
        begin
          Bucket.Entries[J] := FreeExtended(Bucket.Entries[J]);
          if J < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, J + 1, J, Bucket.Size - J - 1);
          Dec(Bucket.Size);
          Dec(FSize);
        end;

        NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
        if NewCapacity < Length(Bucket.Entries) then
          SetLength(Bucket.Entries, NewCapacity);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashSet.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSet.Size: Integer;
begin
  Result := FSize;
end;

procedure TJclExtendedHashSet.Subtract(const ACollection: IJclExtendedCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclExtendedHashSet.Union(const ACollection: IJclExtendedCollection);
begin
  AddAll(ACollection);
end;


function TJclExtendedHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedHashSet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclExtendedHashSetIterator } ============================================

constructor TJclExtendedHashSetIterator.Create(AOwnHashSet: TJclExtendedHashSet;
  ABucketIndex, AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnHashSet := AOwnHashSet;
  FBucketIndex := ABucketIndex;
  FItemIndex := AItemIndex;
  FStart := AStart;
end;

function TJclExtendedHashSetIterator.Add(const AValue: Extended): Boolean;
begin
  Result := FOwnHashSet.Add(AValue);
end;

procedure TJclExtendedHashSetIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclExtendedHashSetIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclExtendedHashSetIterator then
  begin
    ADest := TJclExtendedHashSetIterator(Dest);
    ADest.FBucketIndex := FBucketIndex;
    ADest.FItemIndex := FItemIndex;
    ADest.FOwnHashSet := FOwnHashSet;
    ADest.FStart := FStart;
  end;
end;

function TJclExtendedHashSetIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclExtendedHashSetIterator.Create(FOwnHashSet, FBucketIndex, FItemIndex, Valid, FStart);
end;

procedure TJclExtendedHashSetIterator.Extract;
var
  AValue: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    AValue := GetValue;
    Valid := False;
    FOwnHashSet.Extract(AValue);
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSetIterator.GetValue: Extended;
var
  ABucket: TJclExtendedHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0.0;
    ABucket := FOwnHashSet.FBuckets[FBucketIndex - 1];
    if (ABucket <> nil) and (FItemIndex < ABucket.Size) then
      Result := ABucket.Entries[FItemIndex]
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSetIterator.HasNext: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclExtendedHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex < FOwnHashSet.FCapacity do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex < (ABucket.Size - 1)) or
          ((not SkipCurrent) and (AItemIndex < ABucket.Size)) then
          Exit;
      end;
      AItemIndex := 0;
      Inc(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSetIterator.HasPrevious: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclExtendedHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex >= 0 do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex > 0) or
          ((not SkipCurrent) and (AItemIndex >= 0)) then
          Exit;
      end;
      AItemIndex := MaxInt;
      Dec(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSetIterator.Insert(const AValue: Extended): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclExtendedHashSetIterator.IteratorEquals(const AIterator: IJclExtendedIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclExtendedHashSetIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclExtendedHashSetIterator then
  begin
    ItrObj := TJclExtendedHashSetIterator(Obj);
    Result := (FOwnHashSet = ItrObj.FOwnHashSet) and (FBucketIndex = ItrObj.FBucketIndex) and (FItemIndex = ItrObj.FItemIndex) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclExtendedHashSetIterator.MoveNext: Boolean;
var
  ABucket: TJclExtendedHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := (FBucketIndex >= 0) and (FItemIndex >= 0) and
              (FBucketIndex < FOwnHashSet.FCapacity);
    if Result then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      Result := (ABucket <> nil) and (FItemIndex < ABucket.Size);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclExtendedHashSetIterator.Next: Extended;
var
  ABucket: TJclExtendedHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := 0.0;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSetIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclExtendedHashSetIterator.Previous: Extended;
var
  ABucket: TJclExtendedHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex >= 0 do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if (ABucket <> nil) and (FItemIndex < 0) then
          FItemIndex := ABucket.Size - 1;
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex >= 0) and (FItemIndex < ABucket.Size) then
            Break;
          if (FItemIndex > 0) and (FItemIndex < ABucket.Size) then
          begin
            Dec(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := -1;
        Dec(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := 0.0;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSetIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclExtendedHashSetIterator.Remove;
begin

end;

procedure TJclExtendedHashSetIterator.Reset;
var
  ABucket: TJclExtendedHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          FBucketIndex := 0;
          ABucket := nil;
          while FBucketIndex < FOwnHashSet.FCapacity do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Inc(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := 0
          else
            FItemIndex := -1;
        end;
      isLast:
        begin
          FBucketIndex := FOwnHashSet.FCapacity - 1;
          ABucket := nil;
          while FBucketIndex >= 0 do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Dec(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := ABucket.Size - 1
          else
            FItemIndex := -1;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashSetIterator.SetValue(const AValue: Extended);
begin
  raise EJclOperationNotSupportedError.Create;
end;
//=== { TJclIntegerHashSet } ====================================================

constructor TJclIntegerHashSet.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntegerHashSet.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntegerHashSet.Add(AValue: Integer): Boolean;
var
  Index: Integer;
  Bucket: TJclIntegerHashSetBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if FAllowDefaultElements or (not ItemsEqual(AValue, 0)) then
    begin
      Index := FHashToRangeFunction(Hash(AValue), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if ItemsEqual(Bucket.Entries[I], AValue) then
            Exit;
      end
      else
      begin
        Bucket := TJclIntegerHashSetBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size] := AValue;
        Inc(Bucket.Size);
        Inc(FSize);
        Result := True;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSet.AddAll(const ACollection: IJclIntegerCollection): Boolean;
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
    Result := True;
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

procedure TJclIntegerHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntegerHashSetBucket;
  ADest: TJclIntegerHashSet;
  ACollection: IJclIntegerCollection;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntegerHashSet then
    begin
      ADest := TJclIntegerHashSet(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntegerHashSetBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
            NewBucket.Entries[J] := SelfBucket.Entries[J];
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntegerCollection, ACollection) then
    begin
      ACollection.Clear;
      ACollection.AddAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashSet.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntegerHashSet then
    TJclIntegerHashSet(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntegerHashSet.Clear;
var
  I, J: Integer;
  Bucket: TJclIntegerHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
          FreeInteger(Bucket.Entries[J]);
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSet.CollectionEquals(const ACollection: IJclIntegerCollection): Boolean;
var
  I, J: Integer;
  It: IJclIntegerIterator;
  Bucket: TJclIntegerHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if not ItemsEqual(Bucket.Entries[J], It.Next) then
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSet.Contains(AValue: Integer): Boolean;
var
  I: Integer;
  Bucket: TJclIntegerHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AValue), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AValue) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSet.ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSet.Extract(AValue: Integer): Boolean;
var
  Bucket: TJclIntegerHashSetBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AValue), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AValue) then
        begin
          Result := True;
          Bucket.Entries[I] := 0;
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSet.ExtractAll(const ACollection: IJclIntegerCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSet.First: IJclIntegerIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclIntegerHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := 0;
    ABucket := nil;
    while ABucketIndex < FCapacity do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Inc(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := 0
    else
      AItemIndex := -1;
    Result := TJclIntegerHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isFirst);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntegerHashSet.GetEnumerator: IJclIntegerIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

procedure TJclIntegerHashSet.Intersect(const ACollection: IJclIntegerCollection);
begin
  RetainAll(ACollection);
end;

function TJclIntegerHashSet.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerHashSet.Last: IJclIntegerIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclIntegerHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := FCapacity - 1;
    ABucket := nil;
    while ABucketIndex >= 0 do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Dec(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := ABucket.Size - 1
    else
      AItemIndex := -1;
    Result := TJclIntegerHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isLast);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashSet.Pack;
var
  I: Integer;
  Bucket: TJclIntegerHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSet.Remove(AValue: Integer): Boolean;
var
  Extracted: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AValue);
    if Result then
    begin
      Extracted := AValue;
      FreeInteger(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSet.RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSet.RetainAll(const ACollection: IJclIntegerCollection): Boolean;
var
  I, J, NewCapacity: Integer;
  Bucket: TJclIntegerHashSetBucket;
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
    Result := True;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := Bucket.Size - 1 downto 0 do
          if not ACollection.Contains(Bucket.Entries[I]) then
        begin
          Bucket.Entries[J] := FreeInteger(Bucket.Entries[J]);
          if J < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, J + 1, J, Bucket.Size - J - 1);
          Dec(Bucket.Size);
          Dec(FSize);
        end;

        NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
        if NewCapacity < Length(Bucket.Entries) then
          SetLength(Bucket.Entries, NewCapacity);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashSet.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSet.Size: Integer;
begin
  Result := FSize;
end;

procedure TJclIntegerHashSet.Subtract(const ACollection: IJclIntegerCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclIntegerHashSet.Union(const ACollection: IJclIntegerCollection);
begin
  AddAll(ACollection);
end;


function TJclIntegerHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerHashSet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclIntegerHashSetIterator } ============================================

constructor TJclIntegerHashSetIterator.Create(AOwnHashSet: TJclIntegerHashSet;
  ABucketIndex, AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnHashSet := AOwnHashSet;
  FBucketIndex := ABucketIndex;
  FItemIndex := AItemIndex;
  FStart := AStart;
end;

function TJclIntegerHashSetIterator.Add(AValue: Integer): Boolean;
begin
  Result := FOwnHashSet.Add(AValue);
end;

procedure TJclIntegerHashSetIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclIntegerHashSetIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclIntegerHashSetIterator then
  begin
    ADest := TJclIntegerHashSetIterator(Dest);
    ADest.FBucketIndex := FBucketIndex;
    ADest.FItemIndex := FItemIndex;
    ADest.FOwnHashSet := FOwnHashSet;
    ADest.FStart := FStart;
  end;
end;

function TJclIntegerHashSetIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclIntegerHashSetIterator.Create(FOwnHashSet, FBucketIndex, FItemIndex, Valid, FStart);
end;

procedure TJclIntegerHashSetIterator.Extract;
var
  AValue: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    AValue := GetValue;
    Valid := False;
    FOwnHashSet.Extract(AValue);
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSetIterator.GetValue: Integer;
var
  ABucket: TJclIntegerHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0;
    ABucket := FOwnHashSet.FBuckets[FBucketIndex - 1];
    if (ABucket <> nil) and (FItemIndex < ABucket.Size) then
      Result := ABucket.Entries[FItemIndex]
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSetIterator.HasNext: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclIntegerHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex < FOwnHashSet.FCapacity do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex < (ABucket.Size - 1)) or
          ((not SkipCurrent) and (AItemIndex < ABucket.Size)) then
          Exit;
      end;
      AItemIndex := 0;
      Inc(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSetIterator.HasPrevious: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclIntegerHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex >= 0 do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex > 0) or
          ((not SkipCurrent) and (AItemIndex >= 0)) then
          Exit;
      end;
      AItemIndex := MaxInt;
      Dec(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSetIterator.Insert(AValue: Integer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntegerHashSetIterator.IteratorEquals(const AIterator: IJclIntegerIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclIntegerHashSetIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclIntegerHashSetIterator then
  begin
    ItrObj := TJclIntegerHashSetIterator(Obj);
    Result := (FOwnHashSet = ItrObj.FOwnHashSet) and (FBucketIndex = ItrObj.FBucketIndex) and (FItemIndex = ItrObj.FItemIndex) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntegerHashSetIterator.MoveNext: Boolean;
var
  ABucket: TJclIntegerHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := (FBucketIndex >= 0) and (FItemIndex >= 0) and
              (FBucketIndex < FOwnHashSet.FCapacity);
    if Result then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      Result := (ABucket <> nil) and (FItemIndex < ABucket.Size);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntegerHashSetIterator.Next: Integer;
var
  ABucket: TJclIntegerHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := 0;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSetIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntegerHashSetIterator.Previous: Integer;
var
  ABucket: TJclIntegerHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex >= 0 do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if (ABucket <> nil) and (FItemIndex < 0) then
          FItemIndex := ABucket.Size - 1;
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex >= 0) and (FItemIndex < ABucket.Size) then
            Break;
          if (FItemIndex > 0) and (FItemIndex < ABucket.Size) then
          begin
            Dec(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := -1;
        Dec(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := 0;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSetIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntegerHashSetIterator.Remove;
begin

end;

procedure TJclIntegerHashSetIterator.Reset;
var
  ABucket: TJclIntegerHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          FBucketIndex := 0;
          ABucket := nil;
          while FBucketIndex < FOwnHashSet.FCapacity do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Inc(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := 0
          else
            FItemIndex := -1;
        end;
      isLast:
        begin
          FBucketIndex := FOwnHashSet.FCapacity - 1;
          ABucket := nil;
          while FBucketIndex >= 0 do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Dec(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := ABucket.Size - 1
          else
            FItemIndex := -1;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashSetIterator.SetValue(AValue: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;
//=== { TJclCardinalHashSet } ====================================================

constructor TJclCardinalHashSet.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclCardinalHashSet.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclCardinalHashSet.Add(AValue: Cardinal): Boolean;
var
  Index: Integer;
  Bucket: TJclCardinalHashSetBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if FAllowDefaultElements or (not ItemsEqual(AValue, 0)) then
    begin
      Index := FHashToRangeFunction(Hash(AValue), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if ItemsEqual(Bucket.Entries[I], AValue) then
            Exit;
      end
      else
      begin
        Bucket := TJclCardinalHashSetBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size] := AValue;
        Inc(Bucket.Size);
        Inc(FSize);
        Result := True;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSet.AddAll(const ACollection: IJclCardinalCollection): Boolean;
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
    Result := True;
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

procedure TJclCardinalHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclCardinalHashSetBucket;
  ADest: TJclCardinalHashSet;
  ACollection: IJclCardinalCollection;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclCardinalHashSet then
    begin
      ADest := TJclCardinalHashSet(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclCardinalHashSetBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
            NewBucket.Entries[J] := SelfBucket.Entries[J];
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclCardinalCollection, ACollection) then
    begin
      ACollection.Clear;
      ACollection.AddAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashSet.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclCardinalHashSet then
    TJclCardinalHashSet(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclCardinalHashSet.Clear;
var
  I, J: Integer;
  Bucket: TJclCardinalHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
          FreeCardinal(Bucket.Entries[J]);
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSet.CollectionEquals(const ACollection: IJclCardinalCollection): Boolean;
var
  I, J: Integer;
  It: IJclCardinalIterator;
  Bucket: TJclCardinalHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if not ItemsEqual(Bucket.Entries[J], It.Next) then
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSet.Contains(AValue: Cardinal): Boolean;
var
  I: Integer;
  Bucket: TJclCardinalHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AValue), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AValue) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSet.ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSet.Extract(AValue: Cardinal): Boolean;
var
  Bucket: TJclCardinalHashSetBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AValue), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AValue) then
        begin
          Result := True;
          Bucket.Entries[I] := 0;
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSet.ExtractAll(const ACollection: IJclCardinalCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSet.First: IJclCardinalIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclCardinalHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := 0;
    ABucket := nil;
    while ABucketIndex < FCapacity do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Inc(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := 0
    else
      AItemIndex := -1;
    Result := TJclCardinalHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isFirst);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclCardinalHashSet.GetEnumerator: IJclCardinalIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

procedure TJclCardinalHashSet.Intersect(const ACollection: IJclCardinalCollection);
begin
  RetainAll(ACollection);
end;

function TJclCardinalHashSet.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalHashSet.Last: IJclCardinalIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclCardinalHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := FCapacity - 1;
    ABucket := nil;
    while ABucketIndex >= 0 do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Dec(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := ABucket.Size - 1
    else
      AItemIndex := -1;
    Result := TJclCardinalHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isLast);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashSet.Pack;
var
  I: Integer;
  Bucket: TJclCardinalHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSet.Remove(AValue: Cardinal): Boolean;
var
  Extracted: Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AValue);
    if Result then
    begin
      Extracted := AValue;
      FreeCardinal(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSet.RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSet.RetainAll(const ACollection: IJclCardinalCollection): Boolean;
var
  I, J, NewCapacity: Integer;
  Bucket: TJclCardinalHashSetBucket;
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
    Result := True;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := Bucket.Size - 1 downto 0 do
          if not ACollection.Contains(Bucket.Entries[I]) then
        begin
          Bucket.Entries[J] := FreeCardinal(Bucket.Entries[J]);
          if J < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, J + 1, J, Bucket.Size - J - 1);
          Dec(Bucket.Size);
          Dec(FSize);
        end;

        NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
        if NewCapacity < Length(Bucket.Entries) then
          SetLength(Bucket.Entries, NewCapacity);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashSet.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSet.Size: Integer;
begin
  Result := FSize;
end;

procedure TJclCardinalHashSet.Subtract(const ACollection: IJclCardinalCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclCardinalHashSet.Union(const ACollection: IJclCardinalCollection);
begin
  AddAll(ACollection);
end;


function TJclCardinalHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalHashSet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclCardinalHashSetIterator } ============================================

constructor TJclCardinalHashSetIterator.Create(AOwnHashSet: TJclCardinalHashSet;
  ABucketIndex, AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnHashSet := AOwnHashSet;
  FBucketIndex := ABucketIndex;
  FItemIndex := AItemIndex;
  FStart := AStart;
end;

function TJclCardinalHashSetIterator.Add(AValue: Cardinal): Boolean;
begin
  Result := FOwnHashSet.Add(AValue);
end;

procedure TJclCardinalHashSetIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclCardinalHashSetIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclCardinalHashSetIterator then
  begin
    ADest := TJclCardinalHashSetIterator(Dest);
    ADest.FBucketIndex := FBucketIndex;
    ADest.FItemIndex := FItemIndex;
    ADest.FOwnHashSet := FOwnHashSet;
    ADest.FStart := FStart;
  end;
end;

function TJclCardinalHashSetIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclCardinalHashSetIterator.Create(FOwnHashSet, FBucketIndex, FItemIndex, Valid, FStart);
end;

procedure TJclCardinalHashSetIterator.Extract;
var
  AValue: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    AValue := GetValue;
    Valid := False;
    FOwnHashSet.Extract(AValue);
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSetIterator.GetValue: Cardinal;
var
  ABucket: TJclCardinalHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0;
    ABucket := FOwnHashSet.FBuckets[FBucketIndex - 1];
    if (ABucket <> nil) and (FItemIndex < ABucket.Size) then
      Result := ABucket.Entries[FItemIndex]
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSetIterator.HasNext: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclCardinalHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex < FOwnHashSet.FCapacity do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex < (ABucket.Size - 1)) or
          ((not SkipCurrent) and (AItemIndex < ABucket.Size)) then
          Exit;
      end;
      AItemIndex := 0;
      Inc(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSetIterator.HasPrevious: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclCardinalHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex >= 0 do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex > 0) or
          ((not SkipCurrent) and (AItemIndex >= 0)) then
          Exit;
      end;
      AItemIndex := MaxInt;
      Dec(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSetIterator.Insert(AValue: Cardinal): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclCardinalHashSetIterator.IteratorEquals(const AIterator: IJclCardinalIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclCardinalHashSetIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclCardinalHashSetIterator then
  begin
    ItrObj := TJclCardinalHashSetIterator(Obj);
    Result := (FOwnHashSet = ItrObj.FOwnHashSet) and (FBucketIndex = ItrObj.FBucketIndex) and (FItemIndex = ItrObj.FItemIndex) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclCardinalHashSetIterator.MoveNext: Boolean;
var
  ABucket: TJclCardinalHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := (FBucketIndex >= 0) and (FItemIndex >= 0) and
              (FBucketIndex < FOwnHashSet.FCapacity);
    if Result then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      Result := (ABucket <> nil) and (FItemIndex < ABucket.Size);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclCardinalHashSetIterator.Next: Cardinal;
var
  ABucket: TJclCardinalHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := 0;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSetIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclCardinalHashSetIterator.Previous: Cardinal;
var
  ABucket: TJclCardinalHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex >= 0 do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if (ABucket <> nil) and (FItemIndex < 0) then
          FItemIndex := ABucket.Size - 1;
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex >= 0) and (FItemIndex < ABucket.Size) then
            Break;
          if (FItemIndex > 0) and (FItemIndex < ABucket.Size) then
          begin
            Dec(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := -1;
        Dec(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := 0;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSetIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclCardinalHashSetIterator.Remove;
begin

end;

procedure TJclCardinalHashSetIterator.Reset;
var
  ABucket: TJclCardinalHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          FBucketIndex := 0;
          ABucket := nil;
          while FBucketIndex < FOwnHashSet.FCapacity do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Inc(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := 0
          else
            FItemIndex := -1;
        end;
      isLast:
        begin
          FBucketIndex := FOwnHashSet.FCapacity - 1;
          ABucket := nil;
          while FBucketIndex >= 0 do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Dec(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := ABucket.Size - 1
          else
            FItemIndex := -1;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashSetIterator.SetValue(AValue: Cardinal);
begin
  raise EJclOperationNotSupportedError.Create;
end;
//=== { TJclInt64HashSet } ====================================================

constructor TJclInt64HashSet.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclInt64HashSet.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclInt64HashSet.Add(const AValue: Int64): Boolean;
var
  Index: Integer;
  Bucket: TJclInt64HashSetBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if FAllowDefaultElements or (not ItemsEqual(AValue, 0)) then
    begin
      Index := FHashToRangeFunction(Hash(AValue), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if ItemsEqual(Bucket.Entries[I], AValue) then
            Exit;
      end
      else
      begin
        Bucket := TJclInt64HashSetBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size] := AValue;
        Inc(Bucket.Size);
        Inc(FSize);
        Result := True;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSet.AddAll(const ACollection: IJclInt64Collection): Boolean;
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
    Result := True;
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

procedure TJclInt64HashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclInt64HashSetBucket;
  ADest: TJclInt64HashSet;
  ACollection: IJclInt64Collection;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclInt64HashSet then
    begin
      ADest := TJclInt64HashSet(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclInt64HashSetBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
            NewBucket.Entries[J] := SelfBucket.Entries[J];
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclInt64Collection, ACollection) then
    begin
      ACollection.Clear;
      ACollection.AddAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashSet.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclInt64HashSet then
    TJclInt64HashSet(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclInt64HashSet.Clear;
var
  I, J: Integer;
  Bucket: TJclInt64HashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
          FreeInt64(Bucket.Entries[J]);
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSet.CollectionEquals(const ACollection: IJclInt64Collection): Boolean;
var
  I, J: Integer;
  It: IJclInt64Iterator;
  Bucket: TJclInt64HashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if not ItemsEqual(Bucket.Entries[J], It.Next) then
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSet.Contains(const AValue: Int64): Boolean;
var
  I: Integer;
  Bucket: TJclInt64HashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AValue), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AValue) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSet.ContainsAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSet.Extract(const AValue: Int64): Boolean;
var
  Bucket: TJclInt64HashSetBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AValue), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AValue) then
        begin
          Result := True;
          Bucket.Entries[I] := 0;
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSet.ExtractAll(const ACollection: IJclInt64Collection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSet.First: IJclInt64Iterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclInt64HashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := 0;
    ABucket := nil;
    while ABucketIndex < FCapacity do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Inc(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := 0
    else
      AItemIndex := -1;
    Result := TJclInt64HashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isFirst);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclInt64HashSet.GetEnumerator: IJclInt64Iterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

procedure TJclInt64HashSet.Intersect(const ACollection: IJclInt64Collection);
begin
  RetainAll(ACollection);
end;

function TJclInt64HashSet.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64HashSet.Last: IJclInt64Iterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclInt64HashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := FCapacity - 1;
    ABucket := nil;
    while ABucketIndex >= 0 do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Dec(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := ABucket.Size - 1
    else
      AItemIndex := -1;
    Result := TJclInt64HashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isLast);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashSet.Pack;
var
  I: Integer;
  Bucket: TJclInt64HashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSet.Remove(const AValue: Int64): Boolean;
var
  Extracted: Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AValue);
    if Result then
    begin
      Extracted := AValue;
      FreeInt64(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSet.RemoveAll(const ACollection: IJclInt64Collection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSet.RetainAll(const ACollection: IJclInt64Collection): Boolean;
var
  I, J, NewCapacity: Integer;
  Bucket: TJclInt64HashSetBucket;
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
    Result := True;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := Bucket.Size - 1 downto 0 do
          if not ACollection.Contains(Bucket.Entries[I]) then
        begin
          Bucket.Entries[J] := FreeInt64(Bucket.Entries[J]);
          if J < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, J + 1, J, Bucket.Size - J - 1);
          Dec(Bucket.Size);
          Dec(FSize);
        end;

        NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
        if NewCapacity < Length(Bucket.Entries) then
          SetLength(Bucket.Entries, NewCapacity);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashSet.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSet.Size: Integer;
begin
  Result := FSize;
end;

procedure TJclInt64HashSet.Subtract(const ACollection: IJclInt64Collection);
begin
  RemoveAll(ACollection);
end;

procedure TJclInt64HashSet.Union(const ACollection: IJclInt64Collection);
begin
  AddAll(ACollection);
end;


function TJclInt64HashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64HashSet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclInt64HashSetIterator } ============================================

constructor TJclInt64HashSetIterator.Create(AOwnHashSet: TJclInt64HashSet;
  ABucketIndex, AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnHashSet := AOwnHashSet;
  FBucketIndex := ABucketIndex;
  FItemIndex := AItemIndex;
  FStart := AStart;
end;

function TJclInt64HashSetIterator.Add(const AValue: Int64): Boolean;
begin
  Result := FOwnHashSet.Add(AValue);
end;

procedure TJclInt64HashSetIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclInt64HashSetIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclInt64HashSetIterator then
  begin
    ADest := TJclInt64HashSetIterator(Dest);
    ADest.FBucketIndex := FBucketIndex;
    ADest.FItemIndex := FItemIndex;
    ADest.FOwnHashSet := FOwnHashSet;
    ADest.FStart := FStart;
  end;
end;

function TJclInt64HashSetIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInt64HashSetIterator.Create(FOwnHashSet, FBucketIndex, FItemIndex, Valid, FStart);
end;

procedure TJclInt64HashSetIterator.Extract;
var
  AValue: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    AValue := GetValue;
    Valid := False;
    FOwnHashSet.Extract(AValue);
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSetIterator.GetValue: Int64;
var
  ABucket: TJclInt64HashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0;
    ABucket := FOwnHashSet.FBuckets[FBucketIndex - 1];
    if (ABucket <> nil) and (FItemIndex < ABucket.Size) then
      Result := ABucket.Entries[FItemIndex]
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSetIterator.HasNext: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclInt64HashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex < FOwnHashSet.FCapacity do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex < (ABucket.Size - 1)) or
          ((not SkipCurrent) and (AItemIndex < ABucket.Size)) then
          Exit;
      end;
      AItemIndex := 0;
      Inc(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSetIterator.HasPrevious: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclInt64HashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex >= 0 do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex > 0) or
          ((not SkipCurrent) and (AItemIndex >= 0)) then
          Exit;
      end;
      AItemIndex := MaxInt;
      Dec(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSetIterator.Insert(const AValue: Int64): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclInt64HashSetIterator.IteratorEquals(const AIterator: IJclInt64Iterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclInt64HashSetIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclInt64HashSetIterator then
  begin
    ItrObj := TJclInt64HashSetIterator(Obj);
    Result := (FOwnHashSet = ItrObj.FOwnHashSet) and (FBucketIndex = ItrObj.FBucketIndex) and (FItemIndex = ItrObj.FItemIndex) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclInt64HashSetIterator.MoveNext: Boolean;
var
  ABucket: TJclInt64HashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := (FBucketIndex >= 0) and (FItemIndex >= 0) and
              (FBucketIndex < FOwnHashSet.FCapacity);
    if Result then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      Result := (ABucket <> nil) and (FItemIndex < ABucket.Size);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclInt64HashSetIterator.Next: Int64;
var
  ABucket: TJclInt64HashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := 0;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSetIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclInt64HashSetIterator.Previous: Int64;
var
  ABucket: TJclInt64HashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex >= 0 do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if (ABucket <> nil) and (FItemIndex < 0) then
          FItemIndex := ABucket.Size - 1;
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex >= 0) and (FItemIndex < ABucket.Size) then
            Break;
          if (FItemIndex > 0) and (FItemIndex < ABucket.Size) then
          begin
            Dec(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := -1;
        Dec(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := 0;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSetIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclInt64HashSetIterator.Remove;
begin

end;

procedure TJclInt64HashSetIterator.Reset;
var
  ABucket: TJclInt64HashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          FBucketIndex := 0;
          ABucket := nil;
          while FBucketIndex < FOwnHashSet.FCapacity do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Inc(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := 0
          else
            FItemIndex := -1;
        end;
      isLast:
        begin
          FBucketIndex := FOwnHashSet.FCapacity - 1;
          ABucket := nil;
          while FBucketIndex >= 0 do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Dec(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := ABucket.Size - 1
          else
            FItemIndex := -1;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashSetIterator.SetValue(const AValue: Int64);
begin
  raise EJclOperationNotSupportedError.Create;
end;
//=== { TJclPtrHashSet } ====================================================

constructor TJclPtrHashSet.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclPtrHashSet.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclPtrHashSet.Add(APtr: Pointer): Boolean;
var
  Index: Integer;
  Bucket: TJclPtrHashSetBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if FAllowDefaultElements or (not ItemsEqual(APtr, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(APtr), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if ItemsEqual(Bucket.Entries[I], APtr) then
            Exit;
      end
      else
      begin
        Bucket := TJclPtrHashSetBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size] := APtr;
        Inc(Bucket.Size);
        Inc(FSize);
        Result := True;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSet.AddAll(const ACollection: IJclPtrCollection): Boolean;
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
    Result := True;
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

procedure TJclPtrHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclPtrHashSetBucket;
  ADest: TJclPtrHashSet;
  ACollection: IJclPtrCollection;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclPtrHashSet then
    begin
      ADest := TJclPtrHashSet(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclPtrHashSetBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
            NewBucket.Entries[J] := SelfBucket.Entries[J];
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclPtrCollection, ACollection) then
    begin
      ACollection.Clear;
      ACollection.AddAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashSet.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclPtrHashSet then
    TJclPtrHashSet(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclPtrHashSet.Clear;
var
  I, J: Integer;
  Bucket: TJclPtrHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
          FreePointer(Bucket.Entries[J]);
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSet.CollectionEquals(const ACollection: IJclPtrCollection): Boolean;
var
  I, J: Integer;
  It: IJclPtrIterator;
  Bucket: TJclPtrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if not ItemsEqual(Bucket.Entries[J], It.Next) then
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSet.Contains(APtr: Pointer): Boolean;
var
  I: Integer;
  Bucket: TJclPtrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(APtr), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], APtr) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSet.ContainsAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSet.Extract(APtr: Pointer): Boolean;
var
  Bucket: TJclPtrHashSetBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(APtr), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], APtr) then
        begin
          Result := True;
          Bucket.Entries[I] := nil;
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSet.ExtractAll(const ACollection: IJclPtrCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSet.First: IJclPtrIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclPtrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := 0;
    ABucket := nil;
    while ABucketIndex < FCapacity do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Inc(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := 0
    else
      AItemIndex := -1;
    Result := TJclPtrHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isFirst);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclPtrHashSet.GetEnumerator: IJclPtrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

procedure TJclPtrHashSet.Intersect(const ACollection: IJclPtrCollection);
begin
  RetainAll(ACollection);
end;

function TJclPtrHashSet.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrHashSet.Last: IJclPtrIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclPtrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := FCapacity - 1;
    ABucket := nil;
    while ABucketIndex >= 0 do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Dec(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := ABucket.Size - 1
    else
      AItemIndex := -1;
    Result := TJclPtrHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isLast);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashSet.Pack;
var
  I: Integer;
  Bucket: TJclPtrHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSet.Remove(APtr: Pointer): Boolean;
var
  Extracted: Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(APtr);
    if Result then
    begin
      Extracted := APtr;
      FreePointer(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSet.RemoveAll(const ACollection: IJclPtrCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSet.RetainAll(const ACollection: IJclPtrCollection): Boolean;
var
  I, J, NewCapacity: Integer;
  Bucket: TJclPtrHashSetBucket;
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
    Result := True;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := Bucket.Size - 1 downto 0 do
          if not ACollection.Contains(Bucket.Entries[I]) then
        begin
          Bucket.Entries[J] := FreePointer(Bucket.Entries[J]);
          if J < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, J + 1, J, Bucket.Size - J - 1);
          Dec(Bucket.Size);
          Dec(FSize);
        end;

        NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
        if NewCapacity < Length(Bucket.Entries) then
          SetLength(Bucket.Entries, NewCapacity);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashSet.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSet.Size: Integer;
begin
  Result := FSize;
end;

procedure TJclPtrHashSet.Subtract(const ACollection: IJclPtrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclPtrHashSet.Union(const ACollection: IJclPtrCollection);
begin
  AddAll(ACollection);
end;


function TJclPtrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrHashSet.Create(Size);
  AssignPropertiesTo(Result);
end;

//=== { TJclPtrHashSetIterator } ============================================

constructor TJclPtrHashSetIterator.Create(AOwnHashSet: TJclPtrHashSet;
  ABucketIndex, AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnHashSet := AOwnHashSet;
  FBucketIndex := ABucketIndex;
  FItemIndex := AItemIndex;
  FStart := AStart;
end;

function TJclPtrHashSetIterator.Add(APtr: Pointer): Boolean;
begin
  Result := FOwnHashSet.Add(APtr);
end;

procedure TJclPtrHashSetIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclPtrHashSetIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclPtrHashSetIterator then
  begin
    ADest := TJclPtrHashSetIterator(Dest);
    ADest.FBucketIndex := FBucketIndex;
    ADest.FItemIndex := FItemIndex;
    ADest.FOwnHashSet := FOwnHashSet;
    ADest.FStart := FStart;
  end;
end;

function TJclPtrHashSetIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPtrHashSetIterator.Create(FOwnHashSet, FBucketIndex, FItemIndex, Valid, FStart);
end;

procedure TJclPtrHashSetIterator.Extract;
var
  APtr: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    APtr := GetPointer;
    Valid := False;
    FOwnHashSet.Extract(APtr);
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSetIterator.GetPointer: Pointer;
var
  ABucket: TJclPtrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := nil;
    ABucket := FOwnHashSet.FBuckets[FBucketIndex - 1];
    if (ABucket <> nil) and (FItemIndex < ABucket.Size) then
      Result := ABucket.Entries[FItemIndex]
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSetIterator.HasNext: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclPtrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex < FOwnHashSet.FCapacity do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex < (ABucket.Size - 1)) or
          ((not SkipCurrent) and (AItemIndex < ABucket.Size)) then
          Exit;
      end;
      AItemIndex := 0;
      Inc(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSetIterator.HasPrevious: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclPtrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex >= 0 do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex > 0) or
          ((not SkipCurrent) and (AItemIndex >= 0)) then
          Exit;
      end;
      AItemIndex := MaxInt;
      Dec(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSetIterator.Insert(APtr: Pointer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclPtrHashSetIterator.IteratorEquals(const AIterator: IJclPtrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclPtrHashSetIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclPtrHashSetIterator then
  begin
    ItrObj := TJclPtrHashSetIterator(Obj);
    Result := (FOwnHashSet = ItrObj.FOwnHashSet) and (FBucketIndex = ItrObj.FBucketIndex) and (FItemIndex = ItrObj.FItemIndex) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclPtrHashSetIterator.MoveNext: Boolean;
var
  ABucket: TJclPtrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := (FBucketIndex >= 0) and (FItemIndex >= 0) and
              (FBucketIndex < FOwnHashSet.FCapacity);
    if Result then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      Result := (ABucket <> nil) and (FItemIndex < ABucket.Size);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclPtrHashSetIterator.Next: Pointer;
var
  ABucket: TJclPtrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := nil;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSetIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclPtrHashSetIterator.Previous: Pointer;
var
  ABucket: TJclPtrHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex >= 0 do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if (ABucket <> nil) and (FItemIndex < 0) then
          FItemIndex := ABucket.Size - 1;
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex >= 0) and (FItemIndex < ABucket.Size) then
            Break;
          if (FItemIndex > 0) and (FItemIndex < ABucket.Size) then
          begin
            Dec(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := -1;
        Dec(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := nil;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSetIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclPtrHashSetIterator.Remove;
begin

end;

procedure TJclPtrHashSetIterator.Reset;
var
  ABucket: TJclPtrHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          FBucketIndex := 0;
          ABucket := nil;
          while FBucketIndex < FOwnHashSet.FCapacity do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Inc(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := 0
          else
            FItemIndex := -1;
        end;
      isLast:
        begin
          FBucketIndex := FOwnHashSet.FCapacity - 1;
          ABucket := nil;
          while FBucketIndex >= 0 do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Dec(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := ABucket.Size - 1
          else
            FItemIndex := -1;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashSetIterator.SetPointer(APtr: Pointer);
begin
  raise EJclOperationNotSupportedError.Create;
end;
//=== { TJclHashSet } ====================================================

constructor TJclHashSet.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclHashSet.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclHashSet.Add(AObject: TObject): Boolean;
var
  Index: Integer;
  Bucket: TJclHashSetBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if FAllowDefaultElements or (not ItemsEqual(AObject, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(AObject), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if ItemsEqual(Bucket.Entries[I], AObject) then
            Exit;
      end
      else
      begin
        Bucket := TJclHashSetBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size] := AObject;
        Inc(Bucket.Size);
        Inc(FSize);
        Result := True;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.AddAll(const ACollection: IJclCollection): Boolean;
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
    Result := True;
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

procedure TJclHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclHashSetBucket;
  ADest: TJclHashSet;
  ACollection: IJclCollection;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclHashSet then
    begin
      ADest := TJclHashSet(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclHashSetBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
            NewBucket.Entries[J] := SelfBucket.Entries[J];
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclCollection, ACollection) then
    begin
      ACollection.Clear;
      ACollection.AddAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclHashSet then
    TJclHashSet(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclHashSet.Clear;
var
  I, J: Integer;
  Bucket: TJclHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
          FreeObject(Bucket.Entries[J]);
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.CollectionEquals(const ACollection: IJclCollection): Boolean;
var
  I, J: Integer;
  It: IJclIterator;
  Bucket: TJclHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if not ItemsEqual(Bucket.Entries[J], It.Next) then
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.Contains(AObject: TObject): Boolean;
var
  I: Integer;
  Bucket: TJclHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AObject), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AObject) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.ContainsAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.Extract(AObject: TObject): Boolean;
var
  Bucket: TJclHashSetBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AObject), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AObject) then
        begin
          Result := True;
          Bucket.Entries[I] := nil;
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.ExtractAll(const ACollection: IJclCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.First: IJclIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := 0;
    ABucket := nil;
    while ABucketIndex < FCapacity do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Inc(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := 0
    else
      AItemIndex := -1;
    Result := TJclHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isFirst);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclHashSet.GetEnumerator: IJclIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

procedure TJclHashSet.Intersect(const ACollection: IJclCollection);
begin
  RetainAll(ACollection);
end;

function TJclHashSet.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclHashSet.Last: IJclIterator;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := FCapacity - 1;
    ABucket := nil;
    while ABucketIndex >= 0 do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Dec(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := ABucket.Size - 1
    else
      AItemIndex := -1;
    Result := TJclHashSetIterator.Create(Self, ABucketIndex, AItemIndex,  False, isLast);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet.Pack;
var
  I: Integer;
  Bucket: TJclHashSetBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.Remove(AObject: TObject): Boolean;
var
  Extracted: TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AObject);
    if Result then
    begin
      Extracted := AObject;
      FreeObject(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.RemoveAll(const ACollection: IJclCollection): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.RetainAll(const ACollection: IJclCollection): Boolean;
var
  I, J, NewCapacity: Integer;
  Bucket: TJclHashSetBucket;
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
    Result := True;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := Bucket.Size - 1 downto 0 do
          if not ACollection.Contains(Bucket.Entries[I]) then
        begin
          Bucket.Entries[J] := FreeObject(Bucket.Entries[J]);
          if J < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, J + 1, J, Bucket.Size - J - 1);
          Dec(Bucket.Size);
          Dec(FSize);
        end;

        NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
        if NewCapacity < Length(Bucket.Entries) then
          SetLength(Bucket.Entries, NewCapacity);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.Size: Integer;
begin
  Result := FSize;
end;

procedure TJclHashSet.Subtract(const ACollection: IJclCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclHashSet.Union(const ACollection: IJclCollection);
begin
  AddAll(ACollection);
end;


function TJclHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashSet.Create(Size, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclHashSetIterator } ============================================

constructor TJclHashSetIterator.Create(AOwnHashSet: TJclHashSet;
  ABucketIndex, AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnHashSet := AOwnHashSet;
  FBucketIndex := ABucketIndex;
  FItemIndex := AItemIndex;
  FStart := AStart;
end;

function TJclHashSetIterator.Add(AObject: TObject): Boolean;
begin
  Result := FOwnHashSet.Add(AObject);
end;

procedure TJclHashSetIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclHashSetIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashSetIterator then
  begin
    ADest := TJclHashSetIterator(Dest);
    ADest.FBucketIndex := FBucketIndex;
    ADest.FItemIndex := FItemIndex;
    ADest.FOwnHashSet := FOwnHashSet;
    ADest.FStart := FStart;
  end;
end;

function TJclHashSetIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclHashSetIterator.Create(FOwnHashSet, FBucketIndex, FItemIndex, Valid, FStart);
end;

procedure TJclHashSetIterator.Extract;
var
  AObject: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    AObject := GetObject;
    Valid := False;
    FOwnHashSet.Extract(AObject);
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSetIterator.GetObject: TObject;
var
  ABucket: TJclHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := nil;
    ABucket := FOwnHashSet.FBuckets[FBucketIndex - 1];
    if (ABucket <> nil) and (FItemIndex < ABucket.Size) then
      Result := ABucket.Entries[FItemIndex]
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSetIterator.HasNext: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex < FOwnHashSet.FCapacity do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex < (ABucket.Size - 1)) or
          ((not SkipCurrent) and (AItemIndex < ABucket.Size)) then
          Exit;
      end;
      AItemIndex := 0;
      Inc(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSetIterator.HasPrevious: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex >= 0 do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex > 0) or
          ((not SkipCurrent) and (AItemIndex >= 0)) then
          Exit;
      end;
      AItemIndex := MaxInt;
      Dec(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSetIterator.Insert(AObject: TObject): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclHashSetIterator.IteratorEquals(const AIterator: IJclIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclHashSetIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclHashSetIterator then
  begin
    ItrObj := TJclHashSetIterator(Obj);
    Result := (FOwnHashSet = ItrObj.FOwnHashSet) and (FBucketIndex = ItrObj.FBucketIndex) and (FItemIndex = ItrObj.FItemIndex) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclHashSetIterator.MoveNext: Boolean;
var
  ABucket: TJclHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := (FBucketIndex >= 0) and (FItemIndex >= 0) and
              (FBucketIndex < FOwnHashSet.FCapacity);
    if Result then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      Result := (ABucket <> nil) and (FItemIndex < ABucket.Size);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclHashSetIterator.Next: TObject;
var
  ABucket: TJclHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := nil;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSetIterator.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclHashSetIterator.Previous: TObject;
var
  ABucket: TJclHashSetBucket;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex >= 0 do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if (ABucket <> nil) and (FItemIndex < 0) then
          FItemIndex := ABucket.Size - 1;
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex >= 0) and (FItemIndex < ABucket.Size) then
            Break;
          if (FItemIndex > 0) and (FItemIndex < ABucket.Size) then
          begin
            Dec(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := -1;
        Dec(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := nil;
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSetIterator.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclHashSetIterator.Remove;
begin

end;

procedure TJclHashSetIterator.Reset;
var
  ABucket: TJclHashSetBucket;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          FBucketIndex := 0;
          ABucket := nil;
          while FBucketIndex < FOwnHashSet.FCapacity do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Inc(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := 0
          else
            FItemIndex := -1;
        end;
      isLast:
        begin
          FBucketIndex := FOwnHashSet.FCapacity - 1;
          ABucket := nil;
          while FBucketIndex >= 0 do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Dec(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := ABucket.Size - 1
          else
            FItemIndex := -1;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSetIterator.SetObject(AObject: TObject);
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

//=== { TJclHashSetBucket<T> } =================================================

procedure TJclHashSetBucket<T>.MoveArray(var List: TDynArray; FromIndex, ToIndex, Count: SizeInt);
var
  I: SizeInt;
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

//=== { TJclHashSet<T> } ====================================================

constructor TJclHashSet<T>.Create(ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclHashSet<T>.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclHashSet<T>.Add(const AItem: T): Boolean;
var
  Index: Integer;
  Bucket: TJclHashSetBucket<T>;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if FAllowDefaultElements or (not ItemsEqual(AItem, Default(T))) then
    begin
      Index := FHashToRangeFunction(Hash(AItem), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if ItemsEqual(Bucket.Entries[I], AItem) then
            Exit;
      end
      else
      begin
        Bucket := TJclHashSetBucket<T>.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size] := AItem;
        Inc(Bucket.Size);
        Inc(FSize);
        Result := True;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
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
    Result := True;
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

procedure TJclHashSet<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclHashSetBucket<T>;
  ADest: TJclHashSet<T>;
  ACollection: IJclCollection<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclHashSet<T> then
    begin
      ADest := TJclHashSet<T>(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclHashSetBucket<T>.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
            NewBucket.Entries[J] := SelfBucket.Entries[J];
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclCollection<T>, ACollection) then
    begin
      ACollection.Clear;
      ACollection.AddAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclHashSet<T> then
    TJclHashSet<T>(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclHashSet<T>.Clear;
var
  I, J: Integer;
  Bucket: TJclHashSetBucket<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
          FreeItem(Bucket.Entries[J]);
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.CollectionEquals(const ACollection: IJclCollection<T>): Boolean;
var
  I, J: Integer;
  It: IJclIterator<T>;
  Bucket: TJclHashSetBucket<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if not ItemsEqual(Bucket.Entries[J], It.Next) then
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.Contains(const AItem: T): Boolean;
var
  I: Integer;
  Bucket: TJclHashSetBucket<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AItem), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AItem) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.Extract(const AItem: T): Boolean;
var
  Bucket: TJclHashSetBucket<T>;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(AItem), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if ItemsEqual(Bucket.Entries[I], AItem) then
        begin
          Result := True;
          Bucket.Entries[I] := Default(T);
          if I < Length(Bucket.Entries) - 1 then
            Bucket.MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.ExtractAll(const ACollection: IJclCollection<T>): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Extract(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.First: IJclIterator<T>;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclHashSetBucket<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := 0;
    ABucket := nil;
    while ABucketIndex < FCapacity do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Inc(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := 0
    else
      AItemIndex := -1;
    Result := TJclHashSetIterator<T>.Create(Self, ABucketIndex, AItemIndex,  False, isFirst);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclHashSet<T>.GetEnumerator: IJclIterator<T>;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

procedure TJclHashSet<T>.Intersect(const ACollection: IJclCollection<T>);
begin
  RetainAll(ACollection);
end;

function TJclHashSet<T>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclHashSet<T>.Last: IJclIterator<T>;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclHashSetBucket<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    ABucketIndex := FCapacity - 1;
    ABucket := nil;
    while ABucketIndex >= 0 do
    begin
      ABucket := FBuckets[ABucketIndex];
      if (ABucket <> nil) and (ABucket.Size > 0) then
        Break;
      Dec(ABucketIndex);
    end;
    if ABucket <> nil then
      AItemIndex := ABucket.Size - 1
    else
      AItemIndex := -1;
    Result := TJclHashSetIterator<T>.Create(Self, ABucketIndex, AItemIndex,  False, isLast);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet<T>.Pack;
var
  I: Integer;
  Bucket: TJclHashSetBucket<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.Remove(const AItem: T): Boolean;
var
  Extracted: T;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(AItem);
    if Result then
    begin
      Extracted := AItem;
      FreeItem(Extracted);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
var
  I, J, NewCapacity: Integer;
  Bucket: TJclHashSetBucket<T>;
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
    Result := True;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := Bucket.Size - 1 downto 0 do
          if not ACollection.Contains(Bucket.Entries[I]) then
        begin
          Bucket.Entries[J] := FreeItem(Bucket.Entries[J]);
          if J < Length(Bucket.Entries) - 1 then
            Bucket.MoveArray(Bucket.Entries, J + 1, J, Bucket.Size - J - 1);
          Dec(Bucket.Size);
          Dec(FSize);
        end;

        NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
        if NewCapacity < Length(Bucket.Entries) then
          SetLength(Bucket.Entries, NewCapacity);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet<T>.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.Size: Integer;
begin
  Result := FSize;
end;

procedure TJclHashSet<T>.Subtract(const ACollection: IJclCollection<T>);
begin
  RemoveAll(ACollection);
end;

procedure TJclHashSet<T>.Union(const ACollection: IJclCollection<T>);
begin
  AddAll(ACollection);
end;

//=== { TJclHashSetIterator<T> } ============================================

constructor TJclHashSetIterator<T>.Create(AOwnHashSet: TJclHashSet<T>;
  ABucketIndex, AItemIndex: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnHashSet := AOwnHashSet;
  FBucketIndex := ABucketIndex;
  FItemIndex := AItemIndex;
  FStart := AStart;
end;

function TJclHashSetIterator<T>.Add(const AItem: T): Boolean;
begin
  Result := FOwnHashSet.Add(AItem);
end;

procedure TJclHashSetIterator<T>.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclHashSetIterator<T>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashSetIterator<T> then
  begin
    ADest := TJclHashSetIterator<T>(Dest);
    ADest.FBucketIndex := FBucketIndex;
    ADest.FItemIndex := FItemIndex;
    ADest.FOwnHashSet := FOwnHashSet;
    ADest.FStart := FStart;
  end;
end;

function TJclHashSetIterator<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclHashSetIterator<T>.Create(FOwnHashSet, FBucketIndex, FItemIndex, Valid, FStart);
end;

procedure TJclHashSetIterator<T>.Extract;
var
  AItem: T;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    AItem := GetItem;
    Valid := False;
    FOwnHashSet.Extract(AItem);
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSetIterator<T>.GetItem: T;
var
  ABucket: TJclHashSetBucket<T>;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := Default(T);
    ABucket := FOwnHashSet.FBuckets[FBucketIndex - 1];
    if (ABucket <> nil) and (FItemIndex < ABucket.Size) then
      Result := ABucket.Entries[FItemIndex]
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSetIterator<T>.HasNext: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclHashSetBucket<T>;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex < FOwnHashSet.FCapacity do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex < (ABucket.Size - 1)) or
          ((not SkipCurrent) and (AItemIndex < ABucket.Size)) then
          Exit;
      end;
      AItemIndex := 0;
      Inc(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSetIterator<T>.HasPrevious: Boolean;
var
  ABucketIndex, AItemIndex: Integer;
  ABucket: TJclHashSetBucket<T>;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    ABucketIndex := FBucketIndex;
    AItemIndex := FItemIndex;
    SkipCurrent := Valid;
    while ABucketIndex >= 0 do
    begin
      ABucket := FOwnHashSet.FBuckets[ABucketIndex];
      if ABucket <> nil then
      begin
        if (AItemIndex > 0) or
          ((not SkipCurrent) and (AItemIndex >= 0)) then
          Exit;
      end;
      AItemIndex := MaxInt;
      Dec(ABucketIndex);
      SkipCurrent := False;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSetIterator<T>.Insert(const AItem: T): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclHashSetIterator<T>.IteratorEquals(const AIterator: IJclIterator<T>): Boolean;
var
  Obj: TObject;
  ItrObj: TJclHashSetIterator<T>;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclHashSetIterator<T> then
  begin
    ItrObj := TJclHashSetIterator<T>(Obj);
    Result := (FOwnHashSet = ItrObj.FOwnHashSet) and (FBucketIndex = ItrObj.FBucketIndex) and (FItemIndex = ItrObj.FItemIndex) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclHashSetIterator<T>.MoveNext: Boolean;
var
  ABucket: TJclHashSetBucket<T>;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := (FBucketIndex >= 0) and (FItemIndex >= 0) and
              (FBucketIndex < FOwnHashSet.FCapacity);
    if Result then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      Result := (ABucket <> nil) and (FItemIndex < ABucket.Size);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclHashSetIterator<T>.Next: T;
var
  ABucket: TJclHashSetBucket<T>;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex < FOwnHashSet.FCapacity do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex < ABucket.Size) then
            Break;
          if FItemIndex < (ABucket.Size - 1) then
          begin
            Inc(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := 0;
        Inc(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := Default(T);
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSetIterator<T>.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclHashSetIterator<T>.Previous: T;
var
  ABucket: TJclHashSetBucket<T>;
  SkipCurrent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
    begin
      SkipCurrent := True;
      while FBucketIndex >= 0 do
      begin
        ABucket := FOwnHashSet.FBuckets[FBucketIndex];
        if (ABucket <> nil) and (FItemIndex < 0) then
          FItemIndex := ABucket.Size - 1;
        if ABucket <> nil then
        begin
          if (not SkipCurrent) and (FItemIndex >= 0) and (FItemIndex < ABucket.Size) then
            Break;
          if (FItemIndex > 0) and (FItemIndex < ABucket.Size) then
          begin
            Dec(FItemIndex);
            Break;
          end;
        end;
        FItemIndex := -1;
        Dec(FBucketIndex);
        SkipCurrent := False;
      end;
    end
    else
      Valid := True;

    Result := Default(T);
    if (FBucketIndex >= 0) and (FItemIndex >= 0) and
       (FBucketIndex < FOwnHashSet.FCapacity) then
    begin
      ABucket := FOwnHashSet.FBuckets[FBucketIndex];
      if (ABucket <> nil) and
         (FItemIndex < ABucket.Size) then
        Result := ABucket.Entries[FItemIndex]
      else
      if not FOwnHashSet.ReturnDefaultElements then
        raise EJclNoSuchElementError.Create('');
    end
    else
    if not FOwnHashSet.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSetIterator<T>.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclHashSetIterator<T>.Remove;
begin

end;

procedure TJclHashSetIterator<T>.Reset;
var
  ABucket: TJclHashSetBucket<T>;
begin
  {$IFDEF THREADSAFE}
  FOwnHashSet.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          FBucketIndex := 0;
          ABucket := nil;
          while FBucketIndex < FOwnHashSet.FCapacity do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Inc(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := 0
          else
            FItemIndex := -1;
        end;
      isLast:
        begin
          FBucketIndex := FOwnHashSet.FCapacity - 1;
          ABucket := nil;
          while FBucketIndex >= 0 do
          begin
            ABucket := FOwnHashSet.FBuckets[FBucketIndex];
            if (ABucket <> nil) and (ABucket.Size > 0) then
              Break;
            Dec(FBucketIndex);
          end;
          if ABucket <> nil then
            FItemIndex := ABucket.Size - 1
          else
            FItemIndex := -1;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnHashSet.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSetIterator<T>.SetItem(const AItem: T);
begin
  raise EJclOperationNotSupportedError.Create;
end;

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

