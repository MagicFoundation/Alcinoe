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
{ The Original Code is Vector.pas.                                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Daniele Teti (dade2004)                                                                        }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
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

unit JclVectors;

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
  JclAlgorithms, JclBase, JclAbstractContainers, JclContainerIntf, JclSynch;


type
  TItrStart = (isFirst, isLast);
  TJclIntfVector = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclIntfContainer, IJclIntfFlatContainer, IJclIntfEqualityComparer,
    IJclIntfCollection, IJclIntfList, IJclIntfArray)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FItems: TDynIInterfaceArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
    // complaining about possible unaffected result.
    function RaiseOutOfBoundsError: IInterface;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclIntfCollection); overload;
    destructor Destroy; override;
    property Items: TDynIInterfaceArray read FItems;
    { IJclPackable }
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
    { IJclIntfList }
    function Delete(Index: Integer): IInterface;
    function ExtractIndex(Index: Integer): IInterface;
    function GetObject(Index: Integer): IInterface;
    function IndexOf(const AInterface: IInterface): Integer;
    function Insert(Index: Integer; const AInterface: IInterface): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
    function LastIndexOf(const AInterface: IInterface): Integer;
    procedure SetObject(Index: Integer; const AInterface: IInterface);
    function SubList(First, Count: Integer): IJclIntfList;
  end;

  TJclIntfVectorIterator = class(TJclAbstractIterator, IJclIntfIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: TJclIntfVector;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
  public
    constructor Create(AOwnList: TJclIntfVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
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

  TJclAnsiStrVector = class(TJclAnsiStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclAnsiStrContainer, IJclAnsiStrFlatContainer, IJclAnsiStrEqualityComparer, IJclStrBaseContainer,
    IJclAnsiStrCollection, IJclAnsiStrList, IJclAnsiStrArray)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FItems: TDynAnsiStringArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
    // complaining about possible unaffected result.
    function RaiseOutOfBoundsError: AnsiString;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclAnsiStrCollection); overload;
    destructor Destroy; override;
    property Items: TDynAnsiStringArray read FItems;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrCollection }
    function Add(const AString: AnsiString): Boolean; override;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    procedure Clear; override;
    function CollectionEquals(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Contains(const AString: AnsiString): Boolean; override;
    function ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Extract(const AString: AnsiString): Boolean;  override;
    function ExtractAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function First: IJclAnsiStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclAnsiStrIterator; override;
    function Remove(const AString: AnsiString): Boolean;  override;
    function RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclAnsiStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclAnsiStrList }
    function Delete(Index: Integer): AnsiString;
    function ExtractIndex(Index: Integer): AnsiString;
    function GetString(Index: Integer): AnsiString;
    function IndexOf(const AString: AnsiString): Integer;
    function Insert(Index: Integer; const AString: AnsiString): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclAnsiStrCollection): Boolean;
    function LastIndexOf(const AString: AnsiString): Integer;
    procedure SetString(Index: Integer; const AString: AnsiString);
    function SubList(First, Count: Integer): IJclAnsiStrList;
  end;

  TJclAnsiStrVectorIterator = class(TJclAbstractIterator, IJclAnsiStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: TJclAnsiStrVector;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
  public
    constructor Create(AOwnList: TJclAnsiStrVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
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

  TJclWideStrVector = class(TJclWideStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclWideStrContainer, IJclWideStrFlatContainer, IJclWideStrEqualityComparer, IJclStrBaseContainer,
    IJclWideStrCollection, IJclWideStrList, IJclWideStrArray)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FItems: TDynWideStringArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
    // complaining about possible unaffected result.
    function RaiseOutOfBoundsError: WideString;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclWideStrCollection); overload;
    destructor Destroy; override;
    property Items: TDynWideStringArray read FItems;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrCollection }
    function Add(const AString: WideString): Boolean; override;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean; override;
    procedure Clear; override;
    function CollectionEquals(const ACollection: IJclWideStrCollection): Boolean; override;
    function Contains(const AString: WideString): Boolean; override;
    function ContainsAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Extract(const AString: WideString): Boolean;  override;
    function ExtractAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function First: IJclWideStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclWideStrIterator; override;
    function Remove(const AString: WideString): Boolean;  override;
    function RemoveAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclWideStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclWideStrList }
    function Delete(Index: Integer): WideString;
    function ExtractIndex(Index: Integer): WideString;
    function GetString(Index: Integer): WideString;
    function IndexOf(const AString: WideString): Integer;
    function Insert(Index: Integer; const AString: WideString): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclWideStrCollection): Boolean;
    function LastIndexOf(const AString: WideString): Integer;
    procedure SetString(Index: Integer; const AString: WideString);
    function SubList(First, Count: Integer): IJclWideStrList;
  end;

  TJclWideStrVectorIterator = class(TJclAbstractIterator, IJclWideStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: TJclWideStrVector;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
  public
    constructor Create(AOwnList: TJclWideStrVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
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
  TJclUnicodeStrVector = class(TJclUnicodeStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclUnicodeStrContainer, IJclUnicodeStrFlatContainer, IJclUnicodeStrEqualityComparer, IJclStrBaseContainer,
    IJclUnicodeStrCollection, IJclUnicodeStrList, IJclUnicodeStrArray)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FItems: TDynUnicodeStringArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
    // complaining about possible unaffected result.
    function RaiseOutOfBoundsError: UnicodeString;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclUnicodeStrCollection); overload;
    destructor Destroy; override;
    property Items: TDynUnicodeStringArray read FItems;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclUnicodeStrCollection }
    function Add(const AString: UnicodeString): Boolean; override;
    function AddAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    procedure Clear; override;
    function CollectionEquals(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function Contains(const AString: UnicodeString): Boolean; override;
    function ContainsAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function Extract(const AString: UnicodeString): Boolean;  override;
    function ExtractAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function First: IJclUnicodeStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclUnicodeStrIterator; override;
    function Remove(const AString: UnicodeString): Boolean;  override;
    function RemoveAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclUnicodeStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclUnicodeStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclUnicodeStrList }
    function Delete(Index: Integer): UnicodeString;
    function ExtractIndex(Index: Integer): UnicodeString;
    function GetString(Index: Integer): UnicodeString;
    function IndexOf(const AString: UnicodeString): Integer;
    function Insert(Index: Integer; const AString: UnicodeString): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclUnicodeStrCollection): Boolean;
    function LastIndexOf(const AString: UnicodeString): Integer;
    procedure SetString(Index: Integer; const AString: UnicodeString);
    function SubList(First, Count: Integer): IJclUnicodeStrList;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrVectorIterator = class(TJclAbstractIterator, IJclUnicodeStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: TJclUnicodeStrVector;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
  public
    constructor Create(AOwnList: TJclUnicodeStrVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
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
  TJclStrVector = TJclAnsiStrVector;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrVector = TJclWideStrVector;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrVector = TJclUnicodeStrVector;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrVectorIterator = TJclAnsiStrVectorIterator;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrVectorIterator = TJclWideStrVectorIterator;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrVectorIterator = TJclUnicodeStrVectorIterator;
  {$ENDIF CONTAINER_UNICODESTR}

  TJclSingleVector = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclSingleContainer, IJclSingleFlatContainer, IJclSingleEqualityComparer,
    IJclSingleCollection, IJclSingleList, IJclSingleArray)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FItems: TDynSingleArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
    // complaining about possible unaffected result.
    function RaiseOutOfBoundsError: Single;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclSingleCollection); overload;
    destructor Destroy; override;
    property Items: TDynSingleArray read FItems;
    { IJclPackable }
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
    { IJclSingleList }
    function Delete(Index: Integer): Single;
    function ExtractIndex(Index: Integer): Single;
    function GetValue(Index: Integer): Single;
    function IndexOf(const AValue: Single): Integer;
    function Insert(Index: Integer; const AValue: Single): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclSingleCollection): Boolean;
    function LastIndexOf(const AValue: Single): Integer;
    procedure SetValue(Index: Integer; const AValue: Single);
    function SubList(First, Count: Integer): IJclSingleList;
  end;

  TJclSingleVectorIterator = class(TJclAbstractIterator, IJclSingleIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: TJclSingleVector;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
  public
    constructor Create(AOwnList: TJclSingleVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
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

  TJclDoubleVector = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclDoubleContainer, IJclDoubleFlatContainer, IJclDoubleEqualityComparer,
    IJclDoubleCollection, IJclDoubleList, IJclDoubleArray)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FItems: TDynDoubleArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
    // complaining about possible unaffected result.
    function RaiseOutOfBoundsError: Double;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclDoubleCollection); overload;
    destructor Destroy; override;
    property Items: TDynDoubleArray read FItems;
    { IJclPackable }
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
    { IJclDoubleList }
    function Delete(Index: Integer): Double;
    function ExtractIndex(Index: Integer): Double;
    function GetValue(Index: Integer): Double;
    function IndexOf(const AValue: Double): Integer;
    function Insert(Index: Integer; const AValue: Double): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclDoubleCollection): Boolean;
    function LastIndexOf(const AValue: Double): Integer;
    procedure SetValue(Index: Integer; const AValue: Double);
    function SubList(First, Count: Integer): IJclDoubleList;
  end;

  TJclDoubleVectorIterator = class(TJclAbstractIterator, IJclDoubleIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: TJclDoubleVector;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
  public
    constructor Create(AOwnList: TJclDoubleVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
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

  TJclExtendedVector = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclExtendedContainer, IJclExtendedFlatContainer, IJclExtendedEqualityComparer,
    IJclExtendedCollection, IJclExtendedList, IJclExtendedArray)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FItems: TDynExtendedArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
    // complaining about possible unaffected result.
    function RaiseOutOfBoundsError: Extended;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclExtendedCollection); overload;
    destructor Destroy; override;
    property Items: TDynExtendedArray read FItems;
    { IJclPackable }
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
    { IJclExtendedList }
    function Delete(Index: Integer): Extended;
    function ExtractIndex(Index: Integer): Extended;
    function GetValue(Index: Integer): Extended;
    function IndexOf(const AValue: Extended): Integer;
    function Insert(Index: Integer; const AValue: Extended): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclExtendedCollection): Boolean;
    function LastIndexOf(const AValue: Extended): Integer;
    procedure SetValue(Index: Integer; const AValue: Extended);
    function SubList(First, Count: Integer): IJclExtendedList;
  end;

  TJclExtendedVectorIterator = class(TJclAbstractIterator, IJclExtendedIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: TJclExtendedVector;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
  public
    constructor Create(AOwnList: TJclExtendedVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
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
  TJclFloatVector = TJclSingleVector;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatVector = TJclDoubleVector;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatVector = TJclExtendedVector;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatVectorIterator = TJclSingleVectorIterator;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatVectorIterator = TJclDoubleVectorIterator;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatVectorIterator = TJclExtendedVectorIterator;
  {$ENDIF MATH_EXTENDED_PRECISION}

  TJclIntegerVector = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclIntegerContainer, IJclIntegerFlatContainer, IJclIntegerEqualityComparer,
    IJclIntegerCollection, IJclIntegerList, IJclIntegerArray)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FItems: TDynIntegerArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
    // complaining about possible unaffected result.
    function RaiseOutOfBoundsError: Integer;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclIntegerCollection); overload;
    destructor Destroy; override;
    property Items: TDynIntegerArray read FItems;
    { IJclPackable }
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
    { IJclIntegerList }
    function Delete(Index: Integer): Integer;
    function ExtractIndex(Index: Integer): Integer;
    function GetValue(Index: Integer): Integer;
    function IndexOf(AValue: Integer): Integer;
    function Insert(Index: Integer; AValue: Integer): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclIntegerCollection): Boolean;
    function LastIndexOf(AValue: Integer): Integer;
    procedure SetValue(Index: Integer; AValue: Integer);
    function SubList(First, Count: Integer): IJclIntegerList;
  end;

  TJclIntegerVectorIterator = class(TJclAbstractIterator, IJclIntegerIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: TJclIntegerVector;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
  public
    constructor Create(AOwnList: TJclIntegerVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
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

  TJclCardinalVector = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclCardinalContainer, IJclCardinalFlatContainer, IJclCardinalEqualityComparer,
    IJclCardinalCollection, IJclCardinalList, IJclCardinalArray)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FItems: TDynCardinalArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
    // complaining about possible unaffected result.
    function RaiseOutOfBoundsError: Cardinal;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclCardinalCollection); overload;
    destructor Destroy; override;
    property Items: TDynCardinalArray read FItems;
    { IJclPackable }
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
    { IJclCardinalList }
    function Delete(Index: Integer): Cardinal;
    function ExtractIndex(Index: Integer): Cardinal;
    function GetValue(Index: Integer): Cardinal;
    function IndexOf(AValue: Cardinal): Integer;
    function Insert(Index: Integer; AValue: Cardinal): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclCardinalCollection): Boolean;
    function LastIndexOf(AValue: Cardinal): Integer;
    procedure SetValue(Index: Integer; AValue: Cardinal);
    function SubList(First, Count: Integer): IJclCardinalList;
  end;

  TJclCardinalVectorIterator = class(TJclAbstractIterator, IJclCardinalIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: TJclCardinalVector;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
  public
    constructor Create(AOwnList: TJclCardinalVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
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

  TJclInt64Vector = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclInt64Container, IJclInt64FlatContainer, IJclInt64EqualityComparer,
    IJclInt64Collection, IJclInt64List, IJclInt64Array)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FItems: TDynInt64Array;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
    // complaining about possible unaffected result.
    function RaiseOutOfBoundsError: Int64;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclInt64Collection); overload;
    destructor Destroy; override;
    property Items: TDynInt64Array read FItems;
    { IJclPackable }
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
    { IJclInt64List }
    function Delete(Index: Integer): Int64;
    function ExtractIndex(Index: Integer): Int64;
    function GetValue(Index: Integer): Int64;
    function IndexOf(const AValue: Int64): Integer;
    function Insert(Index: Integer; const AValue: Int64): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclInt64Collection): Boolean;
    function LastIndexOf(const AValue: Int64): Integer;
    procedure SetValue(Index: Integer; const AValue: Int64);
    function SubList(First, Count: Integer): IJclInt64List;
  end;

  TJclInt64VectorIterator = class(TJclAbstractIterator, IJclInt64Iterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: TJclInt64Vector;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
  public
    constructor Create(AOwnList: TJclInt64Vector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
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

  TJclPtrVector = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclPtrContainer, IJclPtrFlatContainer, IJclPtrEqualityComparer,
    IJclPtrCollection, IJclPtrList, IJclPtrArray)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FItems: TDynPointerArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
    // complaining about possible unaffected result.
    function RaiseOutOfBoundsError: Pointer;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclPtrCollection); overload;
    destructor Destroy; override;
    property Items: TDynPointerArray read FItems;
    { IJclPackable }
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
    { IJclPtrList }
    function Delete(Index: Integer): Pointer;
    function ExtractIndex(Index: Integer): Pointer;
    function GetPointer(Index: Integer): Pointer;
    function IndexOf(APtr: Pointer): Integer;
    function Insert(Index: Integer; APtr: Pointer): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclPtrCollection): Boolean;
    function LastIndexOf(APtr: Pointer): Integer;
    procedure SetPointer(Index: Integer; APtr: Pointer);
    function SubList(First, Count: Integer): IJclPtrList;
  end;

  TJclPtrVectorIterator = class(TJclAbstractIterator, IJclPtrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: TJclPtrVector;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
  public
    constructor Create(AOwnList: TJclPtrVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
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

  TJclVector = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclContainer, IJclFlatContainer, IJclEqualityComparer, IJclObjectOwner,
    IJclCollection, IJclList, IJclArray)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FItems: TDynObjectArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
    // complaining about possible unaffected result.
    function RaiseOutOfBoundsError: TObject;
  public
    constructor Create(ACapacity: Integer; AOwnsObjects: Boolean); overload;
    constructor Create(const ACollection: IJclCollection; AOwnsObjects: Boolean); overload;
    destructor Destroy; override;
    property Items: TDynObjectArray read FItems;
    { IJclPackable }
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
    { IJclList }
    function Delete(Index: Integer): TObject;
    function ExtractIndex(Index: Integer): TObject;
    function GetObject(Index: Integer): TObject;
    function IndexOf(AObject: TObject): Integer;
    function Insert(Index: Integer; AObject: TObject): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean;
    function LastIndexOf(AObject: TObject): Integer;
    procedure SetObject(Index: Integer; AObject: TObject);
    function SubList(First, Count: Integer): IJclList;
  end;

  TJclVectorIterator = class(TJclAbstractIterator, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: TJclVector;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
  public
    constructor Create(AOwnList: TJclVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
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

  TJclVectorIterator<T> = class;

  TJclVector<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclContainer<T>, IJclFlatContainer<T>, IJclEqualityComparer<T>, IJclItemOwner<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>)
  protected
    type
      TDynArray = array of T;
      TVectorIterator = TJclVectorIterator<T>;
    procedure MoveArray(var List: TDynArray; FromIndex, ToIndex, Count: SizeInt);
  private
    FItems: TDynArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
    // complaining about possible unaffected result.
    function RaiseOutOfBoundsError: T;
  public
    constructor Create(ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;
    destructor Destroy; override;
    property Items: TDynArray read FItems;
    { IJclPackable }
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
    { IJclList<T> }
    function Delete(Index: Integer): T;
    function ExtractIndex(Index: Integer): T;
    function GetItem(Index: Integer): T;
    function IndexOf(const AItem: T): Integer;
    function Insert(Index: Integer; const AItem: T): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
    function LastIndexOf(const AItem: T): Integer;
    procedure SetItem(Index: Integer; const AItem: T);
    function SubList(First, Count: Integer): IJclList<T>;
  end;

  TJclVectorIterator<T> = class(TJclAbstractIterator, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: IJclList<T>;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
  public
    constructor Create(AOwnList: IJclList<T>; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
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

  // E = External helper to compare items for equality (GetHashCode is not used)
  TJclVectorE<T> = class(TJclVector<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer, IJclContainer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclItemOwner<T>)
  private
    FEqualityComparer: IJclEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityComparer: IJclEqualityComparer<T>; ACapacity: Integer; AOwnsItems: Boolean);
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    property EqualityComparer: IJclEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclVectorF<T> = class(TJclVector<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer, IJclContainer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
  end;

  // I = Items can compare themselves to an other for equality
  TJclVectorI<T: IEquatable<T>> = class(TJclVector<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer, IJclContainer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclItemOwner<T>)
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
  {$HPPEMIT ' #define JclVectors_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT '#endif'}

  {$HPPEMIT END '#ifdef JclVectors_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #define MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #undef JclVectors_MANAGED_INTERFACE_OPERATORS'}
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

//=== { TJclIntfVector } ======================================================

constructor TJclIntfVector.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

constructor TJclIntfVector.Create(const ACollection: IJclIntfCollection);
begin
  inherited Create();
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  SetCapacity(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclIntfVector.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntfVector.Add(const AInterface: IInterface): Boolean;
var
  Index: Integer;
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
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AInterface, FItems[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AInterface;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.AddAll(const ACollection: IJclIntfCollection): Boolean;
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

procedure TJclIntfVector.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntfVector;
  ACollection: IJclIntfCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfVector then
  begin
    ADest := TJclIntfVector(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclIntfCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclIntfVector.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeObject(FItems[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.CollectionEquals(const ACollection: IJclIntfCollection): Boolean;
var
  I: Integer;
  It: IJclIntfIterator;
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
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FItems[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.Contains(const AInterface: IInterface): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AInterface) then
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

function TJclIntfVector.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfVector.Delete(Index: Integer): IInterface;
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
    Extracted := ExtractIndex(Index);
    Result := FreeObject(Extracted);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.Extract(const AInterface: IInterface): Boolean;
var
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AInterface) then
      begin
        if I < (FSize - 1) then
          MoveArray(FItems, I + 1, I, FSize - 1 - I)
        else
          FItems[I] := nil;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.ExtractAll(const ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfVector.ExtractIndex(Index: Integer): IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FItems[Index];
      if Index < (FSize - 1) then
        MoveArray(FItems, Index + 1, Index, FSize - 1 - Index)
      else
        FItems[Index] := nil;
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.First: IJclIntfIterator;
begin
  Result := TJclIntfVectorIterator.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfVector.GetEnumerator: IJclIntfIterator;
begin
  Result := TJclIntfVectorIterator.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntfVector.GetObject(Index: Integer): IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) or (Index < FSize) then
      Result := FItems[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.IndexOf(const AInterface: IInterface): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AInterface) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.Insert(Index: Integer; const AInterface: IInterface): Boolean;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AInterface, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AInterface;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfVector.Last: IJclIntfIterator;
begin
  Result := TJclIntfVectorIterator.Create(Self, FSize - 1, False, isLast);
end;

function TJclIntfVector.LastIndexOf(const AInterface: IInterface): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AInterface) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.RaiseOutOfBoundsError: IInterface;
begin
  raise EJclOutOfBoundsError.Create;
end;

function TJclIntfVector.Remove(const AInterface: IInterface): Boolean;
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

function TJclIntfVector.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfVector.RetainAll(const ACollection: IJclIntfCollection): Boolean;
var
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
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FItems[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfVector.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FItems, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfVector.SetObject(Index: Integer; const AInterface: IInterface);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AInterface, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeObject(FItems[Index]);
        FItems[Index] := AInterface;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfVector.SubList(First, Count: Integer): IJclIntfList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclIntfList;
    for I := First to Last do
      Result.Add(FItems[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfVector.Create(FSize);
  AssignPropertiesTo(Result);
end;

//=== { TJclIntfVectorIterator } ===========================================================

constructor TJclIntfVectorIterator.Create(AOwnList: TJclIntfVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
end;

function TJclIntfVectorIterator.Add(const AInterface: IInterface): Boolean;
begin
  Result := FOwnList.Add(AInterface);
end;

procedure TJclIntfVectorIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclIntfVectorIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclIntfVectorIterator then
  begin
    ADest := TJclIntfVectorIterator(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
    ADest.FStart := FStart;
  end;
end;

function TJclIntfVectorIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclIntfVectorIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclIntfVectorIterator.Extract;
begin
  CheckValid;
  Valid := False;
  FOwnList.ExtractIndex(FCursor);
end;

function TJclIntfVectorIterator.GetObject: IInterface;
begin
  CheckValid;
  Result := FOwnList.GetObject(FCursor);
end;

function TJclIntfVectorIterator.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TJclIntfVectorIterator.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TJclIntfVectorIterator.Insert(const AInterface: IInterface): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AInterface);
end;

function TJclIntfVectorIterator.IteratorEquals(const AIterator: IJclIntfIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclIntfVectorIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclIntfVectorIterator then
  begin
    ItrObj := TJclIntfVectorIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfVectorIterator.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntfVectorIterator.Next: IInterface;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetObject(FCursor);
end;

function TJclIntfVectorIterator.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TJclIntfVectorIterator.Previous: IInterface;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetObject(FCursor);
end;

function TJclIntfVectorIterator.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TJclIntfVectorIterator.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TJclIntfVectorIterator.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TJclIntfVectorIterator.SetObject(const AInterface: IInterface);
begin
  CheckValid;
  FOwnList.SetObject(FCursor, AInterface);
end;

//=== { TJclAnsiStrVector } ======================================================

constructor TJclAnsiStrVector.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

constructor TJclAnsiStrVector.Create(const ACollection: IJclAnsiStrCollection);
begin
  inherited Create();
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  SetCapacity(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclAnsiStrVector.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclAnsiStrVector.Add(const AString: AnsiString): Boolean;
var
  Index: Integer;
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
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AString, FItems[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AString;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrVector.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

procedure TJclAnsiStrVector.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclAnsiStrVector;
  ACollection: IJclAnsiStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrVector then
  begin
    ADest := TJclAnsiStrVector(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclAnsiStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclAnsiStrVector.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeString(FItems[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrVector.CollectionEquals(const ACollection: IJclAnsiStrCollection): Boolean;
var
  I: Integer;
  It: IJclAnsiStrIterator;
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
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FItems[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrVector.Contains(const AString: AnsiString): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AString) then
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

function TJclAnsiStrVector.ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

function TJclAnsiStrVector.Delete(Index: Integer): AnsiString;
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
    Extracted := ExtractIndex(Index);
    Result := FreeString(Extracted);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrVector.Extract(const AString: AnsiString): Boolean;
var
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AString) then
      begin
        if I < (FSize - 1) then
          MoveArray(FItems, I + 1, I, FSize - 1 - I)
        else
          FItems[I] := '';
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrVector.ExtractAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

function TJclAnsiStrVector.ExtractIndex(Index: Integer): AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FItems[Index];
      if Index < (FSize - 1) then
        MoveArray(FItems, Index + 1, Index, FSize - 1 - Index)
      else
        FItems[Index] := '';
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrVector.First: IJclAnsiStrIterator;
begin
  Result := TJclAnsiStrVectorIterator.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrVector.GetEnumerator: IJclAnsiStrIterator;
begin
  Result := TJclAnsiStrVectorIterator.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclAnsiStrVector.GetString(Index: Integer): AnsiString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) or (Index < FSize) then
      Result := FItems[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrVector.IndexOf(const AString: AnsiString): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AString) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrVector.Insert(Index: Integer; const AString: AnsiString): Boolean;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AString, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AString;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrVector.InsertAll(Index: Integer; const ACollection: IJclAnsiStrCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrVector.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrVector.Last: IJclAnsiStrIterator;
begin
  Result := TJclAnsiStrVectorIterator.Create(Self, FSize - 1, False, isLast);
end;

function TJclAnsiStrVector.LastIndexOf(const AString: AnsiString): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AString) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrVector.RaiseOutOfBoundsError: AnsiString;
begin
  raise EJclOutOfBoundsError.Create;
end;

function TJclAnsiStrVector.Remove(const AString: AnsiString): Boolean;
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

function TJclAnsiStrVector.RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

function TJclAnsiStrVector.RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
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
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FItems[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrVector.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FItems, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrVector.SetString(Index: Integer; const AString: AnsiString);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AString, '');
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AString, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeString(FItems[Index]);
        FItems[Index] := AString;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrVector.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrVector.SubList(First, Count: Integer): IJclAnsiStrList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclAnsiStrList;
    for I := First to Last do
      Result.Add(FItems[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrVector.Create(FSize);
  AssignPropertiesTo(Result);
end;

//=== { TJclAnsiStrVectorIterator } ===========================================================

constructor TJclAnsiStrVectorIterator.Create(AOwnList: TJclAnsiStrVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
end;

function TJclAnsiStrVectorIterator.Add(const AString: AnsiString): Boolean;
begin
  Result := FOwnList.Add(AString);
end;

procedure TJclAnsiStrVectorIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclAnsiStrVectorIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclAnsiStrVectorIterator then
  begin
    ADest := TJclAnsiStrVectorIterator(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
    ADest.FStart := FStart;
  end;
end;

function TJclAnsiStrVectorIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclAnsiStrVectorIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclAnsiStrVectorIterator.Extract;
begin
  CheckValid;
  Valid := False;
  FOwnList.ExtractIndex(FCursor);
end;

function TJclAnsiStrVectorIterator.GetString: AnsiString;
begin
  CheckValid;
  Result := FOwnList.GetString(FCursor);
end;

function TJclAnsiStrVectorIterator.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TJclAnsiStrVectorIterator.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TJclAnsiStrVectorIterator.Insert(const AString: AnsiString): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AString);
end;

function TJclAnsiStrVectorIterator.IteratorEquals(const AIterator: IJclAnsiStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclAnsiStrVectorIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclAnsiStrVectorIterator then
  begin
    ItrObj := TJclAnsiStrVectorIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrVectorIterator.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclAnsiStrVectorIterator.Next: AnsiString;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetString(FCursor);
end;

function TJclAnsiStrVectorIterator.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TJclAnsiStrVectorIterator.Previous: AnsiString;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetString(FCursor);
end;

function TJclAnsiStrVectorIterator.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TJclAnsiStrVectorIterator.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TJclAnsiStrVectorIterator.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TJclAnsiStrVectorIterator.SetString(const AString: AnsiString);
begin
  CheckValid;
  FOwnList.SetString(FCursor, AString);
end;

//=== { TJclWideStrVector } ======================================================

constructor TJclWideStrVector.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

constructor TJclWideStrVector.Create(const ACollection: IJclWideStrCollection);
begin
  inherited Create();
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  SetCapacity(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclWideStrVector.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclWideStrVector.Add(const AString: WideString): Boolean;
var
  Index: Integer;
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
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AString, FItems[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AString;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrVector.AddAll(const ACollection: IJclWideStrCollection): Boolean;
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

procedure TJclWideStrVector.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclWideStrVector;
  ACollection: IJclWideStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrVector then
  begin
    ADest := TJclWideStrVector(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclWideStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclWideStrVector.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeString(FItems[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrVector.CollectionEquals(const ACollection: IJclWideStrCollection): Boolean;
var
  I: Integer;
  It: IJclWideStrIterator;
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
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FItems[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrVector.Contains(const AString: WideString): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AString) then
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

function TJclWideStrVector.ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
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

function TJclWideStrVector.Delete(Index: Integer): WideString;
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
    Extracted := ExtractIndex(Index);
    Result := FreeString(Extracted);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrVector.Extract(const AString: WideString): Boolean;
var
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AString) then
      begin
        if I < (FSize - 1) then
          MoveArray(FItems, I + 1, I, FSize - 1 - I)
        else
          FItems[I] := '';
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrVector.ExtractAll(const ACollection: IJclWideStrCollection): Boolean;
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

function TJclWideStrVector.ExtractIndex(Index: Integer): WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FItems[Index];
      if Index < (FSize - 1) then
        MoveArray(FItems, Index + 1, Index, FSize - 1 - Index)
      else
        FItems[Index] := '';
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrVector.First: IJclWideStrIterator;
begin
  Result := TJclWideStrVectorIterator.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrVector.GetEnumerator: IJclWideStrIterator;
begin
  Result := TJclWideStrVectorIterator.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclWideStrVector.GetString(Index: Integer): WideString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) or (Index < FSize) then
      Result := FItems[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrVector.IndexOf(const AString: WideString): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AString) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrVector.Insert(Index: Integer; const AString: WideString): Boolean;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AString, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AString;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrVector.InsertAll(Index: Integer; const ACollection: IJclWideStrCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrVector.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrVector.Last: IJclWideStrIterator;
begin
  Result := TJclWideStrVectorIterator.Create(Self, FSize - 1, False, isLast);
end;

function TJclWideStrVector.LastIndexOf(const AString: WideString): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AString) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrVector.RaiseOutOfBoundsError: WideString;
begin
  raise EJclOutOfBoundsError.Create;
end;

function TJclWideStrVector.Remove(const AString: WideString): Boolean;
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

function TJclWideStrVector.RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
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

function TJclWideStrVector.RetainAll(const ACollection: IJclWideStrCollection): Boolean;
var
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
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FItems[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrVector.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FItems, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrVector.SetString(Index: Integer; const AString: WideString);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AString, '');
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AString, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeString(FItems[Index]);
        FItems[Index] := AString;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrVector.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrVector.SubList(First, Count: Integer): IJclWideStrList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclWideStrList;
    for I := First to Last do
      Result.Add(FItems[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrVector.Create(FSize);
  AssignPropertiesTo(Result);
end;

//=== { TJclWideStrVectorIterator } ===========================================================

constructor TJclWideStrVectorIterator.Create(AOwnList: TJclWideStrVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
end;

function TJclWideStrVectorIterator.Add(const AString: WideString): Boolean;
begin
  Result := FOwnList.Add(AString);
end;

procedure TJclWideStrVectorIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclWideStrVectorIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclWideStrVectorIterator then
  begin
    ADest := TJclWideStrVectorIterator(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
    ADest.FStart := FStart;
  end;
end;

function TJclWideStrVectorIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclWideStrVectorIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclWideStrVectorIterator.Extract;
begin
  CheckValid;
  Valid := False;
  FOwnList.ExtractIndex(FCursor);
end;

function TJclWideStrVectorIterator.GetString: WideString;
begin
  CheckValid;
  Result := FOwnList.GetString(FCursor);
end;

function TJclWideStrVectorIterator.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TJclWideStrVectorIterator.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TJclWideStrVectorIterator.Insert(const AString: WideString): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AString);
end;

function TJclWideStrVectorIterator.IteratorEquals(const AIterator: IJclWideStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclWideStrVectorIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclWideStrVectorIterator then
  begin
    ItrObj := TJclWideStrVectorIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrVectorIterator.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclWideStrVectorIterator.Next: WideString;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetString(FCursor);
end;

function TJclWideStrVectorIterator.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TJclWideStrVectorIterator.Previous: WideString;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetString(FCursor);
end;

function TJclWideStrVectorIterator.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TJclWideStrVectorIterator.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TJclWideStrVectorIterator.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TJclWideStrVectorIterator.SetString(const AString: WideString);
begin
  CheckValid;
  FOwnList.SetString(FCursor, AString);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrVector } ======================================================

constructor TJclUnicodeStrVector.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

constructor TJclUnicodeStrVector.Create(const ACollection: IJclUnicodeStrCollection);
begin
  inherited Create();
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  SetCapacity(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclUnicodeStrVector.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclUnicodeStrVector.Add(const AString: UnicodeString): Boolean;
var
  Index: Integer;
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
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AString, FItems[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AString;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrVector.AddAll(const ACollection: IJclUnicodeStrCollection): Boolean;
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

procedure TJclUnicodeStrVector.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclUnicodeStrVector;
  ACollection: IJclUnicodeStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclUnicodeStrVector then
  begin
    ADest := TJclUnicodeStrVector(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclUnicodeStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclUnicodeStrVector.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeString(FItems[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrVector.CollectionEquals(const ACollection: IJclUnicodeStrCollection): Boolean;
var
  I: Integer;
  It: IJclUnicodeStrIterator;
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
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FItems[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrVector.Contains(const AString: UnicodeString): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AString) then
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

function TJclUnicodeStrVector.ContainsAll(const ACollection: IJclUnicodeStrCollection): Boolean;
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

function TJclUnicodeStrVector.Delete(Index: Integer): UnicodeString;
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
    Extracted := ExtractIndex(Index);
    Result := FreeString(Extracted);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrVector.Extract(const AString: UnicodeString): Boolean;
var
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AString) then
      begin
        if I < (FSize - 1) then
          MoveArray(FItems, I + 1, I, FSize - 1 - I)
        else
          FItems[I] := '';
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrVector.ExtractAll(const ACollection: IJclUnicodeStrCollection): Boolean;
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

function TJclUnicodeStrVector.ExtractIndex(Index: Integer): UnicodeString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FItems[Index];
      if Index < (FSize - 1) then
        MoveArray(FItems, Index + 1, Index, FSize - 1 - Index)
      else
        FItems[Index] := '';
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrVector.First: IJclUnicodeStrIterator;
begin
  Result := TJclUnicodeStrVectorIterator.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclUnicodeStrVector.GetEnumerator: IJclUnicodeStrIterator;
begin
  Result := TJclUnicodeStrVectorIterator.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclUnicodeStrVector.GetString(Index: Integer): UnicodeString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) or (Index < FSize) then
      Result := FItems[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrVector.IndexOf(const AString: UnicodeString): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AString) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrVector.Insert(Index: Integer; const AString: UnicodeString): Boolean;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AString, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AString;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrVector.InsertAll(Index: Integer; const ACollection: IJclUnicodeStrCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrVector.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclUnicodeStrVector.Last: IJclUnicodeStrIterator;
begin
  Result := TJclUnicodeStrVectorIterator.Create(Self, FSize - 1, False, isLast);
end;

function TJclUnicodeStrVector.LastIndexOf(const AString: UnicodeString): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AString) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrVector.RaiseOutOfBoundsError: UnicodeString;
begin
  raise EJclOutOfBoundsError.Create;
end;

function TJclUnicodeStrVector.Remove(const AString: UnicodeString): Boolean;
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

function TJclUnicodeStrVector.RemoveAll(const ACollection: IJclUnicodeStrCollection): Boolean;
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

function TJclUnicodeStrVector.RetainAll(const ACollection: IJclUnicodeStrCollection): Boolean;
var
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
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FItems[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrVector.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FItems, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrVector.SetString(Index: Integer; const AString: UnicodeString);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AString, '');
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AString, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeString(FItems[Index]);
        FItems[Index] := AString;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrVector.Size: Integer;
begin
  Result := FSize;
end;

function TJclUnicodeStrVector.SubList(First, Count: Integer): IJclUnicodeStrList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclUnicodeStrList;
    for I := First to Last do
      Result.Add(FItems[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrVector.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrVectorIterator } ===========================================================

constructor TJclUnicodeStrVectorIterator.Create(AOwnList: TJclUnicodeStrVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
end;

function TJclUnicodeStrVectorIterator.Add(const AString: UnicodeString): Boolean;
begin
  Result := FOwnList.Add(AString);
end;

procedure TJclUnicodeStrVectorIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclUnicodeStrVectorIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclUnicodeStrVectorIterator then
  begin
    ADest := TJclUnicodeStrVectorIterator(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
    ADest.FStart := FStart;
  end;
end;

function TJclUnicodeStrVectorIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclUnicodeStrVectorIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclUnicodeStrVectorIterator.Extract;
begin
  CheckValid;
  Valid := False;
  FOwnList.ExtractIndex(FCursor);
end;

function TJclUnicodeStrVectorIterator.GetString: UnicodeString;
begin
  CheckValid;
  Result := FOwnList.GetString(FCursor);
end;

function TJclUnicodeStrVectorIterator.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TJclUnicodeStrVectorIterator.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TJclUnicodeStrVectorIterator.Insert(const AString: UnicodeString): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AString);
end;

function TJclUnicodeStrVectorIterator.IteratorEquals(const AIterator: IJclUnicodeStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclUnicodeStrVectorIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclUnicodeStrVectorIterator then
  begin
    ItrObj := TJclUnicodeStrVectorIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclUnicodeStrVectorIterator.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclUnicodeStrVectorIterator.Next: UnicodeString;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetString(FCursor);
end;

function TJclUnicodeStrVectorIterator.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TJclUnicodeStrVectorIterator.Previous: UnicodeString;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetString(FCursor);
end;

function TJclUnicodeStrVectorIterator.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TJclUnicodeStrVectorIterator.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TJclUnicodeStrVectorIterator.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TJclUnicodeStrVectorIterator.SetString(const AString: UnicodeString);
begin
  CheckValid;
  FOwnList.SetString(FCursor, AString);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

//=== { TJclSingleVector } ======================================================

constructor TJclSingleVector.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

constructor TJclSingleVector.Create(const ACollection: IJclSingleCollection);
begin
  inherited Create();
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  SetCapacity(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclSingleVector.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclSingleVector.Add(const AValue: Single): Boolean;
var
  Index: Integer;
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
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AValue;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleVector.AddAll(const ACollection: IJclSingleCollection): Boolean;
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

procedure TJclSingleVector.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclSingleVector;
  ACollection: IJclSingleCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSingleVector then
  begin
    ADest := TJclSingleVector(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclSingleCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclSingleVector.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeSingle(FItems[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleVector.CollectionEquals(const ACollection: IJclSingleCollection): Boolean;
var
  I: Integer;
  It: IJclSingleIterator;
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
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FItems[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleVector.Contains(const AValue: Single): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AValue) then
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

function TJclSingleVector.ContainsAll(const ACollection: IJclSingleCollection): Boolean;
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

function TJclSingleVector.Delete(Index: Integer): Single;
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
    Extracted := ExtractIndex(Index);
    Result := FreeSingle(Extracted);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleVector.Extract(const AValue: Single): Boolean;
var
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        if I < (FSize - 1) then
          MoveArray(FItems, I + 1, I, FSize - 1 - I)
        else
          FItems[I] := 0.0;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleVector.ExtractAll(const ACollection: IJclSingleCollection): Boolean;
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

function TJclSingleVector.ExtractIndex(Index: Integer): Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FItems[Index];
      if Index < (FSize - 1) then
        MoveArray(FItems, Index + 1, Index, FSize - 1 - Index)
      else
        FItems[Index] := 0.0;
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleVector.First: IJclSingleIterator;
begin
  Result := TJclSingleVectorIterator.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSingleVector.GetEnumerator: IJclSingleIterator;
begin
  Result := TJclSingleVectorIterator.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSingleVector.GetValue(Index: Integer): Single;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (Index >= 0) or (Index < FSize) then
      Result := FItems[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleVector.IndexOf(const AValue: Single): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleVector.Insert(Index: Integer; const AValue: Single): Boolean;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AValue;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleVector.InsertAll(Index: Integer; const ACollection: IJclSingleCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleVector.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleVector.Last: IJclSingleIterator;
begin
  Result := TJclSingleVectorIterator.Create(Self, FSize - 1, False, isLast);
end;

function TJclSingleVector.LastIndexOf(const AValue: Single): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleVector.RaiseOutOfBoundsError: Single;
begin
  raise EJclOutOfBoundsError.Create;
end;

function TJclSingleVector.Remove(const AValue: Single): Boolean;
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

function TJclSingleVector.RemoveAll(const ACollection: IJclSingleCollection): Boolean;
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

function TJclSingleVector.RetainAll(const ACollection: IJclSingleCollection): Boolean;
var
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
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FItems[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleVector.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FItems, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleVector.SetValue(Index: Integer; const AValue: Single);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeSingle(FItems[Index]);
        FItems[Index] := AValue;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleVector.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleVector.SubList(First, Count: Integer): IJclSingleList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclSingleList;
    for I := First to Last do
      Result.Add(FItems[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleVector.Create(FSize);
  AssignPropertiesTo(Result);
end;

//=== { TJclSingleVectorIterator } ===========================================================

constructor TJclSingleVectorIterator.Create(AOwnList: TJclSingleVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
end;

function TJclSingleVectorIterator.Add(const AValue: Single): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TJclSingleVectorIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclSingleVectorIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclSingleVectorIterator then
  begin
    ADest := TJclSingleVectorIterator(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
    ADest.FStart := FStart;
  end;
end;

function TJclSingleVectorIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclSingleVectorIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclSingleVectorIterator.Extract;
begin
  CheckValid;
  Valid := False;
  FOwnList.ExtractIndex(FCursor);
end;

function TJclSingleVectorIterator.GetValue: Single;
begin
  CheckValid;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclSingleVectorIterator.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TJclSingleVectorIterator.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TJclSingleVectorIterator.Insert(const AValue: Single): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AValue);
end;

function TJclSingleVectorIterator.IteratorEquals(const AIterator: IJclSingleIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclSingleVectorIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclSingleVectorIterator then
  begin
    ItrObj := TJclSingleVectorIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSingleVectorIterator.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSingleVectorIterator.Next: Single;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclSingleVectorIterator.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TJclSingleVectorIterator.Previous: Single;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclSingleVectorIterator.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TJclSingleVectorIterator.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TJclSingleVectorIterator.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TJclSingleVectorIterator.SetValue(const AValue: Single);
begin
  CheckValid;
  FOwnList.SetValue(FCursor, AValue);
end;

//=== { TJclDoubleVector } ======================================================

constructor TJclDoubleVector.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

constructor TJclDoubleVector.Create(const ACollection: IJclDoubleCollection);
begin
  inherited Create();
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  SetCapacity(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclDoubleVector.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclDoubleVector.Add(const AValue: Double): Boolean;
var
  Index: Integer;
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
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AValue;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleVector.AddAll(const ACollection: IJclDoubleCollection): Boolean;
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

procedure TJclDoubleVector.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclDoubleVector;
  ACollection: IJclDoubleCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclDoubleVector then
  begin
    ADest := TJclDoubleVector(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclDoubleCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclDoubleVector.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeDouble(FItems[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleVector.CollectionEquals(const ACollection: IJclDoubleCollection): Boolean;
var
  I: Integer;
  It: IJclDoubleIterator;
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
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FItems[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleVector.Contains(const AValue: Double): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AValue) then
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

function TJclDoubleVector.ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
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

function TJclDoubleVector.Delete(Index: Integer): Double;
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
    Extracted := ExtractIndex(Index);
    Result := FreeDouble(Extracted);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleVector.Extract(const AValue: Double): Boolean;
var
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        if I < (FSize - 1) then
          MoveArray(FItems, I + 1, I, FSize - 1 - I)
        else
          FItems[I] := 0.0;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleVector.ExtractAll(const ACollection: IJclDoubleCollection): Boolean;
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

function TJclDoubleVector.ExtractIndex(Index: Integer): Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FItems[Index];
      if Index < (FSize - 1) then
        MoveArray(FItems, Index + 1, Index, FSize - 1 - Index)
      else
        FItems[Index] := 0.0;
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleVector.First: IJclDoubleIterator;
begin
  Result := TJclDoubleVectorIterator.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclDoubleVector.GetEnumerator: IJclDoubleIterator;
begin
  Result := TJclDoubleVectorIterator.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclDoubleVector.GetValue(Index: Integer): Double;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (Index >= 0) or (Index < FSize) then
      Result := FItems[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleVector.IndexOf(const AValue: Double): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleVector.Insert(Index: Integer; const AValue: Double): Boolean;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AValue;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleVector.InsertAll(Index: Integer; const ACollection: IJclDoubleCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleVector.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleVector.Last: IJclDoubleIterator;
begin
  Result := TJclDoubleVectorIterator.Create(Self, FSize - 1, False, isLast);
end;

function TJclDoubleVector.LastIndexOf(const AValue: Double): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleVector.RaiseOutOfBoundsError: Double;
begin
  raise EJclOutOfBoundsError.Create;
end;

function TJclDoubleVector.Remove(const AValue: Double): Boolean;
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

function TJclDoubleVector.RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
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

function TJclDoubleVector.RetainAll(const ACollection: IJclDoubleCollection): Boolean;
var
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
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FItems[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleVector.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FItems, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleVector.SetValue(Index: Integer; const AValue: Double);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeDouble(FItems[Index]);
        FItems[Index] := AValue;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleVector.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleVector.SubList(First, Count: Integer): IJclDoubleList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclDoubleList;
    for I := First to Last do
      Result.Add(FItems[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleVector.Create(FSize);
  AssignPropertiesTo(Result);
end;

//=== { TJclDoubleVectorIterator } ===========================================================

constructor TJclDoubleVectorIterator.Create(AOwnList: TJclDoubleVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
end;

function TJclDoubleVectorIterator.Add(const AValue: Double): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TJclDoubleVectorIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclDoubleVectorIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclDoubleVectorIterator then
  begin
    ADest := TJclDoubleVectorIterator(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
    ADest.FStart := FStart;
  end;
end;

function TJclDoubleVectorIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclDoubleVectorIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclDoubleVectorIterator.Extract;
begin
  CheckValid;
  Valid := False;
  FOwnList.ExtractIndex(FCursor);
end;

function TJclDoubleVectorIterator.GetValue: Double;
begin
  CheckValid;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclDoubleVectorIterator.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TJclDoubleVectorIterator.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TJclDoubleVectorIterator.Insert(const AValue: Double): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AValue);
end;

function TJclDoubleVectorIterator.IteratorEquals(const AIterator: IJclDoubleIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclDoubleVectorIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclDoubleVectorIterator then
  begin
    ItrObj := TJclDoubleVectorIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclDoubleVectorIterator.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclDoubleVectorIterator.Next: Double;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclDoubleVectorIterator.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TJclDoubleVectorIterator.Previous: Double;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclDoubleVectorIterator.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TJclDoubleVectorIterator.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TJclDoubleVectorIterator.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TJclDoubleVectorIterator.SetValue(const AValue: Double);
begin
  CheckValid;
  FOwnList.SetValue(FCursor, AValue);
end;

//=== { TJclExtendedVector } ======================================================

constructor TJclExtendedVector.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

constructor TJclExtendedVector.Create(const ACollection: IJclExtendedCollection);
begin
  inherited Create();
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  SetCapacity(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclExtendedVector.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclExtendedVector.Add(const AValue: Extended): Boolean;
var
  Index: Integer;
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
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AValue;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedVector.AddAll(const ACollection: IJclExtendedCollection): Boolean;
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

procedure TJclExtendedVector.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclExtendedVector;
  ACollection: IJclExtendedCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclExtendedVector then
  begin
    ADest := TJclExtendedVector(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclExtendedCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclExtendedVector.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeExtended(FItems[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedVector.CollectionEquals(const ACollection: IJclExtendedCollection): Boolean;
var
  I: Integer;
  It: IJclExtendedIterator;
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
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FItems[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedVector.Contains(const AValue: Extended): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AValue) then
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

function TJclExtendedVector.ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
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

function TJclExtendedVector.Delete(Index: Integer): Extended;
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
    Extracted := ExtractIndex(Index);
    Result := FreeExtended(Extracted);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedVector.Extract(const AValue: Extended): Boolean;
var
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        if I < (FSize - 1) then
          MoveArray(FItems, I + 1, I, FSize - 1 - I)
        else
          FItems[I] := 0.0;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedVector.ExtractAll(const ACollection: IJclExtendedCollection): Boolean;
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

function TJclExtendedVector.ExtractIndex(Index: Integer): Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FItems[Index];
      if Index < (FSize - 1) then
        MoveArray(FItems, Index + 1, Index, FSize - 1 - Index)
      else
        FItems[Index] := 0.0;
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedVector.First: IJclExtendedIterator;
begin
  Result := TJclExtendedVectorIterator.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclExtendedVector.GetEnumerator: IJclExtendedIterator;
begin
  Result := TJclExtendedVectorIterator.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclExtendedVector.GetValue(Index: Integer): Extended;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (Index >= 0) or (Index < FSize) then
      Result := FItems[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedVector.IndexOf(const AValue: Extended): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedVector.Insert(Index: Integer; const AValue: Extended): Boolean;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AValue;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedVector.InsertAll(Index: Integer; const ACollection: IJclExtendedCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedVector.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedVector.Last: IJclExtendedIterator;
begin
  Result := TJclExtendedVectorIterator.Create(Self, FSize - 1, False, isLast);
end;

function TJclExtendedVector.LastIndexOf(const AValue: Extended): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedVector.RaiseOutOfBoundsError: Extended;
begin
  raise EJclOutOfBoundsError.Create;
end;

function TJclExtendedVector.Remove(const AValue: Extended): Boolean;
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

function TJclExtendedVector.RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
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

function TJclExtendedVector.RetainAll(const ACollection: IJclExtendedCollection): Boolean;
var
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
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FItems[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedVector.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FItems, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedVector.SetValue(Index: Integer; const AValue: Extended);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeExtended(FItems[Index]);
        FItems[Index] := AValue;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedVector.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedVector.SubList(First, Count: Integer): IJclExtendedList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclExtendedList;
    for I := First to Last do
      Result.Add(FItems[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedVector.Create(FSize);
  AssignPropertiesTo(Result);
end;

//=== { TJclExtendedVectorIterator } ===========================================================

constructor TJclExtendedVectorIterator.Create(AOwnList: TJclExtendedVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
end;

function TJclExtendedVectorIterator.Add(const AValue: Extended): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TJclExtendedVectorIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclExtendedVectorIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclExtendedVectorIterator then
  begin
    ADest := TJclExtendedVectorIterator(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
    ADest.FStart := FStart;
  end;
end;

function TJclExtendedVectorIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclExtendedVectorIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclExtendedVectorIterator.Extract;
begin
  CheckValid;
  Valid := False;
  FOwnList.ExtractIndex(FCursor);
end;

function TJclExtendedVectorIterator.GetValue: Extended;
begin
  CheckValid;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclExtendedVectorIterator.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TJclExtendedVectorIterator.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TJclExtendedVectorIterator.Insert(const AValue: Extended): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AValue);
end;

function TJclExtendedVectorIterator.IteratorEquals(const AIterator: IJclExtendedIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclExtendedVectorIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclExtendedVectorIterator then
  begin
    ItrObj := TJclExtendedVectorIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclExtendedVectorIterator.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclExtendedVectorIterator.Next: Extended;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclExtendedVectorIterator.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TJclExtendedVectorIterator.Previous: Extended;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclExtendedVectorIterator.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TJclExtendedVectorIterator.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TJclExtendedVectorIterator.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TJclExtendedVectorIterator.SetValue(const AValue: Extended);
begin
  CheckValid;
  FOwnList.SetValue(FCursor, AValue);
end;

//=== { TJclIntegerVector } ======================================================

constructor TJclIntegerVector.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

constructor TJclIntegerVector.Create(const ACollection: IJclIntegerCollection);
begin
  inherited Create();
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  SetCapacity(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclIntegerVector.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntegerVector.Add(AValue: Integer): Boolean;
var
  Index: Integer;
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
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AValue;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerVector.AddAll(const ACollection: IJclIntegerCollection): Boolean;
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

procedure TJclIntegerVector.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntegerVector;
  ACollection: IJclIntegerCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntegerVector then
  begin
    ADest := TJclIntegerVector(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclIntegerCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclIntegerVector.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeInteger(FItems[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerVector.CollectionEquals(const ACollection: IJclIntegerCollection): Boolean;
var
  I: Integer;
  It: IJclIntegerIterator;
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
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FItems[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerVector.Contains(AValue: Integer): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AValue) then
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

function TJclIntegerVector.ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
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

function TJclIntegerVector.Delete(Index: Integer): Integer;
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
    Extracted := ExtractIndex(Index);
    Result := FreeInteger(Extracted);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerVector.Extract(AValue: Integer): Boolean;
var
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        if I < (FSize - 1) then
          MoveArray(FItems, I + 1, I, FSize - 1 - I)
        else
          FItems[I] := 0;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerVector.ExtractAll(const ACollection: IJclIntegerCollection): Boolean;
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

function TJclIntegerVector.ExtractIndex(Index: Integer): Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FItems[Index];
      if Index < (FSize - 1) then
        MoveArray(FItems, Index + 1, Index, FSize - 1 - Index)
      else
        FItems[Index] := 0;
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerVector.First: IJclIntegerIterator;
begin
  Result := TJclIntegerVectorIterator.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntegerVector.GetEnumerator: IJclIntegerIterator;
begin
  Result := TJclIntegerVectorIterator.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntegerVector.GetValue(Index: Integer): Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (Index >= 0) or (Index < FSize) then
      Result := FItems[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerVector.IndexOf(AValue: Integer): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerVector.Insert(Index: Integer; AValue: Integer): Boolean;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AValue;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerVector.InsertAll(Index: Integer; const ACollection: IJclIntegerCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerVector.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerVector.Last: IJclIntegerIterator;
begin
  Result := TJclIntegerVectorIterator.Create(Self, FSize - 1, False, isLast);
end;

function TJclIntegerVector.LastIndexOf(AValue: Integer): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerVector.RaiseOutOfBoundsError: Integer;
begin
  raise EJclOutOfBoundsError.Create;
end;

function TJclIntegerVector.Remove(AValue: Integer): Boolean;
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

function TJclIntegerVector.RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
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

function TJclIntegerVector.RetainAll(const ACollection: IJclIntegerCollection): Boolean;
var
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
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FItems[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerVector.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FItems, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerVector.SetValue(Index: Integer; AValue: Integer);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeInteger(FItems[Index]);
        FItems[Index] := AValue;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerVector.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerVector.SubList(First, Count: Integer): IJclIntegerList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclIntegerList;
    for I := First to Last do
      Result.Add(FItems[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerVector.Create(FSize);
  AssignPropertiesTo(Result);
end;

//=== { TJclIntegerVectorIterator } ===========================================================

constructor TJclIntegerVectorIterator.Create(AOwnList: TJclIntegerVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
end;

function TJclIntegerVectorIterator.Add(AValue: Integer): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TJclIntegerVectorIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclIntegerVectorIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclIntegerVectorIterator then
  begin
    ADest := TJclIntegerVectorIterator(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
    ADest.FStart := FStart;
  end;
end;

function TJclIntegerVectorIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclIntegerVectorIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclIntegerVectorIterator.Extract;
begin
  CheckValid;
  Valid := False;
  FOwnList.ExtractIndex(FCursor);
end;

function TJclIntegerVectorIterator.GetValue: Integer;
begin
  CheckValid;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclIntegerVectorIterator.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TJclIntegerVectorIterator.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TJclIntegerVectorIterator.Insert(AValue: Integer): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AValue);
end;

function TJclIntegerVectorIterator.IteratorEquals(const AIterator: IJclIntegerIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclIntegerVectorIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclIntegerVectorIterator then
  begin
    ItrObj := TJclIntegerVectorIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntegerVectorIterator.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntegerVectorIterator.Next: Integer;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclIntegerVectorIterator.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TJclIntegerVectorIterator.Previous: Integer;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclIntegerVectorIterator.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TJclIntegerVectorIterator.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TJclIntegerVectorIterator.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TJclIntegerVectorIterator.SetValue(AValue: Integer);
begin
  CheckValid;
  FOwnList.SetValue(FCursor, AValue);
end;

//=== { TJclCardinalVector } ======================================================

constructor TJclCardinalVector.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

constructor TJclCardinalVector.Create(const ACollection: IJclCardinalCollection);
begin
  inherited Create();
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  SetCapacity(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclCardinalVector.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclCardinalVector.Add(AValue: Cardinal): Boolean;
var
  Index: Integer;
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
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AValue;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalVector.AddAll(const ACollection: IJclCardinalCollection): Boolean;
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

procedure TJclCardinalVector.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclCardinalVector;
  ACollection: IJclCardinalCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclCardinalVector then
  begin
    ADest := TJclCardinalVector(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclCardinalCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclCardinalVector.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeCardinal(FItems[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalVector.CollectionEquals(const ACollection: IJclCardinalCollection): Boolean;
var
  I: Integer;
  It: IJclCardinalIterator;
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
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FItems[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalVector.Contains(AValue: Cardinal): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AValue) then
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

function TJclCardinalVector.ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
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

function TJclCardinalVector.Delete(Index: Integer): Cardinal;
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
    Extracted := ExtractIndex(Index);
    Result := FreeCardinal(Extracted);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalVector.Extract(AValue: Cardinal): Boolean;
var
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        if I < (FSize - 1) then
          MoveArray(FItems, I + 1, I, FSize - 1 - I)
        else
          FItems[I] := 0;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalVector.ExtractAll(const ACollection: IJclCardinalCollection): Boolean;
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

function TJclCardinalVector.ExtractIndex(Index: Integer): Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FItems[Index];
      if Index < (FSize - 1) then
        MoveArray(FItems, Index + 1, Index, FSize - 1 - Index)
      else
        FItems[Index] := 0;
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalVector.First: IJclCardinalIterator;
begin
  Result := TJclCardinalVectorIterator.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclCardinalVector.GetEnumerator: IJclCardinalIterator;
begin
  Result := TJclCardinalVectorIterator.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclCardinalVector.GetValue(Index: Integer): Cardinal;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (Index >= 0) or (Index < FSize) then
      Result := FItems[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalVector.IndexOf(AValue: Cardinal): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalVector.Insert(Index: Integer; AValue: Cardinal): Boolean;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AValue;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalVector.InsertAll(Index: Integer; const ACollection: IJclCardinalCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalVector.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalVector.Last: IJclCardinalIterator;
begin
  Result := TJclCardinalVectorIterator.Create(Self, FSize - 1, False, isLast);
end;

function TJclCardinalVector.LastIndexOf(AValue: Cardinal): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalVector.RaiseOutOfBoundsError: Cardinal;
begin
  raise EJclOutOfBoundsError.Create;
end;

function TJclCardinalVector.Remove(AValue: Cardinal): Boolean;
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

function TJclCardinalVector.RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
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

function TJclCardinalVector.RetainAll(const ACollection: IJclCardinalCollection): Boolean;
var
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
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FItems[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalVector.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FItems, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalVector.SetValue(Index: Integer; AValue: Cardinal);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeCardinal(FItems[Index]);
        FItems[Index] := AValue;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalVector.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalVector.SubList(First, Count: Integer): IJclCardinalList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclCardinalList;
    for I := First to Last do
      Result.Add(FItems[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalVector.Create(FSize);
  AssignPropertiesTo(Result);
end;

//=== { TJclCardinalVectorIterator } ===========================================================

constructor TJclCardinalVectorIterator.Create(AOwnList: TJclCardinalVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
end;

function TJclCardinalVectorIterator.Add(AValue: Cardinal): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TJclCardinalVectorIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclCardinalVectorIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclCardinalVectorIterator then
  begin
    ADest := TJclCardinalVectorIterator(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
    ADest.FStart := FStart;
  end;
end;

function TJclCardinalVectorIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclCardinalVectorIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclCardinalVectorIterator.Extract;
begin
  CheckValid;
  Valid := False;
  FOwnList.ExtractIndex(FCursor);
end;

function TJclCardinalVectorIterator.GetValue: Cardinal;
begin
  CheckValid;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclCardinalVectorIterator.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TJclCardinalVectorIterator.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TJclCardinalVectorIterator.Insert(AValue: Cardinal): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AValue);
end;

function TJclCardinalVectorIterator.IteratorEquals(const AIterator: IJclCardinalIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclCardinalVectorIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclCardinalVectorIterator then
  begin
    ItrObj := TJclCardinalVectorIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclCardinalVectorIterator.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclCardinalVectorIterator.Next: Cardinal;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclCardinalVectorIterator.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TJclCardinalVectorIterator.Previous: Cardinal;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclCardinalVectorIterator.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TJclCardinalVectorIterator.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TJclCardinalVectorIterator.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TJclCardinalVectorIterator.SetValue(AValue: Cardinal);
begin
  CheckValid;
  FOwnList.SetValue(FCursor, AValue);
end;

//=== { TJclInt64Vector } ======================================================

constructor TJclInt64Vector.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

constructor TJclInt64Vector.Create(const ACollection: IJclInt64Collection);
begin
  inherited Create();
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  SetCapacity(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclInt64Vector.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclInt64Vector.Add(const AValue: Int64): Boolean;
var
  Index: Integer;
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
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AValue;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Vector.AddAll(const ACollection: IJclInt64Collection): Boolean;
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

procedure TJclInt64Vector.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclInt64Vector;
  ACollection: IJclInt64Collection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclInt64Vector then
  begin
    ADest := TJclInt64Vector(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclInt64Collection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclInt64Vector.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeInt64(FItems[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Vector.CollectionEquals(const ACollection: IJclInt64Collection): Boolean;
var
  I: Integer;
  It: IJclInt64Iterator;
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
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FItems[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Vector.Contains(const AValue: Int64): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AValue) then
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

function TJclInt64Vector.ContainsAll(const ACollection: IJclInt64Collection): Boolean;
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

function TJclInt64Vector.Delete(Index: Integer): Int64;
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
    Extracted := ExtractIndex(Index);
    Result := FreeInt64(Extracted);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Vector.Extract(const AValue: Int64): Boolean;
var
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        if I < (FSize - 1) then
          MoveArray(FItems, I + 1, I, FSize - 1 - I)
        else
          FItems[I] := 0;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Vector.ExtractAll(const ACollection: IJclInt64Collection): Boolean;
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

function TJclInt64Vector.ExtractIndex(Index: Integer): Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FItems[Index];
      if Index < (FSize - 1) then
        MoveArray(FItems, Index + 1, Index, FSize - 1 - Index)
      else
        FItems[Index] := 0;
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Vector.First: IJclInt64Iterator;
begin
  Result := TJclInt64VectorIterator.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclInt64Vector.GetEnumerator: IJclInt64Iterator;
begin
  Result := TJclInt64VectorIterator.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclInt64Vector.GetValue(Index: Integer): Int64;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (Index >= 0) or (Index < FSize) then
      Result := FItems[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Vector.IndexOf(const AValue: Int64): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Vector.Insert(Index: Integer; const AValue: Int64): Boolean;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AValue;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Vector.InsertAll(Index: Integer; const ACollection: IJclInt64Collection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Vector.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64Vector.Last: IJclInt64Iterator;
begin
  Result := TJclInt64VectorIterator.Create(Self, FSize - 1, False, isLast);
end;

function TJclInt64Vector.LastIndexOf(const AValue: Int64): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Vector.RaiseOutOfBoundsError: Int64;
begin
  raise EJclOutOfBoundsError.Create;
end;

function TJclInt64Vector.Remove(const AValue: Int64): Boolean;
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

function TJclInt64Vector.RemoveAll(const ACollection: IJclInt64Collection): Boolean;
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

function TJclInt64Vector.RetainAll(const ACollection: IJclInt64Collection): Boolean;
var
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
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FItems[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Vector.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FItems, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Vector.SetValue(Index: Integer; const AValue: Int64);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AValue, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeInt64(FItems[Index]);
        FItems[Index] := AValue;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Vector.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64Vector.SubList(First, Count: Integer): IJclInt64List;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclInt64List;
    for I := First to Last do
      Result.Add(FItems[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Vector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Vector.Create(FSize);
  AssignPropertiesTo(Result);
end;

//=== { TJclInt64VectorIterator } ===========================================================

constructor TJclInt64VectorIterator.Create(AOwnList: TJclInt64Vector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
end;

function TJclInt64VectorIterator.Add(const AValue: Int64): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TJclInt64VectorIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclInt64VectorIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclInt64VectorIterator then
  begin
    ADest := TJclInt64VectorIterator(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
    ADest.FStart := FStart;
  end;
end;

function TJclInt64VectorIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInt64VectorIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclInt64VectorIterator.Extract;
begin
  CheckValid;
  Valid := False;
  FOwnList.ExtractIndex(FCursor);
end;

function TJclInt64VectorIterator.GetValue: Int64;
begin
  CheckValid;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclInt64VectorIterator.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TJclInt64VectorIterator.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TJclInt64VectorIterator.Insert(const AValue: Int64): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AValue);
end;

function TJclInt64VectorIterator.IteratorEquals(const AIterator: IJclInt64Iterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclInt64VectorIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclInt64VectorIterator then
  begin
    ItrObj := TJclInt64VectorIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclInt64VectorIterator.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclInt64VectorIterator.Next: Int64;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclInt64VectorIterator.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TJclInt64VectorIterator.Previous: Int64;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TJclInt64VectorIterator.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TJclInt64VectorIterator.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TJclInt64VectorIterator.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TJclInt64VectorIterator.SetValue(const AValue: Int64);
begin
  CheckValid;
  FOwnList.SetValue(FCursor, AValue);
end;

//=== { TJclPtrVector } ======================================================

constructor TJclPtrVector.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

constructor TJclPtrVector.Create(const ACollection: IJclPtrCollection);
begin
  inherited Create();
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  SetCapacity(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclPtrVector.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclPtrVector.Add(APtr: Pointer): Boolean;
var
  Index: Integer;
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
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(APtr, FItems[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := APtr;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrVector.AddAll(const ACollection: IJclPtrCollection): Boolean;
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

procedure TJclPtrVector.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclPtrVector;
  ACollection: IJclPtrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclPtrVector then
  begin
    ADest := TJclPtrVector(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclPtrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclPtrVector.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreePointer(FItems[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrVector.CollectionEquals(const ACollection: IJclPtrCollection): Boolean;
var
  I: Integer;
  It: IJclPtrIterator;
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
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FItems[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrVector.Contains(APtr: Pointer): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], APtr) then
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

function TJclPtrVector.ContainsAll(const ACollection: IJclPtrCollection): Boolean;
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

function TJclPtrVector.Delete(Index: Integer): Pointer;
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
    Extracted := ExtractIndex(Index);
    Result := FreePointer(Extracted);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrVector.Extract(APtr: Pointer): Boolean;
var
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], APtr) then
      begin
        if I < (FSize - 1) then
          MoveArray(FItems, I + 1, I, FSize - 1 - I)
        else
          FItems[I] := nil;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrVector.ExtractAll(const ACollection: IJclPtrCollection): Boolean;
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

function TJclPtrVector.ExtractIndex(Index: Integer): Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FItems[Index];
      if Index < (FSize - 1) then
        MoveArray(FItems, Index + 1, Index, FSize - 1 - Index)
      else
        FItems[Index] := nil;
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrVector.First: IJclPtrIterator;
begin
  Result := TJclPtrVectorIterator.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclPtrVector.GetEnumerator: IJclPtrIterator;
begin
  Result := TJclPtrVectorIterator.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclPtrVector.GetPointer(Index: Integer): Pointer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) or (Index < FSize) then
      Result := FItems[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrVector.IndexOf(APtr: Pointer): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], APtr) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrVector.Insert(Index: Integer; APtr: Pointer): Boolean;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(APtr, nil);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(APtr, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := APtr;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrVector.InsertAll(Index: Integer; const ACollection: IJclPtrCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrVector.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrVector.Last: IJclPtrIterator;
begin
  Result := TJclPtrVectorIterator.Create(Self, FSize - 1, False, isLast);
end;

function TJclPtrVector.LastIndexOf(APtr: Pointer): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], APtr) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrVector.RaiseOutOfBoundsError: Pointer;
begin
  raise EJclOutOfBoundsError.Create;
end;

function TJclPtrVector.Remove(APtr: Pointer): Boolean;
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

function TJclPtrVector.RemoveAll(const ACollection: IJclPtrCollection): Boolean;
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

function TJclPtrVector.RetainAll(const ACollection: IJclPtrCollection): Boolean;
var
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
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FItems[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrVector.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FItems, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrVector.SetPointer(Index: Integer; APtr: Pointer);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(APtr, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(APtr, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreePointer(FItems[Index]);
        FItems[Index] := APtr;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrVector.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrVector.SubList(First, Count: Integer): IJclPtrList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclPtrList;
    for I := First to Last do
      Result.Add(FItems[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrVector.Create(FSize);
  AssignPropertiesTo(Result);
end;

//=== { TJclPtrVectorIterator } ===========================================================

constructor TJclPtrVectorIterator.Create(AOwnList: TJclPtrVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
end;

function TJclPtrVectorIterator.Add(APtr: Pointer): Boolean;
begin
  Result := FOwnList.Add(APtr);
end;

procedure TJclPtrVectorIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclPtrVectorIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclPtrVectorIterator then
  begin
    ADest := TJclPtrVectorIterator(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
    ADest.FStart := FStart;
  end;
end;

function TJclPtrVectorIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPtrVectorIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclPtrVectorIterator.Extract;
begin
  CheckValid;
  Valid := False;
  FOwnList.ExtractIndex(FCursor);
end;

function TJclPtrVectorIterator.GetPointer: Pointer;
begin
  CheckValid;
  Result := FOwnList.GetPointer(FCursor);
end;

function TJclPtrVectorIterator.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TJclPtrVectorIterator.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TJclPtrVectorIterator.Insert(APtr: Pointer): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, APtr);
end;

function TJclPtrVectorIterator.IteratorEquals(const AIterator: IJclPtrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclPtrVectorIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclPtrVectorIterator then
  begin
    ItrObj := TJclPtrVectorIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclPtrVectorIterator.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclPtrVectorIterator.Next: Pointer;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetPointer(FCursor);
end;

function TJclPtrVectorIterator.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TJclPtrVectorIterator.Previous: Pointer;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetPointer(FCursor);
end;

function TJclPtrVectorIterator.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TJclPtrVectorIterator.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TJclPtrVectorIterator.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TJclPtrVectorIterator.SetPointer(APtr: Pointer);
begin
  CheckValid;
  FOwnList.SetPointer(FCursor, APtr);
end;

//=== { TJclVector } ======================================================

constructor TJclVector.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  SetCapacity(ACapacity);
end;

constructor TJclVector.Create(const ACollection: IJclCollection; AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  SetCapacity(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclVector.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclVector.Add(AObject: TObject): Boolean;
var
  Index: Integer;
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
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AObject, FItems[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AObject;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.AddAll(const ACollection: IJclCollection): Boolean;
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

procedure TJclVector.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclVector;
  ACollection: IJclCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclVector then
  begin
    ADest := TJclVector(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclVector.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeObject(FItems[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.CollectionEquals(const ACollection: IJclCollection): Boolean;
var
  I: Integer;
  It: IJclIterator;
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
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FItems[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.Contains(AObject: TObject): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AObject) then
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

function TJclVector.ContainsAll(const ACollection: IJclCollection): Boolean;
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

function TJclVector.Delete(Index: Integer): TObject;
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
    Extracted := ExtractIndex(Index);
    Result := FreeObject(Extracted);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.Extract(AObject: TObject): Boolean;
var
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AObject) then
      begin
        if I < (FSize - 1) then
          MoveArray(FItems, I + 1, I, FSize - 1 - I)
        else
          FItems[I] := nil;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.ExtractAll(const ACollection: IJclCollection): Boolean;
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

function TJclVector.ExtractIndex(Index: Integer): TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FItems[Index];
      if Index < (FSize - 1) then
        MoveArray(FItems, Index + 1, Index, FSize - 1 - Index)
      else
        FItems[Index] := nil;
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.First: IJclIterator;
begin
  Result := TJclVectorIterator.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclVector.GetEnumerator: IJclIterator;
begin
  Result := TJclVectorIterator.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclVector.GetObject(Index: Integer): TObject;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) or (Index < FSize) then
      Result := FItems[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.IndexOf(AObject: TObject): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AObject) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.Insert(Index: Integer; AObject: TObject): Boolean;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AObject, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AObject;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclVector.Last: IJclIterator;
begin
  Result := TJclVectorIterator.Create(Self, FSize - 1, False, isLast);
end;

function TJclVector.LastIndexOf(AObject: TObject): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AObject) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.RaiseOutOfBoundsError: TObject;
begin
  raise EJclOutOfBoundsError.Create;
end;

function TJclVector.Remove(AObject: TObject): Boolean;
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

function TJclVector.RemoveAll(const ACollection: IJclCollection): Boolean;
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

function TJclVector.RetainAll(const ACollection: IJclCollection): Boolean;
var
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
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FItems[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FItems, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector.SetObject(Index: Integer; AObject: TObject);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AObject, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeObject(FItems[Index]);
        FItems[Index] := AObject;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.Size: Integer;
begin
  Result := FSize;
end;

function TJclVector.SubList(First, Count: Integer): IJclList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclList;
    for I := First to Last do
      Result.Add(FItems[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclVector.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclVectorIterator } ===========================================================

constructor TJclVectorIterator.Create(AOwnList: TJclVector; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
end;

function TJclVectorIterator.Add(AObject: TObject): Boolean;
begin
  Result := FOwnList.Add(AObject);
end;

procedure TJclVectorIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclVectorIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclVectorIterator then
  begin
    ADest := TJclVectorIterator(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
    ADest.FStart := FStart;
  end;
end;

function TJclVectorIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclVectorIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclVectorIterator.Extract;
begin
  CheckValid;
  Valid := False;
  FOwnList.ExtractIndex(FCursor);
end;

function TJclVectorIterator.GetObject: TObject;
begin
  CheckValid;
  Result := FOwnList.GetObject(FCursor);
end;

function TJclVectorIterator.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TJclVectorIterator.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TJclVectorIterator.Insert(AObject: TObject): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AObject);
end;

function TJclVectorIterator.IteratorEquals(const AIterator: IJclIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclVectorIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclVectorIterator then
  begin
    ItrObj := TJclVectorIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclVectorIterator.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclVectorIterator.Next: TObject;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetObject(FCursor);
end;

function TJclVectorIterator.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TJclVectorIterator.Previous: TObject;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetObject(FCursor);
end;

function TJclVectorIterator.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TJclVectorIterator.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TJclVectorIterator.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TJclVectorIterator.SetObject(AObject: TObject);
begin
  CheckValid;
  FOwnList.SetObject(FCursor, AObject);
end;


{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

//=== { TJclVector<T> } ======================================================

constructor TJclVector<T>.Create(ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  SetCapacity(ACapacity);
end;

constructor TJclVector<T>.Create(const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  SetCapacity(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclVector<T>.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclVector<T>.Add(const AItem: T): Boolean;
var
  Index: Integer;
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
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AItem, FItems[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AItem;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
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

procedure TJclVector<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclVector<T>;
  ACollection: IJclCollection<T>;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclVector<T> then
  begin
    ADest := TJclVector<T>(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclCollection<T>, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclVector<T>.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeItem(FItems[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.CollectionEquals(const ACollection: IJclCollection<T>): Boolean;
var
  I: Integer;
  It: IJclIterator<T>;
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
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FItems[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.Contains(const AItem: T): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AItem) then
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

function TJclVector<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
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

function TJclVector<T>.Delete(Index: Integer): T;
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
    Extracted := ExtractIndex(Index);
    Result := FreeItem(Extracted);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.Extract(const AItem: T): Boolean;
var
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AItem) then
      begin
        if I < (FSize - 1) then
          MoveArray(FItems, I + 1, I, FSize - 1 - I)
        else
          FItems[I] := Default(T);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.ExtractAll(const ACollection: IJclCollection<T>): Boolean;
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

function TJclVector<T>.ExtractIndex(Index: Integer): T;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FItems[Index];
      if Index < (FSize - 1) then
        MoveArray(FItems, Index + 1, Index, FSize - 1 - Index)
      else
        FItems[Index] := Default(T);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.First: IJclIterator<T>;
begin
  Result := TVectorIterator.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclVector<T>.GetEnumerator: IJclIterator<T>;
begin
  Result := TVectorIterator.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclVector<T>.GetItem(Index: Integer): T;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if (Index >= 0) or (Index < FSize) then
      Result := FItems[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.IndexOf(const AItem: T): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FItems[I], AItem) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.Insert(Index: Integer; const AItem: T): Boolean;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AItem, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AItem;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclVector<T>.Last: IJclIterator<T>;
begin
  Result := TVectorIterator.Create(Self, FSize - 1, False, isLast);
end;

function TJclVector<T>.LastIndexOf(const AItem: T): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AItem) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.RaiseOutOfBoundsError: T;
begin
  raise EJclOutOfBoundsError.Create;
end;

function TJclVector<T>.Remove(const AItem: T): Boolean;
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

function TJclVector<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
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

function TJclVector<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
var
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
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FItems[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector<T>.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FItems, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector<T>.SetItem(Index: Integer; const AItem: T);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AItem, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeItem(FItems[Index]);
        FItems[Index] := AItem;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.Size: Integer;
begin
  Result := FSize;
end;

function TJclVector<T>.SubList(First, Count: Integer): IJclList<T>;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclList<T>;
    for I := First to Last do
      Result.Add(FItems[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector<T>.MoveArray(var List: TDynArray; FromIndex, ToIndex, Count: SizeInt);
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

//=== { TJclVectorIterator<T> } ===========================================================

constructor TJclVectorIterator<T>.Create(AOwnList: IJclList<T>; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
end;

function TJclVectorIterator<T>.Add(const AItem: T): Boolean;
begin
  Result := FOwnList.Add(AItem);
end;

procedure TJclVectorIterator<T>.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclVectorIterator<T>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclVectorIterator<T> then
  begin
    ADest := TJclVectorIterator<T>(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
    ADest.FStart := FStart;
  end;
end;

function TJclVectorIterator<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclVectorIterator<T>.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclVectorIterator<T>.Extract;
begin
  CheckValid;
  Valid := False;
  FOwnList.ExtractIndex(FCursor);
end;

function TJclVectorIterator<T>.GetItem: T;
begin
  CheckValid;
  Result := FOwnList.GetItem(FCursor);
end;

function TJclVectorIterator<T>.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TJclVectorIterator<T>.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TJclVectorIterator<T>.Insert(const AItem: T): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AItem);
end;

function TJclVectorIterator<T>.IteratorEquals(const AIterator: IJclIterator<T>): Boolean;
var
  Obj: TObject;
  ItrObj: TJclVectorIterator<T>;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclVectorIterator<T> then
  begin
    ItrObj := TJclVectorIterator<T>(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclVectorIterator<T>.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclVectorIterator<T>.Next: T;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetItem(FCursor);
end;

function TJclVectorIterator<T>.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TJclVectorIterator<T>.Previous: T;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetItem(FCursor);
end;

function TJclVectorIterator<T>.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TJclVectorIterator<T>.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TJclVectorIterator<T>.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TJclVectorIterator<T>.SetItem(const AItem: T);
begin
  CheckValid;
  FOwnList.SetItem(FCursor, AItem);
end;

//=== { TJclVectorE<T> } =====================================================

constructor TJclVectorE<T>.Create(const AEqualityComparer: IJclEqualityComparer<T>; ACapacity: Integer;
  AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclVectorE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclVectorE<T> then
    TJclVectorE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclVectorE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclVectorE<T>.Create(EqualityComparer, FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclVectorE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.ItemsEqual(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclVectorF<T> } =====================================================

constructor TJclVectorF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer;
  AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
end;

function TJclVectorF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclVectorF<T>.Create(EqualityCompare, FSize, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclVectorI<T> } =====================================================

function TJclVectorI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclVectorI<T>.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclVectorI<T>.ItemsEqual(const A, B: T): Boolean;
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

