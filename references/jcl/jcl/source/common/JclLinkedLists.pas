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


type
  TItrStart = (isFirst, isLast);

  TJclIntfLinkedListItem = class
  public
    Value: IInterface;
    Next: TJclIntfLinkedListItem;
    Previous: TJclIntfLinkedListItem;
  end;

  TJclIntfLinkedList = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclIntfContainer,
    IJclIntfFlatContainer, IJclIntfEqualityComparer,
    IJclIntfCollection, IJclIntfList)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FStart: TJclIntfLinkedListItem;
    FEnd: TJclIntfLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(const ACollection: IJclIntfCollection);
    destructor Destroy; override;
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

  TJclIntfLinkedListIterator = class(TJclAbstractIterator, IJclIntfIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclIntfLinkedListItem;
    FStart: TItrStart;
    FOwnList: TJclIntfLinkedList;
    FEqualityComparer: IJclIntfEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnList: TJclIntfLinkedList; ACursor: TJclIntfLinkedListItem; AValid: Boolean; AStart: TItrStart);
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

  TJclAnsiStrLinkedListItem = class
  public
    Value: AnsiString;
    Next: TJclAnsiStrLinkedListItem;
    Previous: TJclAnsiStrLinkedListItem;
  end;

  TJclAnsiStrLinkedList = class(TJclAnsiStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclAnsiStrContainer,
    IJclAnsiStrFlatContainer, IJclAnsiStrEqualityComparer, IJclStrBaseContainer,
    IJclAnsiStrCollection, IJclAnsiStrList)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FStart: TJclAnsiStrLinkedListItem;
    FEnd: TJclAnsiStrLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(const ACollection: IJclAnsiStrCollection);
    destructor Destroy; override;
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

  TJclAnsiStrLinkedListIterator = class(TJclAbstractIterator, IJclAnsiStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclAnsiStrLinkedListItem;
    FStart: TItrStart;
    FOwnList: TJclAnsiStrLinkedList;
    FEqualityComparer: IJclAnsiStrEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnList: TJclAnsiStrLinkedList; ACursor: TJclAnsiStrLinkedListItem; AValid: Boolean; AStart: TItrStart);
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

  TJclWideStrLinkedListItem = class
  public
    Value: WideString;
    Next: TJclWideStrLinkedListItem;
    Previous: TJclWideStrLinkedListItem;
  end;

  TJclWideStrLinkedList = class(TJclWideStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclWideStrContainer,
    IJclWideStrFlatContainer, IJclWideStrEqualityComparer, IJclStrBaseContainer,
    IJclWideStrCollection, IJclWideStrList)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FStart: TJclWideStrLinkedListItem;
    FEnd: TJclWideStrLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(const ACollection: IJclWideStrCollection);
    destructor Destroy; override;
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

  TJclWideStrLinkedListIterator = class(TJclAbstractIterator, IJclWideStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclWideStrLinkedListItem;
    FStart: TItrStart;
    FOwnList: TJclWideStrLinkedList;
    FEqualityComparer: IJclWideStrEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnList: TJclWideStrLinkedList; ACursor: TJclWideStrLinkedListItem; AValid: Boolean; AStart: TItrStart);
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
  TJclUnicodeStrLinkedListItem = class
  public
    Value: UnicodeString;
    Next: TJclUnicodeStrLinkedListItem;
    Previous: TJclUnicodeStrLinkedListItem;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrLinkedList = class(TJclUnicodeStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclUnicodeStrContainer,
    IJclUnicodeStrFlatContainer, IJclUnicodeStrEqualityComparer, IJclStrBaseContainer,
    IJclUnicodeStrCollection, IJclUnicodeStrList)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FStart: TJclUnicodeStrLinkedListItem;
    FEnd: TJclUnicodeStrLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(const ACollection: IJclUnicodeStrCollection);
    destructor Destroy; override;
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
  TJclUnicodeStrLinkedListIterator = class(TJclAbstractIterator, IJclUnicodeStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclUnicodeStrLinkedListItem;
    FStart: TItrStart;
    FOwnList: TJclUnicodeStrLinkedList;
    FEqualityComparer: IJclUnicodeStrEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnList: TJclUnicodeStrLinkedList; ACursor: TJclUnicodeStrLinkedListItem; AValid: Boolean; AStart: TItrStart);
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
  TJclStrLinkedListItem = TJclAnsiStrLinkedListItem;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrLinkedListItem = TJclWideStrLinkedListItem;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrLinkedListItem = TJclUnicodeStrLinkedListItem;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrLinkedList = TJclAnsiStrLinkedList;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrLinkedList = TJclWideStrLinkedList;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrLinkedList = TJclUnicodeStrLinkedList;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrLinkedListIterator = TJclAnsiStrLinkedListIterator;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrLinkedListIterator = TJclWideStrLinkedListIterator;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrLinkedListIterator = TJclUnicodeStrLinkedListIterator;
  {$ENDIF CONTAINER_UNICODESTR}

  TJclSingleLinkedListItem = class
  public
    Value: Single;
    Next: TJclSingleLinkedListItem;
    Previous: TJclSingleLinkedListItem;
  end;

  TJclSingleLinkedList = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclSingleContainer,
    IJclSingleFlatContainer, IJclSingleEqualityComparer,
    IJclSingleCollection, IJclSingleList)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FStart: TJclSingleLinkedListItem;
    FEnd: TJclSingleLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(const ACollection: IJclSingleCollection);
    destructor Destroy; override;
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

  TJclSingleLinkedListIterator = class(TJclAbstractIterator, IJclSingleIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclSingleLinkedListItem;
    FStart: TItrStart;
    FOwnList: TJclSingleLinkedList;
    FEqualityComparer: IJclSingleEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnList: TJclSingleLinkedList; ACursor: TJclSingleLinkedListItem; AValid: Boolean; AStart: TItrStart);
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

  TJclDoubleLinkedListItem = class
  public
    Value: Double;
    Next: TJclDoubleLinkedListItem;
    Previous: TJclDoubleLinkedListItem;
  end;

  TJclDoubleLinkedList = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclDoubleContainer,
    IJclDoubleFlatContainer, IJclDoubleEqualityComparer,
    IJclDoubleCollection, IJclDoubleList)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FStart: TJclDoubleLinkedListItem;
    FEnd: TJclDoubleLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(const ACollection: IJclDoubleCollection);
    destructor Destroy; override;
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

  TJclDoubleLinkedListIterator = class(TJclAbstractIterator, IJclDoubleIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclDoubleLinkedListItem;
    FStart: TItrStart;
    FOwnList: TJclDoubleLinkedList;
    FEqualityComparer: IJclDoubleEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnList: TJclDoubleLinkedList; ACursor: TJclDoubleLinkedListItem; AValid: Boolean; AStart: TItrStart);
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

  TJclExtendedLinkedListItem = class
  public
    Value: Extended;
    Next: TJclExtendedLinkedListItem;
    Previous: TJclExtendedLinkedListItem;
  end;

  TJclExtendedLinkedList = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclExtendedContainer,
    IJclExtendedFlatContainer, IJclExtendedEqualityComparer,
    IJclExtendedCollection, IJclExtendedList)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FStart: TJclExtendedLinkedListItem;
    FEnd: TJclExtendedLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(const ACollection: IJclExtendedCollection);
    destructor Destroy; override;
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

  TJclExtendedLinkedListIterator = class(TJclAbstractIterator, IJclExtendedIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclExtendedLinkedListItem;
    FStart: TItrStart;
    FOwnList: TJclExtendedLinkedList;
    FEqualityComparer: IJclExtendedEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnList: TJclExtendedLinkedList; ACursor: TJclExtendedLinkedListItem; AValid: Boolean; AStart: TItrStart);
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
  TJclFloatLinkedListItem = TJclSingleLinkedListItem;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatLinkedListItem = TJclDoubleLinkedListItem;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatLinkedListItem = TJclExtendedLinkedListItem;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatLinkedList = TJclSingleLinkedList;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatLinkedList = TJclDoubleLinkedList;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatLinkedList = TJclExtendedLinkedList;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatLinkedListIterator = TJclSingleLinkedListIterator;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatLinkedListIterator = TJclDoubleLinkedListIterator;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatLinkedListIterator = TJclExtendedLinkedListIterator;
  {$ENDIF MATH_EXTENDED_PRECISION}

  TJclIntegerLinkedListItem = class
  public
    Value: Integer;
    Next: TJclIntegerLinkedListItem;
    Previous: TJclIntegerLinkedListItem;
  end;

  TJclIntegerLinkedList = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclIntegerContainer,
    IJclIntegerFlatContainer, IJclIntegerEqualityComparer,
    IJclIntegerCollection, IJclIntegerList)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FStart: TJclIntegerLinkedListItem;
    FEnd: TJclIntegerLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(const ACollection: IJclIntegerCollection);
    destructor Destroy; override;
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

  TJclIntegerLinkedListIterator = class(TJclAbstractIterator, IJclIntegerIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclIntegerLinkedListItem;
    FStart: TItrStart;
    FOwnList: TJclIntegerLinkedList;
    FEqualityComparer: IJclIntegerEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnList: TJclIntegerLinkedList; ACursor: TJclIntegerLinkedListItem; AValid: Boolean; AStart: TItrStart);
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

  TJclCardinalLinkedListItem = class
  public
    Value: Cardinal;
    Next: TJclCardinalLinkedListItem;
    Previous: TJclCardinalLinkedListItem;
  end;

  TJclCardinalLinkedList = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclCardinalContainer,
    IJclCardinalFlatContainer, IJclCardinalEqualityComparer,
    IJclCardinalCollection, IJclCardinalList)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FStart: TJclCardinalLinkedListItem;
    FEnd: TJclCardinalLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(const ACollection: IJclCardinalCollection);
    destructor Destroy; override;
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

  TJclCardinalLinkedListIterator = class(TJclAbstractIterator, IJclCardinalIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclCardinalLinkedListItem;
    FStart: TItrStart;
    FOwnList: TJclCardinalLinkedList;
    FEqualityComparer: IJclCardinalEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnList: TJclCardinalLinkedList; ACursor: TJclCardinalLinkedListItem; AValid: Boolean; AStart: TItrStart);
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

  TJclInt64LinkedListItem = class
  public
    Value: Int64;
    Next: TJclInt64LinkedListItem;
    Previous: TJclInt64LinkedListItem;
  end;

  TJclInt64LinkedList = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclInt64Container,
    IJclInt64FlatContainer, IJclInt64EqualityComparer,
    IJclInt64Collection, IJclInt64List)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FStart: TJclInt64LinkedListItem;
    FEnd: TJclInt64LinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(const ACollection: IJclInt64Collection);
    destructor Destroy; override;
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

  TJclInt64LinkedListIterator = class(TJclAbstractIterator, IJclInt64Iterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclInt64LinkedListItem;
    FStart: TItrStart;
    FOwnList: TJclInt64LinkedList;
    FEqualityComparer: IJclInt64EqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnList: TJclInt64LinkedList; ACursor: TJclInt64LinkedListItem; AValid: Boolean; AStart: TItrStart);
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

  TJclPtrLinkedListItem = class
  public
    Value: Pointer;
    Next: TJclPtrLinkedListItem;
    Previous: TJclPtrLinkedListItem;
  end;

  TJclPtrLinkedList = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclPtrContainer,
    IJclPtrFlatContainer, IJclPtrEqualityComparer,
    IJclPtrCollection, IJclPtrList)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FStart: TJclPtrLinkedListItem;
    FEnd: TJclPtrLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(const ACollection: IJclPtrCollection);
    destructor Destroy; override;
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

  TJclPtrLinkedListIterator = class(TJclAbstractIterator, IJclPtrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclPtrLinkedListItem;
    FStart: TItrStart;
    FOwnList: TJclPtrLinkedList;
    FEqualityComparer: IJclPtrEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnList: TJclPtrLinkedList; ACursor: TJclPtrLinkedListItem; AValid: Boolean; AStart: TItrStart);
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

  TJclLinkedListItem = class
  public
    Value: TObject;
    Next: TJclLinkedListItem;
    Previous: TJclLinkedListItem;
  end;

  TJclLinkedList = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclContainer,
    IJclFlatContainer, IJclEqualityComparer, IJclObjectOwner,
    IJclCollection, IJclList)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FStart: TJclLinkedListItem;
    FEnd: TJclLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(const ACollection: IJclCollection; AOwnsObjects: Boolean);
    destructor Destroy; override;
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

  TJclLinkedListIterator = class(TJclAbstractIterator, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclLinkedListItem;
    FStart: TItrStart;
    FOwnList: TJclLinkedList;
    FEqualityComparer: IJclEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnList: TJclLinkedList; ACursor: TJclLinkedListItem; AValid: Boolean; AStart: TItrStart);
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

  TJclLinkedListItem<T> = class
  public
    Value: T;
    Next: TJclLinkedListItem<T>;
    Previous: TJclLinkedListItem<T>;
  end;

  TJclLinkedListIterator<T> = class;

  TJclLinkedList<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclBaseContainer, IJclContainer<T>,
    IJclFlatContainer<T>, IJclEqualityComparer<T>,IJclItemOwner<T>,
    IJclCollection<T>, IJclList<T>)
  protected
    type
      TLinkedListItem = TJclLinkedListItem<T>;
      TLinkedListIterator = TJclLinkedListIterator<T>;
  private
    FStart: TLinkedListItem;
    FEnd: TLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
    destructor Destroy; override;
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

  TJclLinkedListIterator<T> = class(TJclAbstractIterator, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclLinkedList<T>.TLinkedListItem;
    FStart: TItrStart;
    FOwnList: IJclList<T>;
    FEqualityComparer: IJclEqualityComparer<T>;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
  public
    constructor Create(AOwnList: IJclList<T>; ACursor: TJclLinkedList<T>.TLinkedListItem; AValid: Boolean; AStart: TItrStart);
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

//=== { TJclIntfLinkedList } ==================================================

constructor TJclIntfLinkedList.Create(const ACollection: IJclIntfCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclIntfLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntfLinkedList.Add(const AInterface: IInterface): Boolean;
var
  NewItem: TJclIntfLinkedListItem;
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
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AInterface, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclIntfLinkedListItem.Create;
        NewItem.Value := AInterface;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.AddAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  Item: IInterface;
  AddItem: Boolean;
  NewItem: TJclIntfLinkedListItem;
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
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclIntfLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclIntfCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclIntfCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclIntfLinkedList.Clear;
var
  Old, Current: TJclIntfLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeObject(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.CollectionEquals(const ACollection: IJclIntfCollection): Boolean;
var
  It, ItSelf: IJclIntfIterator;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.Contains(const AInterface: IInterface): Boolean;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AInterface) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
var
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
    Result := True;
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

function TJclIntfLinkedList.Delete(Index: Integer): IInterface;
var
  Current: TJclIntfLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeObject(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclIntfLinkedList.Extract(const AInterface: IInterface): Boolean;
var
  Current: TJclIntfLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AInterface) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Current.Value := nil;
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.ExtractAll(const ACollection: IJclIntfCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclIntfLinkedList.ExtractIndex(Index: Integer): IInterface;
var
  Current: TJclIntfLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := Current.Value;
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclIntfLinkedList.First: IJclIntfIterator;
begin
  Result := TJclIntfLinkedListIterator.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfLinkedList.GetEnumerator: IJclIntfIterator;
begin
  Result := TJclIntfLinkedListIterator.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntfLinkedList.GetObject(Index: Integer): IInterface;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.IndexOf(const AInterface: IInterface): Integer;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AInterface) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.Insert(Index: Integer; const AInterface: IInterface): Boolean;
var
  Current, NewItem: TJclIntfLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AInterface, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclIntfLinkedListItem.Create;
        NewItem.Value := AInterface;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
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

function TJclIntfLinkedList.InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  Current, NewItem, Test: TJclIntfLinkedListItem;
  AddItem: Boolean;
  Item: IInterface;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclIntfLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclIntfLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclIntfLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
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

function TJclIntfLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfLinkedList.Last: IJclIntfIterator;
begin
  Result := TJclIntfLinkedListIterator.Create(Self, FEnd, False, isLast);
end;

function TJclIntfLinkedList.LastIndexOf(const AInterface: IInterface): Integer;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AInterface) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.Remove(const AInterface: IInterface): Boolean;
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

function TJclIntfLinkedList.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclIntfLinkedList.RetainAll(const ACollection: IJclIntfCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfLinkedList.SetObject(Index: Integer; const AInterface: IInterface);
var
  Current: TJclIntfLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AInterface, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeObject(Current.Value);
            Current.Value := AInterface;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
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

function TJclIntfLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfLinkedList.SubList(First, Count: Integer): IJclIntfList;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclIntfList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

//=== { TJclIntfLinkedListIterator } ============================================================

constructor TJclIntfLinkedListIterator.Create(AOwnList: TJclIntfLinkedList; ACursor: TJclIntfLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclIntfEqualityComparer;
end;

function TJclIntfLinkedListIterator.Add(const AInterface: IInterface): Boolean;
begin
  Result := FOwnList.Add(AInterface);
end;

procedure TJclIntfLinkedListIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclIntfLinkedListIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclIntfLinkedListIterator then
  begin
    ADest := TJclIntfLinkedListIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclIntfLinkedListIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclIntfLinkedListIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclIntfLinkedListIterator.Extract;
var
  OldCursor: TJclIntfLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedListIterator.GetObject: IInterface;
begin
  CheckValid;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TJclIntfLinkedListIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedListIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedListIterator.Insert(const AInterface: IInterface): Boolean;
var
  NewCursor: TJclIntfLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AInterface, nil);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AInterface);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AInterface);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclIntfLinkedListItem.Create;
          NewCursor.Value := AInterface;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedListIterator.IteratorEquals(const AIterator: IJclIntfIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclIntfLinkedListIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclIntfLinkedListIterator then
  begin
    ItrObj := TJclIntfLinkedListIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfLinkedListIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntfLinkedListIterator.Next: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedListIterator.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntfLinkedListIterator.Previous: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedListIterator.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfLinkedListIterator.Remove;
var
  OldCursor: TJclIntfLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := nil;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfLinkedListIterator.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfLinkedListIterator.SetObject(const AInterface: IInterface);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := nil;
    FCursor.Value := AInterface;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclAnsiStrLinkedList } ==================================================

constructor TJclAnsiStrLinkedList.Create(const ACollection: IJclAnsiStrCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclAnsiStrLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclAnsiStrLinkedList.Add(const AString: AnsiString): Boolean;
var
  NewItem: TJclAnsiStrLinkedListItem;
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
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AString, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclAnsiStrLinkedListItem.Create;
        NewItem.Value := AString;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
  Item: AnsiString;
  AddItem: Boolean;
  NewItem: TJclAnsiStrLinkedListItem;
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
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclAnsiStrLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclAnsiStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclAnsiStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclAnsiStrLinkedList.Clear;
var
  Old, Current: TJclAnsiStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeString(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.CollectionEquals(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It, ItSelf: IJclAnsiStrIterator;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.Contains(const AString: AnsiString): Boolean;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AString) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
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
    Result := True;
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

function TJclAnsiStrLinkedList.Delete(Index: Integer): AnsiString;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeString(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclAnsiStrLinkedList.Extract(const AString: AnsiString): Boolean;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AString) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Current.Value := '';
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.ExtractAll(const ACollection: IJclAnsiStrCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclAnsiStrLinkedList.ExtractIndex(Index: Integer): AnsiString;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := Current.Value;
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclAnsiStrLinkedList.First: IJclAnsiStrIterator;
begin
  Result := TJclAnsiStrLinkedListIterator.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrLinkedList.GetEnumerator: IJclAnsiStrIterator;
begin
  Result := TJclAnsiStrLinkedListIterator.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclAnsiStrLinkedList.GetString(Index: Integer): AnsiString;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.IndexOf(const AString: AnsiString): Integer;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AString) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.Insert(Index: Integer; const AString: AnsiString): Boolean;
var
  Current, NewItem: TJclAnsiStrLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AString, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclAnsiStrLinkedListItem.Create;
        NewItem.Value := AString;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
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

function TJclAnsiStrLinkedList.InsertAll(Index: Integer; const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
  Current, NewItem, Test: TJclAnsiStrLinkedListItem;
  AddItem: Boolean;
  Item: AnsiString;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclAnsiStrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclAnsiStrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclAnsiStrLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
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

function TJclAnsiStrLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrLinkedList.Last: IJclAnsiStrIterator;
begin
  Result := TJclAnsiStrLinkedListIterator.Create(Self, FEnd, False, isLast);
end;

function TJclAnsiStrLinkedList.LastIndexOf(const AString: AnsiString): Integer;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AString) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.Remove(const AString: AnsiString): Boolean;
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

function TJclAnsiStrLinkedList.RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclAnsiStrLinkedList.RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrLinkedList.SetString(Index: Integer; const AString: AnsiString);
var
  Current: TJclAnsiStrLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AString, '');
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AString, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeString(Current.Value);
            Current.Value := AString;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
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

function TJclAnsiStrLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrLinkedList.SubList(First, Count: Integer): IJclAnsiStrList;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclAnsiStrList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

//=== { TJclAnsiStrLinkedListIterator } ============================================================

constructor TJclAnsiStrLinkedListIterator.Create(AOwnList: TJclAnsiStrLinkedList; ACursor: TJclAnsiStrLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclAnsiStrEqualityComparer;
end;

function TJclAnsiStrLinkedListIterator.Add(const AString: AnsiString): Boolean;
begin
  Result := FOwnList.Add(AString);
end;

procedure TJclAnsiStrLinkedListIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclAnsiStrLinkedListIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclAnsiStrLinkedListIterator then
  begin
    ADest := TJclAnsiStrLinkedListIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclAnsiStrLinkedListIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclAnsiStrLinkedListIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclAnsiStrLinkedListIterator.Extract;
var
  OldCursor: TJclAnsiStrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedListIterator.GetString: AnsiString;
begin
  CheckValid;
  Result := '';
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TJclAnsiStrLinkedListIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedListIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedListIterator.Insert(const AString: AnsiString): Boolean;
var
  NewCursor: TJclAnsiStrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, '');
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AString);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AString);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclAnsiStrLinkedListItem.Create;
          NewCursor.Value := AString;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedListIterator.IteratorEquals(const AIterator: IJclAnsiStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclAnsiStrLinkedListIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclAnsiStrLinkedListIterator then
  begin
    ItrObj := TJclAnsiStrLinkedListIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrLinkedListIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclAnsiStrLinkedListIterator.Next: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedListIterator.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclAnsiStrLinkedListIterator.Previous: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedListIterator.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrLinkedListIterator.Remove;
var
  OldCursor: TJclAnsiStrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := '';
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrLinkedListIterator.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrLinkedListIterator.SetString(const AString: AnsiString);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := '';
    FCursor.Value := AString;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclWideStrLinkedList } ==================================================

constructor TJclWideStrLinkedList.Create(const ACollection: IJclWideStrCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclWideStrLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclWideStrLinkedList.Add(const AString: WideString): Boolean;
var
  NewItem: TJclWideStrLinkedListItem;
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
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AString, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclWideStrLinkedListItem.Create;
        NewItem.Value := AString;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.AddAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
  Item: WideString;
  AddItem: Boolean;
  NewItem: TJclWideStrLinkedListItem;
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
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclWideStrLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclWideStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclWideStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclWideStrLinkedList.Clear;
var
  Old, Current: TJclWideStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeString(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.CollectionEquals(const ACollection: IJclWideStrCollection): Boolean;
var
  It, ItSelf: IJclWideStrIterator;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.Contains(const AString: WideString): Boolean;
var
  Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AString) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
var
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
    Result := True;
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

function TJclWideStrLinkedList.Delete(Index: Integer): WideString;
var
  Current: TJclWideStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeString(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclWideStrLinkedList.Extract(const AString: WideString): Boolean;
var
  Current: TJclWideStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AString) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Current.Value := '';
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.ExtractAll(const ACollection: IJclWideStrCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclWideStrLinkedList.ExtractIndex(Index: Integer): WideString;
var
  Current: TJclWideStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := Current.Value;
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclWideStrLinkedList.First: IJclWideStrIterator;
begin
  Result := TJclWideStrLinkedListIterator.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrLinkedList.GetEnumerator: IJclWideStrIterator;
begin
  Result := TJclWideStrLinkedListIterator.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclWideStrLinkedList.GetString(Index: Integer): WideString;
var
  Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.IndexOf(const AString: WideString): Integer;
var
  Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AString) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.Insert(Index: Integer; const AString: WideString): Boolean;
var
  Current, NewItem: TJclWideStrLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AString, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclWideStrLinkedListItem.Create;
        NewItem.Value := AString;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
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

function TJclWideStrLinkedList.InsertAll(Index: Integer; const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
  Current, NewItem, Test: TJclWideStrLinkedListItem;
  AddItem: Boolean;
  Item: WideString;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclWideStrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclWideStrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclWideStrLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
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

function TJclWideStrLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrLinkedList.Last: IJclWideStrIterator;
begin
  Result := TJclWideStrLinkedListIterator.Create(Self, FEnd, False, isLast);
end;

function TJclWideStrLinkedList.LastIndexOf(const AString: WideString): Integer;
var
  Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AString) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.Remove(const AString: WideString): Boolean;
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

function TJclWideStrLinkedList.RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclWideStrLinkedList.RetainAll(const ACollection: IJclWideStrCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrLinkedList.SetString(Index: Integer; const AString: WideString);
var
  Current: TJclWideStrLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AString, '');
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AString, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeString(Current.Value);
            Current.Value := AString;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
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

function TJclWideStrLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrLinkedList.SubList(First, Count: Integer): IJclWideStrList;
var
  Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclWideStrList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

//=== { TJclWideStrLinkedListIterator } ============================================================

constructor TJclWideStrLinkedListIterator.Create(AOwnList: TJclWideStrLinkedList; ACursor: TJclWideStrLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclWideStrEqualityComparer;
end;

function TJclWideStrLinkedListIterator.Add(const AString: WideString): Boolean;
begin
  Result := FOwnList.Add(AString);
end;

procedure TJclWideStrLinkedListIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclWideStrLinkedListIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclWideStrLinkedListIterator then
  begin
    ADest := TJclWideStrLinkedListIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclWideStrLinkedListIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclWideStrLinkedListIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclWideStrLinkedListIterator.Extract;
var
  OldCursor: TJclWideStrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedListIterator.GetString: WideString;
begin
  CheckValid;
  Result := '';
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TJclWideStrLinkedListIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedListIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedListIterator.Insert(const AString: WideString): Boolean;
var
  NewCursor: TJclWideStrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, '');
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AString);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AString);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclWideStrLinkedListItem.Create;
          NewCursor.Value := AString;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedListIterator.IteratorEquals(const AIterator: IJclWideStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclWideStrLinkedListIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclWideStrLinkedListIterator then
  begin
    ItrObj := TJclWideStrLinkedListIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrLinkedListIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclWideStrLinkedListIterator.Next: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedListIterator.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclWideStrLinkedListIterator.Previous: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedListIterator.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrLinkedListIterator.Remove;
var
  OldCursor: TJclWideStrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := '';
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrLinkedListIterator.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrLinkedListIterator.SetString(const AString: WideString);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := '';
    FCursor.Value := AString;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrLinkedList } ==================================================

constructor TJclUnicodeStrLinkedList.Create(const ACollection: IJclUnicodeStrCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclUnicodeStrLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclUnicodeStrLinkedList.Add(const AString: UnicodeString): Boolean;
var
  NewItem: TJclUnicodeStrLinkedListItem;
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
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AString, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclUnicodeStrLinkedListItem.Create;
        NewItem.Value := AString;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrLinkedList.AddAll(const ACollection: IJclUnicodeStrCollection): Boolean;
var
  It: IJclUnicodeStrIterator;
  Item: UnicodeString;
  AddItem: Boolean;
  NewItem: TJclUnicodeStrLinkedListItem;
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
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclUnicodeStrLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclUnicodeStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclUnicodeStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclUnicodeStrLinkedList.Clear;
var
  Old, Current: TJclUnicodeStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeString(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrLinkedList.CollectionEquals(const ACollection: IJclUnicodeStrCollection): Boolean;
var
  It, ItSelf: IJclUnicodeStrIterator;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrLinkedList.Contains(const AString: UnicodeString): Boolean;
var
  Current: TJclUnicodeStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AString) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrLinkedList.ContainsAll(const ACollection: IJclUnicodeStrCollection): Boolean;
var
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
    Result := True;
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

function TJclUnicodeStrLinkedList.Delete(Index: Integer): UnicodeString;
var
  Current: TJclUnicodeStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeString(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclUnicodeStrLinkedList.Extract(const AString: UnicodeString): Boolean;
var
  Current: TJclUnicodeStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AString) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Current.Value := '';
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrLinkedList.ExtractAll(const ACollection: IJclUnicodeStrCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclUnicodeStrLinkedList.ExtractIndex(Index: Integer): UnicodeString;
var
  Current: TJclUnicodeStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := Current.Value;
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclUnicodeStrLinkedList.First: IJclUnicodeStrIterator;
begin
  Result := TJclUnicodeStrLinkedListIterator.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclUnicodeStrLinkedList.GetEnumerator: IJclUnicodeStrIterator;
begin
  Result := TJclUnicodeStrLinkedListIterator.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclUnicodeStrLinkedList.GetString(Index: Integer): UnicodeString;
var
  Current: TJclUnicodeStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrLinkedList.IndexOf(const AString: UnicodeString): Integer;
var
  Current: TJclUnicodeStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AString) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrLinkedList.Insert(Index: Integer; const AString: UnicodeString): Boolean;
var
  Current, NewItem: TJclUnicodeStrLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AString, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclUnicodeStrLinkedListItem.Create;
        NewItem.Value := AString;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
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

function TJclUnicodeStrLinkedList.InsertAll(Index: Integer; const ACollection: IJclUnicodeStrCollection): Boolean;
var
  It: IJclUnicodeStrIterator;
  Current, NewItem, Test: TJclUnicodeStrLinkedListItem;
  AddItem: Boolean;
  Item: UnicodeString;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclUnicodeStrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclUnicodeStrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclUnicodeStrLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
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

function TJclUnicodeStrLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclUnicodeStrLinkedList.Last: IJclUnicodeStrIterator;
begin
  Result := TJclUnicodeStrLinkedListIterator.Create(Self, FEnd, False, isLast);
end;

function TJclUnicodeStrLinkedList.LastIndexOf(const AString: UnicodeString): Integer;
var
  Current: TJclUnicodeStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AString) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrLinkedList.Remove(const AString: UnicodeString): Boolean;
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

function TJclUnicodeStrLinkedList.RemoveAll(const ACollection: IJclUnicodeStrCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclUnicodeStrLinkedList.RetainAll(const ACollection: IJclUnicodeStrCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrLinkedList.SetString(Index: Integer; const AString: UnicodeString);
var
  Current: TJclUnicodeStrLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AString, '');
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AString, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeString(Current.Value);
            Current.Value := AString;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
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

function TJclUnicodeStrLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclUnicodeStrLinkedList.SubList(First, Count: Integer): IJclUnicodeStrList;
var
  Current: TJclUnicodeStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclUnicodeStrList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrLinkedListIterator } ============================================================

constructor TJclUnicodeStrLinkedListIterator.Create(AOwnList: TJclUnicodeStrLinkedList; ACursor: TJclUnicodeStrLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclUnicodeStrEqualityComparer;
end;

function TJclUnicodeStrLinkedListIterator.Add(const AString: UnicodeString): Boolean;
begin
  Result := FOwnList.Add(AString);
end;

procedure TJclUnicodeStrLinkedListIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclUnicodeStrLinkedListIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclUnicodeStrLinkedListIterator then
  begin
    ADest := TJclUnicodeStrLinkedListIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclUnicodeStrLinkedListIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclUnicodeStrLinkedListIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclUnicodeStrLinkedListIterator.Extract;
var
  OldCursor: TJclUnicodeStrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrLinkedListIterator.GetString: UnicodeString;
begin
  CheckValid;
  Result := '';
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TJclUnicodeStrLinkedListIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrLinkedListIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrLinkedListIterator.Insert(const AString: UnicodeString): Boolean;
var
  NewCursor: TJclUnicodeStrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, '');
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AString);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AString);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclUnicodeStrLinkedListItem.Create;
          NewCursor.Value := AString;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrLinkedListIterator.IteratorEquals(const AIterator: IJclUnicodeStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclUnicodeStrLinkedListIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclUnicodeStrLinkedListIterator then
  begin
    ItrObj := TJclUnicodeStrLinkedListIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclUnicodeStrLinkedListIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclUnicodeStrLinkedListIterator.Next: UnicodeString;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrLinkedListIterator.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclUnicodeStrLinkedListIterator.Previous: UnicodeString;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrLinkedListIterator.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclUnicodeStrLinkedListIterator.Remove;
var
  OldCursor: TJclUnicodeStrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := '';
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrLinkedListIterator.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrLinkedListIterator.SetString(const AString: UnicodeString);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := '';
    FCursor.Value := AString;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

//=== { TJclSingleLinkedList } ==================================================

constructor TJclSingleLinkedList.Create(const ACollection: IJclSingleCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclSingleLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclSingleLinkedList.Add(const AValue: Single): Boolean;
var
  NewItem: TJclSingleLinkedListItem;
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
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AValue, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclSingleLinkedListItem.Create;
        NewItem.Value := AValue;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.AddAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
  Item: Single;
  AddItem: Boolean;
  NewItem: TJclSingleLinkedListItem;
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
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclSingleLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclSingleCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclSingleCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclSingleLinkedList.Clear;
var
  Old, Current: TJclSingleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeSingle(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.CollectionEquals(const ACollection: IJclSingleCollection): Boolean;
var
  It, ItSelf: IJclSingleIterator;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.Contains(const AValue: Single): Boolean;
var
  Current: TJclSingleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.ContainsAll(const ACollection: IJclSingleCollection): Boolean;
var
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
    Result := True;
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

function TJclSingleLinkedList.Delete(Index: Integer): Single;
var
  Current: TJclSingleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeSingle(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclSingleLinkedList.Extract(const AValue: Single): Boolean;
var
  Current: TJclSingleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Current.Value := 0.0;
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.ExtractAll(const ACollection: IJclSingleCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclSingleLinkedList.ExtractIndex(Index: Integer): Single;
var
  Current: TJclSingleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := Current.Value;
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclSingleLinkedList.First: IJclSingleIterator;
begin
  Result := TJclSingleLinkedListIterator.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSingleLinkedList.GetEnumerator: IJclSingleIterator;
begin
  Result := TJclSingleLinkedListIterator.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSingleLinkedList.GetValue(Index: Integer): Single;
var
  Current: TJclSingleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.IndexOf(const AValue: Single): Integer;
var
  Current: TJclSingleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.Insert(Index: Integer; const AValue: Single): Boolean;
var
  Current, NewItem: TJclSingleLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclSingleLinkedListItem.Create;
        NewItem.Value := AValue;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
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

function TJclSingleLinkedList.InsertAll(Index: Integer; const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
  Current, NewItem, Test: TJclSingleLinkedListItem;
  AddItem: Boolean;
  Item: Single;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclSingleLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclSingleLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclSingleLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
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

function TJclSingleLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleLinkedList.Last: IJclSingleIterator;
begin
  Result := TJclSingleLinkedListIterator.Create(Self, FEnd, False, isLast);
end;

function TJclSingleLinkedList.LastIndexOf(const AValue: Single): Integer;
var
  Current: TJclSingleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.Remove(const AValue: Single): Boolean;
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

function TJclSingleLinkedList.RemoveAll(const ACollection: IJclSingleCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclSingleLinkedList.RetainAll(const ACollection: IJclSingleCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleLinkedList.SetValue(Index: Integer; const AValue: Single);
var
  Current: TJclSingleLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeSingle(Current.Value);
            Current.Value := AValue;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
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

function TJclSingleLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleLinkedList.SubList(First, Count: Integer): IJclSingleList;
var
  Current: TJclSingleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclSingleList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

//=== { TJclSingleLinkedListIterator } ============================================================

constructor TJclSingleLinkedListIterator.Create(AOwnList: TJclSingleLinkedList; ACursor: TJclSingleLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclSingleEqualityComparer;
end;

function TJclSingleLinkedListIterator.Add(const AValue: Single): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TJclSingleLinkedListIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclSingleLinkedListIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclSingleLinkedListIterator then
  begin
    ADest := TJclSingleLinkedListIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclSingleLinkedListIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclSingleLinkedListIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclSingleLinkedListIterator.Extract;
var
  OldCursor: TJclSingleLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedListIterator.GetValue: Single;
begin
  CheckValid;
  Result := 0.0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TJclSingleLinkedListIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedListIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedListIterator.Insert(const AValue: Single): Boolean;
var
  NewCursor: TJclSingleLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AValue);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AValue);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclSingleLinkedListItem.Create;
          NewCursor.Value := AValue;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedListIterator.IteratorEquals(const AIterator: IJclSingleIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclSingleLinkedListIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclSingleLinkedListIterator then
  begin
    ItrObj := TJclSingleLinkedListIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSingleLinkedListIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSingleLinkedListIterator.Next: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedListIterator.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclSingleLinkedListIterator.Previous: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedListIterator.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclSingleLinkedListIterator.Remove;
var
  OldCursor: TJclSingleLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := 0.0;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleLinkedListIterator.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleLinkedListIterator.SetValue(const AValue: Single);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := 0.0;
    FCursor.Value := AValue;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclDoubleLinkedList } ==================================================

constructor TJclDoubleLinkedList.Create(const ACollection: IJclDoubleCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclDoubleLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclDoubleLinkedList.Add(const AValue: Double): Boolean;
var
  NewItem: TJclDoubleLinkedListItem;
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
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AValue, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclDoubleLinkedListItem.Create;
        NewItem.Value := AValue;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.AddAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
  Item: Double;
  AddItem: Boolean;
  NewItem: TJclDoubleLinkedListItem;
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
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclDoubleLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclDoubleCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclDoubleCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclDoubleLinkedList.Clear;
var
  Old, Current: TJclDoubleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeDouble(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.CollectionEquals(const ACollection: IJclDoubleCollection): Boolean;
var
  It, ItSelf: IJclDoubleIterator;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.Contains(const AValue: Double): Boolean;
var
  Current: TJclDoubleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
var
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
    Result := True;
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

function TJclDoubleLinkedList.Delete(Index: Integer): Double;
var
  Current: TJclDoubleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeDouble(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclDoubleLinkedList.Extract(const AValue: Double): Boolean;
var
  Current: TJclDoubleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Current.Value := 0.0;
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.ExtractAll(const ACollection: IJclDoubleCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclDoubleLinkedList.ExtractIndex(Index: Integer): Double;
var
  Current: TJclDoubleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := Current.Value;
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclDoubleLinkedList.First: IJclDoubleIterator;
begin
  Result := TJclDoubleLinkedListIterator.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclDoubleLinkedList.GetEnumerator: IJclDoubleIterator;
begin
  Result := TJclDoubleLinkedListIterator.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclDoubleLinkedList.GetValue(Index: Integer): Double;
var
  Current: TJclDoubleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.IndexOf(const AValue: Double): Integer;
var
  Current: TJclDoubleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.Insert(Index: Integer; const AValue: Double): Boolean;
var
  Current, NewItem: TJclDoubleLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclDoubleLinkedListItem.Create;
        NewItem.Value := AValue;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
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

function TJclDoubleLinkedList.InsertAll(Index: Integer; const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
  Current, NewItem, Test: TJclDoubleLinkedListItem;
  AddItem: Boolean;
  Item: Double;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclDoubleLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclDoubleLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclDoubleLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
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

function TJclDoubleLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleLinkedList.Last: IJclDoubleIterator;
begin
  Result := TJclDoubleLinkedListIterator.Create(Self, FEnd, False, isLast);
end;

function TJclDoubleLinkedList.LastIndexOf(const AValue: Double): Integer;
var
  Current: TJclDoubleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.Remove(const AValue: Double): Boolean;
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

function TJclDoubleLinkedList.RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclDoubleLinkedList.RetainAll(const ACollection: IJclDoubleCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleLinkedList.SetValue(Index: Integer; const AValue: Double);
var
  Current: TJclDoubleLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeDouble(Current.Value);
            Current.Value := AValue;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
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

function TJclDoubleLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleLinkedList.SubList(First, Count: Integer): IJclDoubleList;
var
  Current: TJclDoubleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclDoubleList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

//=== { TJclDoubleLinkedListIterator } ============================================================

constructor TJclDoubleLinkedListIterator.Create(AOwnList: TJclDoubleLinkedList; ACursor: TJclDoubleLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclDoubleEqualityComparer;
end;

function TJclDoubleLinkedListIterator.Add(const AValue: Double): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TJclDoubleLinkedListIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclDoubleLinkedListIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclDoubleLinkedListIterator then
  begin
    ADest := TJclDoubleLinkedListIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclDoubleLinkedListIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclDoubleLinkedListIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclDoubleLinkedListIterator.Extract;
var
  OldCursor: TJclDoubleLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedListIterator.GetValue: Double;
begin
  CheckValid;
  Result := 0.0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TJclDoubleLinkedListIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedListIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedListIterator.Insert(const AValue: Double): Boolean;
var
  NewCursor: TJclDoubleLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AValue);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AValue);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclDoubleLinkedListItem.Create;
          NewCursor.Value := AValue;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedListIterator.IteratorEquals(const AIterator: IJclDoubleIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclDoubleLinkedListIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclDoubleLinkedListIterator then
  begin
    ItrObj := TJclDoubleLinkedListIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclDoubleLinkedListIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclDoubleLinkedListIterator.Next: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedListIterator.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclDoubleLinkedListIterator.Previous: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedListIterator.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclDoubleLinkedListIterator.Remove;
var
  OldCursor: TJclDoubleLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := 0.0;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleLinkedListIterator.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleLinkedListIterator.SetValue(const AValue: Double);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := 0.0;
    FCursor.Value := AValue;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclExtendedLinkedList } ==================================================

constructor TJclExtendedLinkedList.Create(const ACollection: IJclExtendedCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclExtendedLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclExtendedLinkedList.Add(const AValue: Extended): Boolean;
var
  NewItem: TJclExtendedLinkedListItem;
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
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AValue, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclExtendedLinkedListItem.Create;
        NewItem.Value := AValue;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.AddAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
  Item: Extended;
  AddItem: Boolean;
  NewItem: TJclExtendedLinkedListItem;
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
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclExtendedLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclExtendedCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclExtendedCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclExtendedLinkedList.Clear;
var
  Old, Current: TJclExtendedLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeExtended(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.CollectionEquals(const ACollection: IJclExtendedCollection): Boolean;
var
  It, ItSelf: IJclExtendedIterator;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.Contains(const AValue: Extended): Boolean;
var
  Current: TJclExtendedLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
var
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
    Result := True;
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

function TJclExtendedLinkedList.Delete(Index: Integer): Extended;
var
  Current: TJclExtendedLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeExtended(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclExtendedLinkedList.Extract(const AValue: Extended): Boolean;
var
  Current: TJclExtendedLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Current.Value := 0.0;
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.ExtractAll(const ACollection: IJclExtendedCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclExtendedLinkedList.ExtractIndex(Index: Integer): Extended;
var
  Current: TJclExtendedLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := Current.Value;
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclExtendedLinkedList.First: IJclExtendedIterator;
begin
  Result := TJclExtendedLinkedListIterator.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclExtendedLinkedList.GetEnumerator: IJclExtendedIterator;
begin
  Result := TJclExtendedLinkedListIterator.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclExtendedLinkedList.GetValue(Index: Integer): Extended;
var
  Current: TJclExtendedLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.IndexOf(const AValue: Extended): Integer;
var
  Current: TJclExtendedLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.Insert(Index: Integer; const AValue: Extended): Boolean;
var
  Current, NewItem: TJclExtendedLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclExtendedLinkedListItem.Create;
        NewItem.Value := AValue;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
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

function TJclExtendedLinkedList.InsertAll(Index: Integer; const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
  Current, NewItem, Test: TJclExtendedLinkedListItem;
  AddItem: Boolean;
  Item: Extended;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclExtendedLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclExtendedLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclExtendedLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
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

function TJclExtendedLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedLinkedList.Last: IJclExtendedIterator;
begin
  Result := TJclExtendedLinkedListIterator.Create(Self, FEnd, False, isLast);
end;

function TJclExtendedLinkedList.LastIndexOf(const AValue: Extended): Integer;
var
  Current: TJclExtendedLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.Remove(const AValue: Extended): Boolean;
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

function TJclExtendedLinkedList.RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclExtendedLinkedList.RetainAll(const ACollection: IJclExtendedCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedLinkedList.SetValue(Index: Integer; const AValue: Extended);
var
  Current: TJclExtendedLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeExtended(Current.Value);
            Current.Value := AValue;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
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

function TJclExtendedLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedLinkedList.SubList(First, Count: Integer): IJclExtendedList;
var
  Current: TJclExtendedLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclExtendedList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

//=== { TJclExtendedLinkedListIterator } ============================================================

constructor TJclExtendedLinkedListIterator.Create(AOwnList: TJclExtendedLinkedList; ACursor: TJclExtendedLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclExtendedEqualityComparer;
end;

function TJclExtendedLinkedListIterator.Add(const AValue: Extended): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TJclExtendedLinkedListIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclExtendedLinkedListIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclExtendedLinkedListIterator then
  begin
    ADest := TJclExtendedLinkedListIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclExtendedLinkedListIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclExtendedLinkedListIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclExtendedLinkedListIterator.Extract;
var
  OldCursor: TJclExtendedLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedListIterator.GetValue: Extended;
begin
  CheckValid;
  Result := 0.0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TJclExtendedLinkedListIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedListIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedListIterator.Insert(const AValue: Extended): Boolean;
var
  NewCursor: TJclExtendedLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AValue);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AValue);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclExtendedLinkedListItem.Create;
          NewCursor.Value := AValue;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedListIterator.IteratorEquals(const AIterator: IJclExtendedIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclExtendedLinkedListIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclExtendedLinkedListIterator then
  begin
    ItrObj := TJclExtendedLinkedListIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclExtendedLinkedListIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclExtendedLinkedListIterator.Next: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedListIterator.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclExtendedLinkedListIterator.Previous: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedListIterator.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclExtendedLinkedListIterator.Remove;
var
  OldCursor: TJclExtendedLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := 0.0;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedLinkedListIterator.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedLinkedListIterator.SetValue(const AValue: Extended);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := 0.0;
    FCursor.Value := AValue;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclIntegerLinkedList } ==================================================

constructor TJclIntegerLinkedList.Create(const ACollection: IJclIntegerCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclIntegerLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntegerLinkedList.Add(AValue: Integer): Boolean;
var
  NewItem: TJclIntegerLinkedListItem;
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
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AValue, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclIntegerLinkedListItem.Create;
        NewItem.Value := AValue;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.AddAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
  Item: Integer;
  AddItem: Boolean;
  NewItem: TJclIntegerLinkedListItem;
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
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclIntegerLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclIntegerCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclIntegerCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclIntegerLinkedList.Clear;
var
  Old, Current: TJclIntegerLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeInteger(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.CollectionEquals(const ACollection: IJclIntegerCollection): Boolean;
var
  It, ItSelf: IJclIntegerIterator;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.Contains(AValue: Integer): Boolean;
var
  Current: TJclIntegerLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
var
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
    Result := True;
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

function TJclIntegerLinkedList.Delete(Index: Integer): Integer;
var
  Current: TJclIntegerLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeInteger(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclIntegerLinkedList.Extract(AValue: Integer): Boolean;
var
  Current: TJclIntegerLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Current.Value := 0;
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.ExtractAll(const ACollection: IJclIntegerCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclIntegerLinkedList.ExtractIndex(Index: Integer): Integer;
var
  Current: TJclIntegerLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := Current.Value;
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclIntegerLinkedList.First: IJclIntegerIterator;
begin
  Result := TJclIntegerLinkedListIterator.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntegerLinkedList.GetEnumerator: IJclIntegerIterator;
begin
  Result := TJclIntegerLinkedListIterator.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntegerLinkedList.GetValue(Index: Integer): Integer;
var
  Current: TJclIntegerLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.IndexOf(AValue: Integer): Integer;
var
  Current: TJclIntegerLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.Insert(Index: Integer; AValue: Integer): Boolean;
var
  Current, NewItem: TJclIntegerLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclIntegerLinkedListItem.Create;
        NewItem.Value := AValue;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
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

function TJclIntegerLinkedList.InsertAll(Index: Integer; const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
  Current, NewItem, Test: TJclIntegerLinkedListItem;
  AddItem: Boolean;
  Item: Integer;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclIntegerLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclIntegerLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclIntegerLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
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

function TJclIntegerLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerLinkedList.Last: IJclIntegerIterator;
begin
  Result := TJclIntegerLinkedListIterator.Create(Self, FEnd, False, isLast);
end;

function TJclIntegerLinkedList.LastIndexOf(AValue: Integer): Integer;
var
  Current: TJclIntegerLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.Remove(AValue: Integer): Boolean;
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

function TJclIntegerLinkedList.RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclIntegerLinkedList.RetainAll(const ACollection: IJclIntegerCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerLinkedList.SetValue(Index: Integer; AValue: Integer);
var
  Current: TJclIntegerLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeInteger(Current.Value);
            Current.Value := AValue;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
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

function TJclIntegerLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerLinkedList.SubList(First, Count: Integer): IJclIntegerList;
var
  Current: TJclIntegerLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclIntegerList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

//=== { TJclIntegerLinkedListIterator } ============================================================

constructor TJclIntegerLinkedListIterator.Create(AOwnList: TJclIntegerLinkedList; ACursor: TJclIntegerLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclIntegerEqualityComparer;
end;

function TJclIntegerLinkedListIterator.Add(AValue: Integer): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TJclIntegerLinkedListIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclIntegerLinkedListIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclIntegerLinkedListIterator then
  begin
    ADest := TJclIntegerLinkedListIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclIntegerLinkedListIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclIntegerLinkedListIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclIntegerLinkedListIterator.Extract;
var
  OldCursor: TJclIntegerLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedListIterator.GetValue: Integer;
begin
  CheckValid;
  Result := 0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TJclIntegerLinkedListIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedListIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedListIterator.Insert(AValue: Integer): Boolean;
var
  NewCursor: TJclIntegerLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AValue);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AValue);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclIntegerLinkedListItem.Create;
          NewCursor.Value := AValue;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedListIterator.IteratorEquals(const AIterator: IJclIntegerIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclIntegerLinkedListIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclIntegerLinkedListIterator then
  begin
    ItrObj := TJclIntegerLinkedListIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntegerLinkedListIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntegerLinkedListIterator.Next: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedListIterator.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclIntegerLinkedListIterator.Previous: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedListIterator.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntegerLinkedListIterator.Remove;
var
  OldCursor: TJclIntegerLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := 0;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerLinkedListIterator.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerLinkedListIterator.SetValue(AValue: Integer);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := 0;
    FCursor.Value := AValue;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclCardinalLinkedList } ==================================================

constructor TJclCardinalLinkedList.Create(const ACollection: IJclCardinalCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclCardinalLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclCardinalLinkedList.Add(AValue: Cardinal): Boolean;
var
  NewItem: TJclCardinalLinkedListItem;
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
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AValue, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclCardinalLinkedListItem.Create;
        NewItem.Value := AValue;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.AddAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
  Item: Cardinal;
  AddItem: Boolean;
  NewItem: TJclCardinalLinkedListItem;
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
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclCardinalLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclCardinalCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclCardinalCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclCardinalLinkedList.Clear;
var
  Old, Current: TJclCardinalLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeCardinal(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.CollectionEquals(const ACollection: IJclCardinalCollection): Boolean;
var
  It, ItSelf: IJclCardinalIterator;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.Contains(AValue: Cardinal): Boolean;
var
  Current: TJclCardinalLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
var
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
    Result := True;
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

function TJclCardinalLinkedList.Delete(Index: Integer): Cardinal;
var
  Current: TJclCardinalLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeCardinal(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclCardinalLinkedList.Extract(AValue: Cardinal): Boolean;
var
  Current: TJclCardinalLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Current.Value := 0;
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.ExtractAll(const ACollection: IJclCardinalCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclCardinalLinkedList.ExtractIndex(Index: Integer): Cardinal;
var
  Current: TJclCardinalLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := Current.Value;
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclCardinalLinkedList.First: IJclCardinalIterator;
begin
  Result := TJclCardinalLinkedListIterator.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclCardinalLinkedList.GetEnumerator: IJclCardinalIterator;
begin
  Result := TJclCardinalLinkedListIterator.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclCardinalLinkedList.GetValue(Index: Integer): Cardinal;
var
  Current: TJclCardinalLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.IndexOf(AValue: Cardinal): Integer;
var
  Current: TJclCardinalLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.Insert(Index: Integer; AValue: Cardinal): Boolean;
var
  Current, NewItem: TJclCardinalLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclCardinalLinkedListItem.Create;
        NewItem.Value := AValue;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
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

function TJclCardinalLinkedList.InsertAll(Index: Integer; const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
  Current, NewItem, Test: TJclCardinalLinkedListItem;
  AddItem: Boolean;
  Item: Cardinal;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclCardinalLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclCardinalLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclCardinalLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
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

function TJclCardinalLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalLinkedList.Last: IJclCardinalIterator;
begin
  Result := TJclCardinalLinkedListIterator.Create(Self, FEnd, False, isLast);
end;

function TJclCardinalLinkedList.LastIndexOf(AValue: Cardinal): Integer;
var
  Current: TJclCardinalLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.Remove(AValue: Cardinal): Boolean;
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

function TJclCardinalLinkedList.RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclCardinalLinkedList.RetainAll(const ACollection: IJclCardinalCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalLinkedList.SetValue(Index: Integer; AValue: Cardinal);
var
  Current: TJclCardinalLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeCardinal(Current.Value);
            Current.Value := AValue;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
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

function TJclCardinalLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalLinkedList.SubList(First, Count: Integer): IJclCardinalList;
var
  Current: TJclCardinalLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclCardinalList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

//=== { TJclCardinalLinkedListIterator } ============================================================

constructor TJclCardinalLinkedListIterator.Create(AOwnList: TJclCardinalLinkedList; ACursor: TJclCardinalLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclCardinalEqualityComparer;
end;

function TJclCardinalLinkedListIterator.Add(AValue: Cardinal): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TJclCardinalLinkedListIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclCardinalLinkedListIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclCardinalLinkedListIterator then
  begin
    ADest := TJclCardinalLinkedListIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclCardinalLinkedListIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclCardinalLinkedListIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclCardinalLinkedListIterator.Extract;
var
  OldCursor: TJclCardinalLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedListIterator.GetValue: Cardinal;
begin
  CheckValid;
  Result := 0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TJclCardinalLinkedListIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedListIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedListIterator.Insert(AValue: Cardinal): Boolean;
var
  NewCursor: TJclCardinalLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AValue);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AValue);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclCardinalLinkedListItem.Create;
          NewCursor.Value := AValue;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedListIterator.IteratorEquals(const AIterator: IJclCardinalIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclCardinalLinkedListIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclCardinalLinkedListIterator then
  begin
    ItrObj := TJclCardinalLinkedListIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclCardinalLinkedListIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclCardinalLinkedListIterator.Next: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedListIterator.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclCardinalLinkedListIterator.Previous: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedListIterator.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclCardinalLinkedListIterator.Remove;
var
  OldCursor: TJclCardinalLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := 0;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalLinkedListIterator.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalLinkedListIterator.SetValue(AValue: Cardinal);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := 0;
    FCursor.Value := AValue;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclInt64LinkedList } ==================================================

constructor TJclInt64LinkedList.Create(const ACollection: IJclInt64Collection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclInt64LinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclInt64LinkedList.Add(const AValue: Int64): Boolean;
var
  NewItem: TJclInt64LinkedListItem;
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
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AValue, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclInt64LinkedListItem.Create;
        NewItem.Value := AValue;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.AddAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
  Item: Int64;
  AddItem: Boolean;
  NewItem: TJclInt64LinkedListItem;
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
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclInt64LinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64LinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclInt64Collection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclInt64Collection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclInt64LinkedList.Clear;
var
  Old, Current: TJclInt64LinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeInt64(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.CollectionEquals(const ACollection: IJclInt64Collection): Boolean;
var
  It, ItSelf: IJclInt64Iterator;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.Contains(const AValue: Int64): Boolean;
var
  Current: TJclInt64LinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.ContainsAll(const ACollection: IJclInt64Collection): Boolean;
var
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
    Result := True;
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

function TJclInt64LinkedList.Delete(Index: Integer): Int64;
var
  Current: TJclInt64LinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeInt64(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclInt64LinkedList.Extract(const AValue: Int64): Boolean;
var
  Current: TJclInt64LinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Current.Value := 0;
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.ExtractAll(const ACollection: IJclInt64Collection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclInt64LinkedList.ExtractIndex(Index: Integer): Int64;
var
  Current: TJclInt64LinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := Current.Value;
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclInt64LinkedList.First: IJclInt64Iterator;
begin
  Result := TJclInt64LinkedListIterator.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclInt64LinkedList.GetEnumerator: IJclInt64Iterator;
begin
  Result := TJclInt64LinkedListIterator.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclInt64LinkedList.GetValue(Index: Integer): Int64;
var
  Current: TJclInt64LinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.IndexOf(const AValue: Int64): Integer;
var
  Current: TJclInt64LinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.Insert(Index: Integer; const AValue: Int64): Boolean;
var
  Current, NewItem: TJclInt64LinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclInt64LinkedListItem.Create;
        NewItem.Value := AValue;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
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

function TJclInt64LinkedList.InsertAll(Index: Integer; const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
  Current, NewItem, Test: TJclInt64LinkedListItem;
  AddItem: Boolean;
  Item: Int64;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclInt64LinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclInt64LinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclInt64LinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
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

function TJclInt64LinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64LinkedList.Last: IJclInt64Iterator;
begin
  Result := TJclInt64LinkedListIterator.Create(Self, FEnd, False, isLast);
end;

function TJclInt64LinkedList.LastIndexOf(const AValue: Int64): Integer;
var
  Current: TJclInt64LinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.Remove(const AValue: Int64): Boolean;
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

function TJclInt64LinkedList.RemoveAll(const ACollection: IJclInt64Collection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclInt64LinkedList.RetainAll(const ACollection: IJclInt64Collection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64LinkedList.SetValue(Index: Integer; const AValue: Int64);
var
  Current: TJclInt64LinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeInt64(Current.Value);
            Current.Value := AValue;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
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

function TJclInt64LinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64LinkedList.SubList(First, Count: Integer): IJclInt64List;
var
  Current: TJclInt64LinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclInt64List;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64LinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

//=== { TJclInt64LinkedListIterator } ============================================================

constructor TJclInt64LinkedListIterator.Create(AOwnList: TJclInt64LinkedList; ACursor: TJclInt64LinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclInt64EqualityComparer;
end;

function TJclInt64LinkedListIterator.Add(const AValue: Int64): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TJclInt64LinkedListIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclInt64LinkedListIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclInt64LinkedListIterator then
  begin
    ADest := TJclInt64LinkedListIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclInt64LinkedListIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclInt64LinkedListIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclInt64LinkedListIterator.Extract;
var
  OldCursor: TJclInt64LinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedListIterator.GetValue: Int64;
begin
  CheckValid;
  Result := 0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TJclInt64LinkedListIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedListIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedListIterator.Insert(const AValue: Int64): Boolean;
var
  NewCursor: TJclInt64LinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AValue);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AValue);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclInt64LinkedListItem.Create;
          NewCursor.Value := AValue;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedListIterator.IteratorEquals(const AIterator: IJclInt64Iterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclInt64LinkedListIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclInt64LinkedListIterator then
  begin
    ItrObj := TJclInt64LinkedListIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclInt64LinkedListIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclInt64LinkedListIterator.Next: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedListIterator.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclInt64LinkedListIterator.Previous: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedListIterator.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclInt64LinkedListIterator.Remove;
var
  OldCursor: TJclInt64LinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := 0;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64LinkedListIterator.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64LinkedListIterator.SetValue(const AValue: Int64);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := 0;
    FCursor.Value := AValue;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclPtrLinkedList } ==================================================

constructor TJclPtrLinkedList.Create(const ACollection: IJclPtrCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclPtrLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclPtrLinkedList.Add(APtr: Pointer): Boolean;
var
  NewItem: TJclPtrLinkedListItem;
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
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(APtr, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclPtrLinkedListItem.Create;
        NewItem.Value := APtr;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.AddAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
  Item: Pointer;
  AddItem: Boolean;
  NewItem: TJclPtrLinkedListItem;
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
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclPtrLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclPtrCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclPtrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclPtrLinkedList.Clear;
var
  Old, Current: TJclPtrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreePointer(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.CollectionEquals(const ACollection: IJclPtrCollection): Boolean;
var
  It, ItSelf: IJclPtrIterator;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.Contains(APtr: Pointer): Boolean;
var
  Current: TJclPtrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, APtr) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.ContainsAll(const ACollection: IJclPtrCollection): Boolean;
var
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
    Result := True;
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

function TJclPtrLinkedList.Delete(Index: Integer): Pointer;
var
  Current: TJclPtrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreePointer(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclPtrLinkedList.Extract(APtr: Pointer): Boolean;
var
  Current: TJclPtrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, APtr) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Current.Value := nil;
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.ExtractAll(const ACollection: IJclPtrCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclPtrLinkedList.ExtractIndex(Index: Integer): Pointer;
var
  Current: TJclPtrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := Current.Value;
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclPtrLinkedList.First: IJclPtrIterator;
begin
  Result := TJclPtrLinkedListIterator.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclPtrLinkedList.GetEnumerator: IJclPtrIterator;
begin
  Result := TJclPtrLinkedListIterator.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclPtrLinkedList.GetPointer(Index: Integer): Pointer;
var
  Current: TJclPtrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.IndexOf(APtr: Pointer): Integer;
var
  Current: TJclPtrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, APtr) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.Insert(Index: Integer; APtr: Pointer): Boolean;
var
  Current, NewItem: TJclPtrLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(APtr, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclPtrLinkedListItem.Create;
        NewItem.Value := APtr;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
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

function TJclPtrLinkedList.InsertAll(Index: Integer; const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
  Current, NewItem, Test: TJclPtrLinkedListItem;
  AddItem: Boolean;
  Item: Pointer;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclPtrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclPtrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclPtrLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
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

function TJclPtrLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrLinkedList.Last: IJclPtrIterator;
begin
  Result := TJclPtrLinkedListIterator.Create(Self, FEnd, False, isLast);
end;

function TJclPtrLinkedList.LastIndexOf(APtr: Pointer): Integer;
var
  Current: TJclPtrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, APtr) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.Remove(APtr: Pointer): Boolean;
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

function TJclPtrLinkedList.RemoveAll(const ACollection: IJclPtrCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclPtrLinkedList.RetainAll(const ACollection: IJclPtrCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrLinkedList.SetPointer(Index: Integer; APtr: Pointer);
var
  Current: TJclPtrLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(APtr, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(APtr, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreePointer(Current.Value);
            Current.Value := APtr;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
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

function TJclPtrLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrLinkedList.SubList(First, Count: Integer): IJclPtrList;
var
  Current: TJclPtrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclPtrList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

//=== { TJclPtrLinkedListIterator } ============================================================

constructor TJclPtrLinkedListIterator.Create(AOwnList: TJclPtrLinkedList; ACursor: TJclPtrLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclPtrEqualityComparer;
end;

function TJclPtrLinkedListIterator.Add(APtr: Pointer): Boolean;
begin
  Result := FOwnList.Add(APtr);
end;

procedure TJclPtrLinkedListIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclPtrLinkedListIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclPtrLinkedListIterator then
  begin
    ADest := TJclPtrLinkedListIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclPtrLinkedListIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclPtrLinkedListIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclPtrLinkedListIterator.Extract;
var
  OldCursor: TJclPtrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedListIterator.GetPointer: Pointer;
begin
  CheckValid;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TJclPtrLinkedListIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedListIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedListIterator.Insert(APtr: Pointer): Boolean;
var
  NewCursor: TJclPtrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(APtr, nil);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(APtr);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(APtr);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclPtrLinkedListItem.Create;
          NewCursor.Value := APtr;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedListIterator.IteratorEquals(const AIterator: IJclPtrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclPtrLinkedListIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclPtrLinkedListIterator then
  begin
    ItrObj := TJclPtrLinkedListIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclPtrLinkedListIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclPtrLinkedListIterator.Next: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedListIterator.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclPtrLinkedListIterator.Previous: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedListIterator.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclPtrLinkedListIterator.Remove;
var
  OldCursor: TJclPtrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := nil;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrLinkedListIterator.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrLinkedListIterator.SetPointer(APtr: Pointer);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := nil;
    FCursor.Value := APtr;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclLinkedList } ==================================================

constructor TJclLinkedList.Create(const ACollection: IJclCollection; AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclLinkedList.Add(AObject: TObject): Boolean;
var
  NewItem: TJclLinkedListItem;
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
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AObject, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclLinkedListItem.Create;
        NewItem.Value := AObject;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.AddAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  Item: TObject;
  AddItem: Boolean;
  NewItem: TJclLinkedListItem;
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
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclLinkedList.Clear;
var
  Old, Current: TJclLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeObject(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.CollectionEquals(const ACollection: IJclCollection): Boolean;
var
  It, ItSelf: IJclIterator;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.Contains(AObject: TObject): Boolean;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AObject) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.ContainsAll(const ACollection: IJclCollection): Boolean;
var
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
    Result := True;
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

function TJclLinkedList.Delete(Index: Integer): TObject;
var
  Current: TJclLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeObject(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclLinkedList.Extract(AObject: TObject): Boolean;
var
  Current: TJclLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AObject) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Current.Value := nil;
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.ExtractAll(const ACollection: IJclCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclLinkedList.ExtractIndex(Index: Integer): TObject;
var
  Current: TJclLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := Current.Value;
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclLinkedList.First: IJclIterator;
begin
  Result := TJclLinkedListIterator.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclLinkedList.GetEnumerator: IJclIterator;
begin
  Result := TJclLinkedListIterator.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclLinkedList.GetObject(Index: Integer): TObject;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.IndexOf(AObject: TObject): Integer;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AObject) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.Insert(Index: Integer; AObject: TObject): Boolean;
var
  Current, NewItem: TJclLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AObject, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclLinkedListItem.Create;
        NewItem.Value := AObject;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
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

function TJclLinkedList.InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  Current, NewItem, Test: TJclLinkedListItem;
  AddItem: Boolean;
  Item: TObject;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
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

function TJclLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclLinkedList.Last: IJclIterator;
begin
  Result := TJclLinkedListIterator.Create(Self, FEnd, False, isLast);
end;

function TJclLinkedList.LastIndexOf(AObject: TObject): Integer;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AObject) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.Remove(AObject: TObject): Boolean;
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

function TJclLinkedList.RemoveAll(const ACollection: IJclCollection): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclLinkedList.RetainAll(const ACollection: IJclCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList.SetObject(Index: Integer; AObject: TObject);
var
  Current: TJclLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AObject, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeObject(Current.Value);
            Current.Value := AObject;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
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

function TJclLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclLinkedList.SubList(First, Count: Integer): IJclList;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedList.Create(nil, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclLinkedListIterator } ============================================================

constructor TJclLinkedListIterator.Create(AOwnList: TJclLinkedList; ACursor: TJclLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclEqualityComparer;
end;

function TJclLinkedListIterator.Add(AObject: TObject): Boolean;
begin
  Result := FOwnList.Add(AObject);
end;

procedure TJclLinkedListIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclLinkedListIterator;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclLinkedListIterator then
  begin
    ADest := TJclLinkedListIterator(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclLinkedListIterator.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclLinkedListIterator.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclLinkedListIterator.Extract;
var
  OldCursor: TJclLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedListIterator.GetObject: TObject;
begin
  CheckValid;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TJclLinkedListIterator.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedListIterator.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedListIterator.Insert(AObject: TObject): Boolean;
var
  NewCursor: TJclLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AObject, nil);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AObject);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AObject);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclLinkedListItem.Create;
          NewCursor.Value := AObject;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedListIterator.IteratorEquals(const AIterator: IJclIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TJclLinkedListIterator;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclLinkedListIterator then
  begin
    ItrObj := TJclLinkedListIterator(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclLinkedListIterator.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclLinkedListIterator.Next: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedListIterator.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclLinkedListIterator.Previous: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedListIterator.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclLinkedListIterator.Remove;
var
  OldCursor: TJclLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      (FownList as IJclObjectOwner).FreeObject(FCursor.Value);
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedListIterator.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedListIterator.SetObject(AObject: TObject);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    (FownList as IJclObjectOwner).FreeObject(FCursor.Value);
    FCursor.Value := AObject;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;


{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

//=== { TJclLinkedList<T> } ==================================================

constructor TJclLinkedList<T>.Create(const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclLinkedList<T>.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclLinkedList<T>.Add(const AItem: T): Boolean;
var
  NewItem: TLinkedListItem;
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
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AItem, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TLinkedListItem.Create;
        NewItem.Value := AItem;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
  Item: T;
  AddItem: Boolean;
  NewItem: TLinkedListItem;
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
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, Default(T));
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclCollection<T>;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclCollection<T>, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclLinkedList<T>.Clear;
var
  Old, Current: TLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeItem(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.CollectionEquals(const ACollection: IJclCollection<T>): Boolean;
var
  It, ItSelf: IJclIterator<T>;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.Contains(const AItem: T): Boolean;
var
  Current: TLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AItem) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
var
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
    Result := True;
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

function TJclLinkedList<T>.Delete(Index: Integer): T;
var
  Current: TLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeItem(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclLinkedList<T>.Extract(const AItem: T): Boolean;
var
  Current: TLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AItem) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Current.Value := Default(T);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.ExtractAll(const ACollection: IJclCollection<T>): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclLinkedList<T>.ExtractIndex(Index: Integer): T;
var
  Current: TLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := Current.Value;
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
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

function TJclLinkedList<T>.First: IJclIterator<T>;
begin
  Result := TLinkedListIterator.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclLinkedList<T>.GetEnumerator: IJclIterator<T>;
begin
  Result := TLinkedListIterator.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclLinkedList<T>.GetItem(Index: Integer): T;
var
  Current: TLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.IndexOf(const AItem: T): Integer;
var
  Current: TLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AItem) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.Insert(Index: Integer; const AItem: T): Boolean;
var
  Current, NewItem: TLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AItem, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TLinkedListItem.Create;
        NewItem.Value := AItem;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
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

function TJclLinkedList<T>.InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
  Current, NewItem, Test: TLinkedListItem;
  AddItem: Boolean;
  Item: T;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, Default(T));
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, Default(T));
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, Default(T));
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
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

function TJclLinkedList<T>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclLinkedList<T>.Last: IJclIterator<T>;
begin
  Result := TLinkedListIterator.Create(Self, FEnd, False, isLast);
end;

function TJclLinkedList<T>.LastIndexOf(const AItem: T): Integer;
var
  Current: TLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AItem) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.Remove(const AItem: T): Boolean;
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

function TJclLinkedList<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
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
    Result := True;
    if ACollection = nil then
      Exit;
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

function TJclLinkedList<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList<T>.SetItem(Index: Integer; const AItem: T);
var
  Current: TLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AItem, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeItem(Current.Value);
            Current.Value := AItem;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
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

function TJclLinkedList<T>.Size: Integer;
begin
  Result := FSize;
end;

function TJclLinkedList<T>.SubList(First, Count: Integer): IJclList<T>;
var
  Current: TLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclList<T>;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclLinkedListIterator<T> } ============================================================

constructor TJclLinkedListIterator<T>.Create(AOwnList: IJclList<T>; ACursor: TJclLinkedList<T>.TLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclEqualityComparer<T>;
end;

function TJclLinkedListIterator<T>.Add(const AItem: T): Boolean;
begin
  Result := FOwnList.Add(AItem);
end;

procedure TJclLinkedListIterator<T>.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TJclLinkedListIterator<T>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclLinkedListIterator<T> then
  begin
    ADest := TJclLinkedListIterator<T>(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TJclLinkedListIterator<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TJclLinkedListIterator<T>.Create(FOwnList, FCursor, Valid, FStart);
end;

procedure TJclLinkedListIterator<T>.Extract;
var
  OldCursor: TJclLinkedList<T>.TLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedListIterator<T>.GetItem: T;
begin
  CheckValid;
  Result := Default(T);
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TJclLinkedListIterator<T>.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedListIterator<T>.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedListIterator<T>.Insert(const AItem: T): Boolean;
var
  NewCursor: TJclLinkedList<T>.TLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AItem, Default(T));
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AItem);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AItem);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclLinkedList<T>.TLinkedListItem.Create;
          NewCursor.Value := AItem;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedListIterator<T>.IteratorEquals(const AIterator: IJclIterator<T>): Boolean;
var
  Obj: TObject;
  ItrObj: TJclLinkedListIterator<T>;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TJclLinkedListIterator<T> then
  begin
    ItrObj := TJclLinkedListIterator<T>(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclLinkedListIterator<T>.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclLinkedListIterator<T>.Next: T;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := Default(T);
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedListIterator<T>.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TJclLinkedListIterator<T>.Previous: T;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := Default(T);
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedListIterator<T>.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclLinkedListIterator<T>.Remove;
var
  OldCursor: TJclLinkedList<T>.TLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      (FownList as IJclItemOwner<T>).FreeItem(FCursor.Value);
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedListIterator<T>.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedListIterator<T>.SetItem(const AItem: T);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    (FownList as IJclItemOwner<T>).FreeItem(FCursor.Value);
    FCursor.Value := AItem;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

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

