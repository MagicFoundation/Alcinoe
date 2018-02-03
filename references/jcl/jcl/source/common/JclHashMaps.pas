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


type
  TJclIntfIntfHashMapEntry = record
    Key: IInterface;
    Value: IInterface;
  end;

  TJclIntfIntfHashMapEntryArray = array of TJclIntfIntfHashMapEntry;

  TJclIntfIntfHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclIntfIntfHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclIntfIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclIntfIntfHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclIntfIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclIntfIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclIntfIntfHashMap = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclIntfContainer,
    IJclIntfIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclIntfIntfHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfIntfMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: IInterface): IInterface;
    function GetValue(const Key: IInterface): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): IInterface;
    function KeySet: IJclIntfSet;
    function Keys: IJclIntfCollection;
    function MapEquals(const AMap: IJclIntfIntfMap): Boolean;
    procedure PutAll(const AMap: IJclIntfIntfMap);
    procedure PutValue(const Key: IInterface; const Value: IInterface);
    function Remove(const Key: IInterface): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  end;

  TJclAnsiStrIntfHashMapEntry = record
    Key: AnsiString;
    Value: IInterface;
  end;

  TJclAnsiStrIntfHashMapEntryArray = array of TJclAnsiStrIntfHashMapEntry;

  TJclAnsiStrIntfHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclAnsiStrIntfHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclAnsiStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclAnsiStrIntfHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclAnsiStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclAnsiStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclAnsiStrIntfHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclStrBaseContainer, IJclAnsiStrContainer, IJclIntfContainer,
    IJclAnsiStrIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: AnsiString): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclAnsiStrIntfHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrIntfMap }
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: AnsiString): IInterface;
    function GetValue(const Key: AnsiString): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): AnsiString;
    function KeySet: IJclAnsiStrSet;
    function Keys: IJclAnsiStrCollection;
    function MapEquals(const AMap: IJclAnsiStrIntfMap): Boolean;
    procedure PutAll(const AMap: IJclAnsiStrIntfMap);
    procedure PutValue(const Key: AnsiString; const Value: IInterface);
    function Remove(const Key: AnsiString): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  end;

  TJclIntfAnsiStrHashMapEntry = record
    Key: IInterface;
    Value: AnsiString;
  end;

  TJclIntfAnsiStrHashMapEntryArray = array of TJclIntfAnsiStrHashMapEntry;

  TJclIntfAnsiStrHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclIntfAnsiStrHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclIntfAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclIntfAnsiStrHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclIntfAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclIntfAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclIntfAnsiStrHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclStrBaseContainer, IJclIntfContainer, IJclAnsiStrContainer,
    IJclIntfAnsiStrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: AnsiString): AnsiString;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: AnsiString): Boolean;
  private
    FBuckets: array of TJclIntfAnsiStrHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfAnsiStrMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: AnsiString): Boolean;
    function Extract(const Key: IInterface): AnsiString;
    function GetValue(const Key: IInterface): AnsiString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: AnsiString): IInterface;
    function KeySet: IJclIntfSet;
    function Keys: IJclIntfCollection;
    function MapEquals(const AMap: IJclIntfAnsiStrMap): Boolean;
    procedure PutAll(const AMap: IJclIntfAnsiStrMap);
    procedure PutValue(const Key: IInterface; const Value: AnsiString);
    function Remove(const Key: IInterface): AnsiString;
    function Size: Integer;
    function Values: IJclAnsiStrCollection;
  end;

  TJclAnsiStrAnsiStrHashMapEntry = record
    Key: AnsiString;
    Value: AnsiString;
  end;

  TJclAnsiStrAnsiStrHashMapEntryArray = array of TJclAnsiStrAnsiStrHashMapEntry;

  TJclAnsiStrAnsiStrHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclAnsiStrAnsiStrHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclAnsiStrAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclAnsiStrAnsiStrHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclAnsiStrAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclAnsiStrAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclAnsiStrAnsiStrHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclStrBaseContainer, IJclAnsiStrContainer,
    IJclAnsiStrAnsiStrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: AnsiString): AnsiString;
    function KeysEqual(const A, B: AnsiString): Boolean;
    function ValuesEqual(const A, B: AnsiString): Boolean;
  private
    FBuckets: array of TJclAnsiStrAnsiStrHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrAnsiStrMap }
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(const Value: AnsiString): Boolean;
    function Extract(const Key: AnsiString): AnsiString;
    function GetValue(const Key: AnsiString): AnsiString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: AnsiString): AnsiString;
    function KeySet: IJclAnsiStrSet;
    function Keys: IJclAnsiStrCollection;
    function MapEquals(const AMap: IJclAnsiStrAnsiStrMap): Boolean;
    procedure PutAll(const AMap: IJclAnsiStrAnsiStrMap);
    procedure PutValue(const Key: AnsiString; const Value: AnsiString);
    function Remove(const Key: AnsiString): AnsiString;
    function Size: Integer;
    function Values: IJclAnsiStrCollection;
  end;

  TJclWideStrIntfHashMapEntry = record
    Key: WideString;
    Value: IInterface;
  end;

  TJclWideStrIntfHashMapEntryArray = array of TJclWideStrIntfHashMapEntry;

  TJclWideStrIntfHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclWideStrIntfHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclWideStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclWideStrIntfHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclWideStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclWideStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclWideStrIntfHashMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclStrBaseContainer, IJclWideStrContainer, IJclIntfContainer,
    IJclWideStrIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: WideString): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclWideStrIntfHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrIntfMap }
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: WideString): IInterface;
    function GetValue(const Key: WideString): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): WideString;
    function KeySet: IJclWideStrSet;
    function Keys: IJclWideStrCollection;
    function MapEquals(const AMap: IJclWideStrIntfMap): Boolean;
    procedure PutAll(const AMap: IJclWideStrIntfMap);
    procedure PutValue(const Key: WideString; const Value: IInterface);
    function Remove(const Key: WideString): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  end;

  TJclIntfWideStrHashMapEntry = record
    Key: IInterface;
    Value: WideString;
  end;

  TJclIntfWideStrHashMapEntryArray = array of TJclIntfWideStrHashMapEntry;

  TJclIntfWideStrHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclIntfWideStrHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclIntfWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclIntfWideStrHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclIntfWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclIntfWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclIntfWideStrHashMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclStrBaseContainer, IJclIntfContainer, IJclWideStrContainer,
    IJclIntfWideStrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: WideString): WideString;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: WideString): Boolean;
  private
    FBuckets: array of TJclIntfWideStrHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfWideStrMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: WideString): Boolean;
    function Extract(const Key: IInterface): WideString;
    function GetValue(const Key: IInterface): WideString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: WideString): IInterface;
    function KeySet: IJclIntfSet;
    function Keys: IJclIntfCollection;
    function MapEquals(const AMap: IJclIntfWideStrMap): Boolean;
    procedure PutAll(const AMap: IJclIntfWideStrMap);
    procedure PutValue(const Key: IInterface; const Value: WideString);
    function Remove(const Key: IInterface): WideString;
    function Size: Integer;
    function Values: IJclWideStrCollection;
  end;

  TJclWideStrWideStrHashMapEntry = record
    Key: WideString;
    Value: WideString;
  end;

  TJclWideStrWideStrHashMapEntryArray = array of TJclWideStrWideStrHashMapEntry;

  TJclWideStrWideStrHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclWideStrWideStrHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclWideStrWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclWideStrWideStrHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclWideStrWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclWideStrWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclWideStrWideStrHashMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclStrBaseContainer, IJclWideStrContainer,
    IJclWideStrWideStrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: WideString): WideString;
    function KeysEqual(const A, B: WideString): Boolean;
    function ValuesEqual(const A, B: WideString): Boolean;
  private
    FBuckets: array of TJclWideStrWideStrHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrWideStrMap }
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(const Value: WideString): Boolean;
    function Extract(const Key: WideString): WideString;
    function GetValue(const Key: WideString): WideString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: WideString): WideString;
    function KeySet: IJclWideStrSet;
    function Keys: IJclWideStrCollection;
    function MapEquals(const AMap: IJclWideStrWideStrMap): Boolean;
    procedure PutAll(const AMap: IJclWideStrWideStrMap);
    procedure PutValue(const Key: WideString; const Value: WideString);
    function Remove(const Key: WideString): WideString;
    function Size: Integer;
    function Values: IJclWideStrCollection;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrIntfHashMapEntry = record
    Key: UnicodeString;
    Value: IInterface;
  end;

  TJclUnicodeStrIntfHashMapEntryArray = array of TJclUnicodeStrIntfHashMapEntry;

  TJclUnicodeStrIntfHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclUnicodeStrIntfHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclUnicodeStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclUnicodeStrIntfHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclUnicodeStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclUnicodeStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrIntfHashMap = class(TJclUnicodeStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclStrBaseContainer, IJclUnicodeStrContainer, IJclIntfContainer,
    IJclUnicodeStrIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: UnicodeString): UnicodeString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: UnicodeString): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclUnicodeStrIntfHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclUnicodeStrIntfMap }
    procedure Clear;
    function ContainsKey(const Key: UnicodeString): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: UnicodeString): IInterface;
    function GetValue(const Key: UnicodeString): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): UnicodeString;
    function KeySet: IJclUnicodeStrSet;
    function Keys: IJclUnicodeStrCollection;
    function MapEquals(const AMap: IJclUnicodeStrIntfMap): Boolean;
    procedure PutAll(const AMap: IJclUnicodeStrIntfMap);
    procedure PutValue(const Key: UnicodeString; const Value: IInterface);
    function Remove(const Key: UnicodeString): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclIntfUnicodeStrHashMapEntry = record
    Key: IInterface;
    Value: UnicodeString;
  end;

  TJclIntfUnicodeStrHashMapEntryArray = array of TJclIntfUnicodeStrHashMapEntry;

  TJclIntfUnicodeStrHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclIntfUnicodeStrHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclIntfUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclIntfUnicodeStrHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclIntfUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclIntfUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclIntfUnicodeStrHashMap = class(TJclUnicodeStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclStrBaseContainer, IJclIntfContainer, IJclUnicodeStrContainer,
    IJclIntfUnicodeStrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: UnicodeString): UnicodeString;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: UnicodeString): Boolean;
  private
    FBuckets: array of TJclIntfUnicodeStrHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfUnicodeStrMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: UnicodeString): Boolean;
    function Extract(const Key: IInterface): UnicodeString;
    function GetValue(const Key: IInterface): UnicodeString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: UnicodeString): IInterface;
    function KeySet: IJclIntfSet;
    function Keys: IJclIntfCollection;
    function MapEquals(const AMap: IJclIntfUnicodeStrMap): Boolean;
    procedure PutAll(const AMap: IJclIntfUnicodeStrMap);
    procedure PutValue(const Key: IInterface; const Value: UnicodeString);
    function Remove(const Key: IInterface): UnicodeString;
    function Size: Integer;
    function Values: IJclUnicodeStrCollection;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrUnicodeStrHashMapEntry = record
    Key: UnicodeString;
    Value: UnicodeString;
  end;

  TJclUnicodeStrUnicodeStrHashMapEntryArray = array of TJclUnicodeStrUnicodeStrHashMapEntry;

  TJclUnicodeStrUnicodeStrHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclUnicodeStrUnicodeStrHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclUnicodeStrUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclUnicodeStrUnicodeStrHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclUnicodeStrUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclUnicodeStrUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrUnicodeStrHashMap = class(TJclUnicodeStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclStrBaseContainer, IJclUnicodeStrContainer,
    IJclUnicodeStrUnicodeStrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: UnicodeString): UnicodeString;
    function FreeValue(var Value: UnicodeString): UnicodeString;
    function KeysEqual(const A, B: UnicodeString): Boolean;
    function ValuesEqual(const A, B: UnicodeString): Boolean;
  private
    FBuckets: array of TJclUnicodeStrUnicodeStrHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclUnicodeStrUnicodeStrMap }
    procedure Clear;
    function ContainsKey(const Key: UnicodeString): Boolean;
    function ContainsValue(const Value: UnicodeString): Boolean;
    function Extract(const Key: UnicodeString): UnicodeString;
    function GetValue(const Key: UnicodeString): UnicodeString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: UnicodeString): UnicodeString;
    function KeySet: IJclUnicodeStrSet;
    function Keys: IJclUnicodeStrCollection;
    function MapEquals(const AMap: IJclUnicodeStrUnicodeStrMap): Boolean;
    procedure PutAll(const AMap: IJclUnicodeStrUnicodeStrMap);
    procedure PutValue(const Key: UnicodeString; const Value: UnicodeString);
    function Remove(const Key: UnicodeString): UnicodeString;
    function Size: Integer;
    function Values: IJclUnicodeStrCollection;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrIntfHashMapEntry = TJclAnsiStrIntfHashMapEntry;
  TJclStrIntfHashMapBucket = TJclAnsiStrIntfHashMapBucket;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrIntfHashMapEntry = TJclWideStrIntfHashMapEntry;
  TJclStrIntfHashMapBucket = TJclWideStrIntfHashMapBucket;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrIntfHashMapEntry = TJclUnicodeStrIntfHashMapEntry;
  TJclStrIntfHashMapBucket = TJclUnicodeStrIntfHashMapBucket;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrIntfHashMap = TJclAnsiStrIntfHashMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrIntfHashMap = TJclWideStrIntfHashMap;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrIntfHashMap = TJclUnicodeStrIntfHashMap;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  TJclIntfStrHashMapEntry = TJclIntfAnsiStrHashMapEntry;
  TJclIntfStrHashMapBucket = TJclIntfAnsiStrHashMapBucket;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclIntfStrHashMapEntry = TJclIntfWideStrHashMapEntry;
  TJclIntfStrHashMapBucket = TJclIntfWideStrHashMapBucket;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclIntfStrHashMapEntry = TJclIntfUnicodeStrHashMapEntry;
  TJclIntfStrHashMapBucket = TJclIntfUnicodeStrHashMapBucket;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  TJclIntfStrHashMap = TJclIntfAnsiStrHashMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclIntfStrHashMap = TJclIntfWideStrHashMap;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclIntfStrHashMap = TJclIntfUnicodeStrHashMap;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrStrHashMapEntry = TJclAnsiStrAnsiStrHashMapEntry;
  TJclStrStrHashMapBucket = TJclAnsiStrAnsiStrHashMapBucket;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrStrHashMapEntry = TJclWideStrWideStrHashMapEntry;
  TJclStrStrHashMapBucket = TJclWideStrWideStrHashMapBucket;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrStrHashMapEntry = TJclUnicodeStrUnicodeStrHashMapEntry;
  TJclStrStrHashMapBucket = TJclUnicodeStrUnicodeStrHashMapBucket;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrStrHashMap = TJclAnsiStrAnsiStrHashMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrStrHashMap = TJclWideStrWideStrHashMap;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrStrHashMap = TJclUnicodeStrUnicodeStrHashMap;
  {$ENDIF CONTAINER_UNICODESTR}

  TJclSingleIntfHashMapEntry = record
    Key: Single;
    Value: IInterface;
  end;

  TJclSingleIntfHashMapEntryArray = array of TJclSingleIntfHashMapEntry;

  TJclSingleIntfHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclSingleIntfHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclSingleIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclSingleIntfHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclSingleIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclSingleIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclSingleIntfHashMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclSingleContainer, IJclIntfContainer,
    IJclSingleIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: Single): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclSingleIntfHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclSingleIntfMap }
    procedure Clear;
    function ContainsKey(const Key: Single): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: Single): IInterface;
    function GetValue(const Key: Single): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Single;
    function KeySet: IJclSingleSet;
    function Keys: IJclSingleCollection;
    function MapEquals(const AMap: IJclSingleIntfMap): Boolean;
    procedure PutAll(const AMap: IJclSingleIntfMap);
    procedure PutValue(const Key: Single; const Value: IInterface);
    function Remove(const Key: Single): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  end;

  TJclIntfSingleHashMapEntry = record
    Key: IInterface;
    Value: Single;
  end;

  TJclIntfSingleHashMapEntryArray = array of TJclIntfSingleHashMapEntry;

  TJclIntfSingleHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclIntfSingleHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclIntfSingleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclIntfSingleHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclIntfSingleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclIntfSingleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclIntfSingleHashMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclIntfContainer, IJclSingleContainer,
    IJclIntfSingleMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Single): Single;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: Single): Boolean;
  private
    FBuckets: array of TJclIntfSingleHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfSingleMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Single): Boolean;
    function Extract(const Key: IInterface): Single;
    function GetValue(const Key: IInterface): Single;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Single): IInterface;
    function KeySet: IJclIntfSet;
    function Keys: IJclIntfCollection;
    function MapEquals(const AMap: IJclIntfSingleMap): Boolean;
    procedure PutAll(const AMap: IJclIntfSingleMap);
    procedure PutValue(const Key: IInterface; const Value: Single);
    function Remove(const Key: IInterface): Single;
    function Size: Integer;
    function Values: IJclSingleCollection;
  end;

  TJclSingleSingleHashMapEntry = record
    Key: Single;
    Value: Single;
  end;

  TJclSingleSingleHashMapEntryArray = array of TJclSingleSingleHashMapEntry;

  TJclSingleSingleHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclSingleSingleHashMapEntryArray;
    procedure InitializeArrayAfterMove(var List: TJclSingleSingleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure MoveArray(var List: TJclSingleSingleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclSingleSingleHashMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclSingleContainer,
    IJclSingleSingleMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: Single): Single;
    function KeysEqual(const A, B: Single): Boolean;
    function ValuesEqual(const A, B: Single): Boolean;
  private
    FBuckets: array of TJclSingleSingleHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclSingleSingleMap }
    procedure Clear;
    function ContainsKey(const Key: Single): Boolean;
    function ContainsValue(const Value: Single): Boolean;
    function Extract(const Key: Single): Single;
    function GetValue(const Key: Single): Single;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Single): Single;
    function KeySet: IJclSingleSet;
    function Keys: IJclSingleCollection;
    function MapEquals(const AMap: IJclSingleSingleMap): Boolean;
    procedure PutAll(const AMap: IJclSingleSingleMap);
    procedure PutValue(const Key: Single; const Value: Single);
    function Remove(const Key: Single): Single;
    function Size: Integer;
    function Values: IJclSingleCollection;
  end;

  TJclDoubleIntfHashMapEntry = record
    Key: Double;
    Value: IInterface;
  end;

  TJclDoubleIntfHashMapEntryArray = array of TJclDoubleIntfHashMapEntry;

  TJclDoubleIntfHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclDoubleIntfHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclDoubleIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclDoubleIntfHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclDoubleIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclDoubleIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclDoubleIntfHashMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclDoubleContainer, IJclIntfContainer,
    IJclDoubleIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: Double): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclDoubleIntfHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclDoubleIntfMap }
    procedure Clear;
    function ContainsKey(const Key: Double): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: Double): IInterface;
    function GetValue(const Key: Double): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Double;
    function KeySet: IJclDoubleSet;
    function Keys: IJclDoubleCollection;
    function MapEquals(const AMap: IJclDoubleIntfMap): Boolean;
    procedure PutAll(const AMap: IJclDoubleIntfMap);
    procedure PutValue(const Key: Double; const Value: IInterface);
    function Remove(const Key: Double): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  end;

  TJclIntfDoubleHashMapEntry = record
    Key: IInterface;
    Value: Double;
  end;

  TJclIntfDoubleHashMapEntryArray = array of TJclIntfDoubleHashMapEntry;

  TJclIntfDoubleHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclIntfDoubleHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclIntfDoubleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclIntfDoubleHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclIntfDoubleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclIntfDoubleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclIntfDoubleHashMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclIntfContainer, IJclDoubleContainer,
    IJclIntfDoubleMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Double): Double;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: Double): Boolean;
  private
    FBuckets: array of TJclIntfDoubleHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfDoubleMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Double): Boolean;
    function Extract(const Key: IInterface): Double;
    function GetValue(const Key: IInterface): Double;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Double): IInterface;
    function KeySet: IJclIntfSet;
    function Keys: IJclIntfCollection;
    function MapEquals(const AMap: IJclIntfDoubleMap): Boolean;
    procedure PutAll(const AMap: IJclIntfDoubleMap);
    procedure PutValue(const Key: IInterface; const Value: Double);
    function Remove(const Key: IInterface): Double;
    function Size: Integer;
    function Values: IJclDoubleCollection;
  end;

  TJclDoubleDoubleHashMapEntry = record
    Key: Double;
    Value: Double;
  end;

  TJclDoubleDoubleHashMapEntryArray = array of TJclDoubleDoubleHashMapEntry;

  TJclDoubleDoubleHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclDoubleDoubleHashMapEntryArray;
    procedure InitializeArrayAfterMove(var List: TJclDoubleDoubleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure MoveArray(var List: TJclDoubleDoubleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclDoubleDoubleHashMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclDoubleContainer,
    IJclDoubleDoubleMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: Double): Double;
    function KeysEqual(const A, B: Double): Boolean;
    function ValuesEqual(const A, B: Double): Boolean;
  private
    FBuckets: array of TJclDoubleDoubleHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclDoubleDoubleMap }
    procedure Clear;
    function ContainsKey(const Key: Double): Boolean;
    function ContainsValue(const Value: Double): Boolean;
    function Extract(const Key: Double): Double;
    function GetValue(const Key: Double): Double;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Double): Double;
    function KeySet: IJclDoubleSet;
    function Keys: IJclDoubleCollection;
    function MapEquals(const AMap: IJclDoubleDoubleMap): Boolean;
    procedure PutAll(const AMap: IJclDoubleDoubleMap);
    procedure PutValue(const Key: Double; const Value: Double);
    function Remove(const Key: Double): Double;
    function Size: Integer;
    function Values: IJclDoubleCollection;
  end;

  TJclExtendedIntfHashMapEntry = record
    Key: Extended;
    Value: IInterface;
  end;

  TJclExtendedIntfHashMapEntryArray = array of TJclExtendedIntfHashMapEntry;

  TJclExtendedIntfHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclExtendedIntfHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclExtendedIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclExtendedIntfHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclExtendedIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclExtendedIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclExtendedIntfHashMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclExtendedContainer, IJclIntfContainer,
    IJclExtendedIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: Extended): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclExtendedIntfHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclExtendedIntfMap }
    procedure Clear;
    function ContainsKey(const Key: Extended): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: Extended): IInterface;
    function GetValue(const Key: Extended): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Extended;
    function KeySet: IJclExtendedSet;
    function Keys: IJclExtendedCollection;
    function MapEquals(const AMap: IJclExtendedIntfMap): Boolean;
    procedure PutAll(const AMap: IJclExtendedIntfMap);
    procedure PutValue(const Key: Extended; const Value: IInterface);
    function Remove(const Key: Extended): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  end;

  TJclIntfExtendedHashMapEntry = record
    Key: IInterface;
    Value: Extended;
  end;

  TJclIntfExtendedHashMapEntryArray = array of TJclIntfExtendedHashMapEntry;

  TJclIntfExtendedHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclIntfExtendedHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclIntfExtendedHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclIntfExtendedHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclIntfExtendedHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclIntfExtendedHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclIntfExtendedHashMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclIntfContainer, IJclExtendedContainer,
    IJclIntfExtendedMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Extended): Extended;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: Extended): Boolean;
  private
    FBuckets: array of TJclIntfExtendedHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfExtendedMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Extended): Boolean;
    function Extract(const Key: IInterface): Extended;
    function GetValue(const Key: IInterface): Extended;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Extended): IInterface;
    function KeySet: IJclIntfSet;
    function Keys: IJclIntfCollection;
    function MapEquals(const AMap: IJclIntfExtendedMap): Boolean;
    procedure PutAll(const AMap: IJclIntfExtendedMap);
    procedure PutValue(const Key: IInterface; const Value: Extended);
    function Remove(const Key: IInterface): Extended;
    function Size: Integer;
    function Values: IJclExtendedCollection;
  end;

  TJclExtendedExtendedHashMapEntry = record
    Key: Extended;
    Value: Extended;
  end;

  TJclExtendedExtendedHashMapEntryArray = array of TJclExtendedExtendedHashMapEntry;

  TJclExtendedExtendedHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclExtendedExtendedHashMapEntryArray;
    procedure InitializeArrayAfterMove(var List: TJclExtendedExtendedHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure MoveArray(var List: TJclExtendedExtendedHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclExtendedExtendedHashMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclExtendedContainer,
    IJclExtendedExtendedMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: Extended): Extended;
    function KeysEqual(const A, B: Extended): Boolean;
    function ValuesEqual(const A, B: Extended): Boolean;
  private
    FBuckets: array of TJclExtendedExtendedHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclExtendedExtendedMap }
    procedure Clear;
    function ContainsKey(const Key: Extended): Boolean;
    function ContainsValue(const Value: Extended): Boolean;
    function Extract(const Key: Extended): Extended;
    function GetValue(const Key: Extended): Extended;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Extended): Extended;
    function KeySet: IJclExtendedSet;
    function Keys: IJclExtendedCollection;
    function MapEquals(const AMap: IJclExtendedExtendedMap): Boolean;
    procedure PutAll(const AMap: IJclExtendedExtendedMap);
    procedure PutValue(const Key: Extended; const Value: Extended);
    function Remove(const Key: Extended): Extended;
    function Size: Integer;
    function Values: IJclExtendedCollection;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatIntfHashMapEntry = TJclSingleIntfHashMapEntry;
  TJclFloatIntfHashMapBucket = TJclSingleIntfHashMapBucket;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatIntfHashMapEntry = TJclDoubleIntfHashMapEntry;
  TJclFloatIntfHashMapBucket = TJclDoubleIntfHashMapBucket;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatIntfHashMapEntry = TJclExtendedIntfHashMapEntry;
  TJclFloatIntfHashMapBucket = TJclExtendedIntfHashMapBucket;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatIntfHashMap = TJclSingleIntfHashMap;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatIntfHashMap = TJclDoubleIntfHashMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatIntfHashMap = TJclExtendedIntfHashMap;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclIntfFloatHashMapEntry = TJclIntfSingleHashMapEntry;
  TJclIntfFloatHashMapBucket = TJclIntfSingleHashMapBucket;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclIntfFloatHashMapEntry = TJclIntfDoubleHashMapEntry;
  TJclIntfFloatHashMapBucket = TJclIntfDoubleHashMapBucket;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclIntfFloatHashMapEntry = TJclIntfExtendedHashMapEntry;
  TJclIntfFloatHashMapBucket = TJclIntfExtendedHashMapBucket;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclIntfFloatHashMap = TJclIntfSingleHashMap;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclIntfFloatHashMap = TJclIntfDoubleHashMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclIntfFloatHashMap = TJclIntfExtendedHashMap;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatFloatHashMapEntry = TJclSingleSingleHashMapEntry;
  TJclFloatFloatHashMapBucket = TJclSingleSingleHashMapBucket;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatFloatHashMapEntry = TJclDoubleDoubleHashMapEntry;
  TJclFloatFloatHashMapBucket = TJclDoubleDoubleHashMapBucket;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatFloatHashMapEntry = TJclExtendedExtendedHashMapEntry;
  TJclFloatFloatHashMapBucket = TJclExtendedExtendedHashMapBucket;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatFloatHashMap = TJclSingleSingleHashMap;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatFloatHashMap = TJclDoubleDoubleHashMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatFloatHashMap = TJclExtendedExtendedHashMap;
  {$ENDIF MATH_EXTENDED_PRECISION}

  TJclIntegerIntfHashMapEntry = record
    Key: Integer;
    Value: IInterface;
  end;

  TJclIntegerIntfHashMapEntryArray = array of TJclIntegerIntfHashMapEntry;

  TJclIntegerIntfHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclIntegerIntfHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclIntegerIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclIntegerIntfHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclIntegerIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclIntegerIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclIntegerIntfHashMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclIntegerContainer, IJclIntfContainer,
    IJclIntegerIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(A, B: Integer): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclIntegerIntfHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntegerIntfMap }
    procedure Clear;
    function ContainsKey(Key: Integer): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(Key: Integer): IInterface;
    function GetValue(Key: Integer): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Integer;
    function KeySet: IJclIntegerSet;
    function Keys: IJclIntegerCollection;
    function MapEquals(const AMap: IJclIntegerIntfMap): Boolean;
    procedure PutAll(const AMap: IJclIntegerIntfMap);
    procedure PutValue(Key: Integer; const Value: IInterface);
    function Remove(Key: Integer): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  end;

  TJclIntfIntegerHashMapEntry = record
    Key: IInterface;
    Value: Integer;
  end;

  TJclIntfIntegerHashMapEntryArray = array of TJclIntfIntegerHashMapEntry;

  TJclIntfIntegerHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclIntfIntegerHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclIntfIntegerHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclIntfIntegerHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclIntfIntegerHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclIntfIntegerHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclIntfIntegerHashMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclIntfContainer, IJclIntegerContainer,
    IJclIntfIntegerMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Integer): Integer;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(A, B: Integer): Boolean;
  private
    FBuckets: array of TJclIntfIntegerHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfIntegerMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: Integer): Boolean;
    function Extract(const Key: IInterface): Integer;
    function GetValue(const Key: IInterface): Integer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Integer): IInterface;
    function KeySet: IJclIntfSet;
    function Keys: IJclIntfCollection;
    function MapEquals(const AMap: IJclIntfIntegerMap): Boolean;
    procedure PutAll(const AMap: IJclIntfIntegerMap);
    procedure PutValue(const Key: IInterface; Value: Integer);
    function Remove(const Key: IInterface): Integer;
    function Size: Integer;
    function Values: IJclIntegerCollection;
  end;

  TJclIntegerIntegerHashMapEntry = record
    Key: Integer;
    Value: Integer;
  end;

  TJclIntegerIntegerHashMapEntryArray = array of TJclIntegerIntegerHashMapEntry;

  TJclIntegerIntegerHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclIntegerIntegerHashMapEntryArray;
    procedure InitializeArrayAfterMove(var List: TJclIntegerIntegerHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure MoveArray(var List: TJclIntegerIntegerHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclIntegerIntegerHashMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclIntegerContainer,
    IJclIntegerIntegerMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: Integer): Integer;
    function KeysEqual(A, B: Integer): Boolean;
    function ValuesEqual(A, B: Integer): Boolean;
  private
    FBuckets: array of TJclIntegerIntegerHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntegerIntegerMap }
    procedure Clear;
    function ContainsKey(Key: Integer): Boolean;
    function ContainsValue(Value: Integer): Boolean;
    function Extract(Key: Integer): Integer;
    function GetValue(Key: Integer): Integer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Integer): Integer;
    function KeySet: IJclIntegerSet;
    function Keys: IJclIntegerCollection;
    function MapEquals(const AMap: IJclIntegerIntegerMap): Boolean;
    procedure PutAll(const AMap: IJclIntegerIntegerMap);
    procedure PutValue(Key: Integer; Value: Integer);
    function Remove(Key: Integer): Integer;
    function Size: Integer;
    function Values: IJclIntegerCollection;
  end;

  TJclCardinalIntfHashMapEntry = record
    Key: Cardinal;
    Value: IInterface;
  end;

  TJclCardinalIntfHashMapEntryArray = array of TJclCardinalIntfHashMapEntry;

  TJclCardinalIntfHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclCardinalIntfHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclCardinalIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclCardinalIntfHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclCardinalIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclCardinalIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclCardinalIntfHashMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclCardinalContainer, IJclIntfContainer,
    IJclCardinalIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(A, B: Cardinal): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclCardinalIntfHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCardinalIntfMap }
    procedure Clear;
    function ContainsKey(Key: Cardinal): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(Key: Cardinal): IInterface;
    function GetValue(Key: Cardinal): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Cardinal;
    function KeySet: IJclCardinalSet;
    function Keys: IJclCardinalCollection;
    function MapEquals(const AMap: IJclCardinalIntfMap): Boolean;
    procedure PutAll(const AMap: IJclCardinalIntfMap);
    procedure PutValue(Key: Cardinal; const Value: IInterface);
    function Remove(Key: Cardinal): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  end;

  TJclIntfCardinalHashMapEntry = record
    Key: IInterface;
    Value: Cardinal;
  end;

  TJclIntfCardinalHashMapEntryArray = array of TJclIntfCardinalHashMapEntry;

  TJclIntfCardinalHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclIntfCardinalHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclIntfCardinalHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclIntfCardinalHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclIntfCardinalHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclIntfCardinalHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclIntfCardinalHashMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclIntfContainer, IJclCardinalContainer,
    IJclIntfCardinalMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Cardinal): Cardinal;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(A, B: Cardinal): Boolean;
  private
    FBuckets: array of TJclIntfCardinalHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCardinalMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: Cardinal): Boolean;
    function Extract(const Key: IInterface): Cardinal;
    function GetValue(const Key: IInterface): Cardinal;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Cardinal): IInterface;
    function KeySet: IJclIntfSet;
    function Keys: IJclIntfCollection;
    function MapEquals(const AMap: IJclIntfCardinalMap): Boolean;
    procedure PutAll(const AMap: IJclIntfCardinalMap);
    procedure PutValue(const Key: IInterface; Value: Cardinal);
    function Remove(const Key: IInterface): Cardinal;
    function Size: Integer;
    function Values: IJclCardinalCollection;
  end;

  TJclCardinalCardinalHashMapEntry = record
    Key: Cardinal;
    Value: Cardinal;
  end;

  TJclCardinalCardinalHashMapEntryArray = array of TJclCardinalCardinalHashMapEntry;

  TJclCardinalCardinalHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclCardinalCardinalHashMapEntryArray;
    procedure InitializeArrayAfterMove(var List: TJclCardinalCardinalHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure MoveArray(var List: TJclCardinalCardinalHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclCardinalCardinalHashMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclCardinalContainer,
    IJclCardinalCardinalMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: Cardinal): Cardinal;
    function KeysEqual(A, B: Cardinal): Boolean;
    function ValuesEqual(A, B: Cardinal): Boolean;
  private
    FBuckets: array of TJclCardinalCardinalHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCardinalCardinalMap }
    procedure Clear;
    function ContainsKey(Key: Cardinal): Boolean;
    function ContainsValue(Value: Cardinal): Boolean;
    function Extract(Key: Cardinal): Cardinal;
    function GetValue(Key: Cardinal): Cardinal;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Cardinal): Cardinal;
    function KeySet: IJclCardinalSet;
    function Keys: IJclCardinalCollection;
    function MapEquals(const AMap: IJclCardinalCardinalMap): Boolean;
    procedure PutAll(const AMap: IJclCardinalCardinalMap);
    procedure PutValue(Key: Cardinal; Value: Cardinal);
    function Remove(Key: Cardinal): Cardinal;
    function Size: Integer;
    function Values: IJclCardinalCollection;
  end;

  TJclInt64IntfHashMapEntry = record
    Key: Int64;
    Value: IInterface;
  end;

  TJclInt64IntfHashMapEntryArray = array of TJclInt64IntfHashMapEntry;

  TJclInt64IntfHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclInt64IntfHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclInt64IntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclInt64IntfHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclInt64IntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclInt64IntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclInt64IntfHashMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclInt64Container, IJclIntfContainer,
    IJclInt64IntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: Int64): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclInt64IntfHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclInt64IntfMap }
    procedure Clear;
    function ContainsKey(const Key: Int64): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: Int64): IInterface;
    function GetValue(const Key: Int64): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Int64;
    function KeySet: IJclInt64Set;
    function Keys: IJclInt64Collection;
    function MapEquals(const AMap: IJclInt64IntfMap): Boolean;
    procedure PutAll(const AMap: IJclInt64IntfMap);
    procedure PutValue(const Key: Int64; const Value: IInterface);
    function Remove(const Key: Int64): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  end;

  TJclIntfInt64HashMapEntry = record
    Key: IInterface;
    Value: Int64;
  end;

  TJclIntfInt64HashMapEntryArray = array of TJclIntfInt64HashMapEntry;

  TJclIntfInt64HashMapBucket = class
  public
    Size: Integer;
    Entries: TJclIntfInt64HashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclIntfInt64HashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclIntfInt64HashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclIntfInt64HashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclIntfInt64HashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclIntfInt64HashMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclIntfContainer, IJclInt64Container,
    IJclIntfInt64Map)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Int64): Int64;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: Int64): Boolean;
  private
    FBuckets: array of TJclIntfInt64HashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfInt64Map }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Int64): Boolean;
    function Extract(const Key: IInterface): Int64;
    function GetValue(const Key: IInterface): Int64;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Int64): IInterface;
    function KeySet: IJclIntfSet;
    function Keys: IJclIntfCollection;
    function MapEquals(const AMap: IJclIntfInt64Map): Boolean;
    procedure PutAll(const AMap: IJclIntfInt64Map);
    procedure PutValue(const Key: IInterface; const Value: Int64);
    function Remove(const Key: IInterface): Int64;
    function Size: Integer;
    function Values: IJclInt64Collection;
  end;

  TJclInt64Int64HashMapEntry = record
    Key: Int64;
    Value: Int64;
  end;

  TJclInt64Int64HashMapEntryArray = array of TJclInt64Int64HashMapEntry;

  TJclInt64Int64HashMapBucket = class
  public
    Size: Integer;
    Entries: TJclInt64Int64HashMapEntryArray;
    procedure InitializeArrayAfterMove(var List: TJclInt64Int64HashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure MoveArray(var List: TJclInt64Int64HashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclInt64Int64HashMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclInt64Container,
    IJclInt64Int64Map)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: Int64): Int64;
    function KeysEqual(const A, B: Int64): Boolean;
    function ValuesEqual(const A, B: Int64): Boolean;
  private
    FBuckets: array of TJclInt64Int64HashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclInt64Int64Map }
    procedure Clear;
    function ContainsKey(const Key: Int64): Boolean;
    function ContainsValue(const Value: Int64): Boolean;
    function Extract(const Key: Int64): Int64;
    function GetValue(const Key: Int64): Int64;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Int64): Int64;
    function KeySet: IJclInt64Set;
    function Keys: IJclInt64Collection;
    function MapEquals(const AMap: IJclInt64Int64Map): Boolean;
    procedure PutAll(const AMap: IJclInt64Int64Map);
    procedure PutValue(const Key: Int64; const Value: Int64);
    function Remove(const Key: Int64): Int64;
    function Size: Integer;
    function Values: IJclInt64Collection;
  end;

  TJclPtrIntfHashMapEntry = record
    Key: Pointer;
    Value: IInterface;
  end;

  TJclPtrIntfHashMapEntryArray = array of TJclPtrIntfHashMapEntry;

  TJclPtrIntfHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclPtrIntfHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclPtrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclPtrIntfHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclPtrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclPtrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclPtrIntfHashMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclPtrContainer, IJclIntfContainer,
    IJclPtrIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(A, B: Pointer): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclPtrIntfHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclPtrIntfMap }
    procedure Clear;
    function ContainsKey(Key: Pointer): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(Key: Pointer): IInterface;
    function GetValue(Key: Pointer): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Pointer;
    function KeySet: IJclPtrSet;
    function Keys: IJclPtrCollection;
    function MapEquals(const AMap: IJclPtrIntfMap): Boolean;
    procedure PutAll(const AMap: IJclPtrIntfMap);
    procedure PutValue(Key: Pointer; const Value: IInterface);
    function Remove(Key: Pointer): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  end;

  TJclIntfPtrHashMapEntry = record
    Key: IInterface;
    Value: Pointer;
  end;

  TJclIntfPtrHashMapEntryArray = array of TJclIntfPtrHashMapEntry;

  TJclIntfPtrHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclIntfPtrHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclIntfPtrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclIntfPtrHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclIntfPtrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclIntfPtrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclIntfPtrHashMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclIntfContainer, IJclPtrContainer,
    IJclIntfPtrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Pointer): Pointer;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(A, B: Pointer): Boolean;
  private
    FBuckets: array of TJclIntfPtrHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfPtrMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: Pointer): Boolean;
    function Extract(const Key: IInterface): Pointer;
    function GetValue(const Key: IInterface): Pointer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Pointer): IInterface;
    function KeySet: IJclIntfSet;
    function Keys: IJclIntfCollection;
    function MapEquals(const AMap: IJclIntfPtrMap): Boolean;
    procedure PutAll(const AMap: IJclIntfPtrMap);
    procedure PutValue(const Key: IInterface; Value: Pointer);
    function Remove(const Key: IInterface): Pointer;
    function Size: Integer;
    function Values: IJclPtrCollection;
  end;

  TJclPtrPtrHashMapEntry = record
    Key: Pointer;
    Value: Pointer;
  end;

  TJclPtrPtrHashMapEntryArray = array of TJclPtrPtrHashMapEntry;

  TJclPtrPtrHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclPtrPtrHashMapEntryArray;
    procedure InitializeArrayAfterMove(var List: TJclPtrPtrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure MoveArray(var List: TJclPtrPtrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclPtrPtrHashMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclPtrContainer,
    IJclPtrPtrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: Pointer): Pointer;
    function KeysEqual(A, B: Pointer): Boolean;
    function ValuesEqual(A, B: Pointer): Boolean;
  private
    FBuckets: array of TJclPtrPtrHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclPtrPtrMap }
    procedure Clear;
    function ContainsKey(Key: Pointer): Boolean;
    function ContainsValue(Value: Pointer): Boolean;
    function Extract(Key: Pointer): Pointer;
    function GetValue(Key: Pointer): Pointer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Pointer): Pointer;
    function KeySet: IJclPtrSet;
    function Keys: IJclPtrCollection;
    function MapEquals(const AMap: IJclPtrPtrMap): Boolean;
    procedure PutAll(const AMap: IJclPtrPtrMap);
    procedure PutValue(Key: Pointer; Value: Pointer);
    function Remove(Key: Pointer): Pointer;
    function Size: Integer;
    function Values: IJclPtrCollection;
  end;

  TJclIntfHashMapEntry = record
    Key: IInterface;
    Value: TObject;
  end;

  TJclIntfHashMapEntryArray = array of TJclIntfHashMapEntry;

  TJclIntfHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclIntfHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclIntfHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclIntfHashMap = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclIntfContainer, IJclContainer, IJclValueOwner,
    IJclIntfMap)
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclIntfHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: IInterface): TObject;
    function GetValue(const Key: IInterface): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): IInterface;
    function KeySet: IJclIntfSet;
    function Keys: IJclIntfCollection;
    function MapEquals(const AMap: IJclIntfMap): Boolean;
    procedure PutAll(const AMap: IJclIntfMap);
    procedure PutValue(const Key: IInterface; Value: TObject);
    function Remove(const Key: IInterface): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  end;

  TJclAnsiStrHashMapEntry = record
    Key: AnsiString;
    Value: TObject;
  end;

  TJclAnsiStrHashMapEntryArray = array of TJclAnsiStrHashMapEntry;

  TJclAnsiStrHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclAnsiStrHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclAnsiStrHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclAnsiStrHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclStrBaseContainer, IJclAnsiStrContainer, IJclContainer, IJclValueOwner,
    IJclAnsiStrMap)
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function KeysEqual(const A, B: AnsiString): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclAnsiStrHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrMap }
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: AnsiString): TObject;
    function GetValue(const Key: AnsiString): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): AnsiString;
    function KeySet: IJclAnsiStrSet;
    function Keys: IJclAnsiStrCollection;
    function MapEquals(const AMap: IJclAnsiStrMap): Boolean;
    procedure PutAll(const AMap: IJclAnsiStrMap);
    procedure PutValue(const Key: AnsiString; Value: TObject);
    function Remove(const Key: AnsiString): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  end;

  TJclWideStrHashMapEntry = record
    Key: WideString;
    Value: TObject;
  end;

  TJclWideStrHashMapEntryArray = array of TJclWideStrHashMapEntry;

  TJclWideStrHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclWideStrHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclWideStrHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclWideStrHashMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclStrBaseContainer, IJclWideStrContainer, IJclContainer, IJclValueOwner,
    IJclWideStrMap)
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function KeysEqual(const A, B: WideString): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclWideStrHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrMap }
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: WideString): TObject;
    function GetValue(const Key: WideString): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): WideString;
    function KeySet: IJclWideStrSet;
    function Keys: IJclWideStrCollection;
    function MapEquals(const AMap: IJclWideStrMap): Boolean;
    procedure PutAll(const AMap: IJclWideStrMap);
    procedure PutValue(const Key: WideString; Value: TObject);
    function Remove(const Key: WideString): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrHashMapEntry = record
    Key: UnicodeString;
    Value: TObject;
  end;

  TJclUnicodeStrHashMapEntryArray = array of TJclUnicodeStrHashMapEntry;

  TJclUnicodeStrHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclUnicodeStrHashMapEntryArray;
    procedure FinalizeArrayBeforeMove(var List: TJclUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: TJclUnicodeStrHashMapEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: TJclUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: TJclUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrHashMap = class(TJclUnicodeStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclStrBaseContainer, IJclUnicodeStrContainer, IJclContainer, IJclValueOwner,
    IJclUnicodeStrMap)
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: UnicodeString): UnicodeString;
    function KeysEqual(const A, B: UnicodeString): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclUnicodeStrHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclUnicodeStrMap }
    procedure Clear;
    function ContainsKey(const Key: UnicodeString): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: UnicodeString): TObject;
    function GetValue(const Key: UnicodeString): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): UnicodeString;
    function KeySet: IJclUnicodeStrSet;
    function Keys: IJclUnicodeStrCollection;
    function MapEquals(const AMap: IJclUnicodeStrMap): Boolean;
    procedure PutAll(const AMap: IJclUnicodeStrMap);
    procedure PutValue(const Key: UnicodeString; Value: TObject);
    function Remove(const Key: UnicodeString): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrHashMapEntry = TJclAnsiStrHashMapEntry;
  TJclStrHashMapBucket = TJclAnsiStrHashMapBucket;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrHashMapEntry = TJclWideStrHashMapEntry;
  TJclStrHashMapBucket = TJclWideStrHashMapBucket;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrHashMapEntry = TJclUnicodeStrHashMapEntry;
  TJclStrHashMapBucket = TJclUnicodeStrHashMapBucket;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrHashMap = TJclAnsiStrHashMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrHashMap = TJclWideStrHashMap;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrHashMap = TJclUnicodeStrHashMap;
  {$ENDIF CONTAINER_UNICODESTR}

  TJclSingleHashMapEntry = record
    Key: Single;
    Value: TObject;
  end;

  TJclSingleHashMapEntryArray = array of TJclSingleHashMapEntry;

  TJclSingleHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclSingleHashMapEntryArray;
    procedure InitializeArrayAfterMove(var List: TJclSingleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure MoveArray(var List: TJclSingleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclSingleHashMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclSingleContainer, IJclContainer, IJclValueOwner,
    IJclSingleMap)
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function KeysEqual(const A, B: Single): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclSingleHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclSingleMap }
    procedure Clear;
    function ContainsKey(const Key: Single): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: Single): TObject;
    function GetValue(const Key: Single): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Single;
    function KeySet: IJclSingleSet;
    function Keys: IJclSingleCollection;
    function MapEquals(const AMap: IJclSingleMap): Boolean;
    procedure PutAll(const AMap: IJclSingleMap);
    procedure PutValue(const Key: Single; Value: TObject);
    function Remove(const Key: Single): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  end;

  TJclDoubleHashMapEntry = record
    Key: Double;
    Value: TObject;
  end;

  TJclDoubleHashMapEntryArray = array of TJclDoubleHashMapEntry;

  TJclDoubleHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclDoubleHashMapEntryArray;
    procedure InitializeArrayAfterMove(var List: TJclDoubleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure MoveArray(var List: TJclDoubleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclDoubleHashMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclDoubleContainer, IJclContainer, IJclValueOwner,
    IJclDoubleMap)
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function KeysEqual(const A, B: Double): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclDoubleHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclDoubleMap }
    procedure Clear;
    function ContainsKey(const Key: Double): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: Double): TObject;
    function GetValue(const Key: Double): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Double;
    function KeySet: IJclDoubleSet;
    function Keys: IJclDoubleCollection;
    function MapEquals(const AMap: IJclDoubleMap): Boolean;
    procedure PutAll(const AMap: IJclDoubleMap);
    procedure PutValue(const Key: Double; Value: TObject);
    function Remove(const Key: Double): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  end;

  TJclExtendedHashMapEntry = record
    Key: Extended;
    Value: TObject;
  end;

  TJclExtendedHashMapEntryArray = array of TJclExtendedHashMapEntry;

  TJclExtendedHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclExtendedHashMapEntryArray;
    procedure InitializeArrayAfterMove(var List: TJclExtendedHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure MoveArray(var List: TJclExtendedHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclExtendedHashMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclExtendedContainer, IJclContainer, IJclValueOwner,
    IJclExtendedMap)
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function KeysEqual(const A, B: Extended): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclExtendedHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclExtendedMap }
    procedure Clear;
    function ContainsKey(const Key: Extended): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: Extended): TObject;
    function GetValue(const Key: Extended): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Extended;
    function KeySet: IJclExtendedSet;
    function Keys: IJclExtendedCollection;
    function MapEquals(const AMap: IJclExtendedMap): Boolean;
    procedure PutAll(const AMap: IJclExtendedMap);
    procedure PutValue(const Key: Extended; Value: TObject);
    function Remove(const Key: Extended): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatHashMapEntry = TJclSingleHashMapEntry;
  TJclFloatHashMapBucket = TJclSingleHashMapBucket;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatHashMapEntry = TJclDoubleHashMapEntry;
  TJclFloatHashMapBucket = TJclDoubleHashMapBucket;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatHashMapEntry = TJclExtendedHashMapEntry;
  TJclFloatHashMapBucket = TJclExtendedHashMapBucket;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatHashMap = TJclSingleHashMap;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatHashMap = TJclDoubleHashMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatHashMap = TJclExtendedHashMap;
  {$ENDIF MATH_EXTENDED_PRECISION}

  TJclIntegerHashMapEntry = record
    Key: Integer;
    Value: TObject;
  end;

  TJclIntegerHashMapEntryArray = array of TJclIntegerHashMapEntry;

  TJclIntegerHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclIntegerHashMapEntryArray;
    procedure InitializeArrayAfterMove(var List: TJclIntegerHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure MoveArray(var List: TJclIntegerHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclIntegerHashMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclIntegerContainer, IJclContainer, IJclValueOwner,
    IJclIntegerMap)
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function KeysEqual(A, B: Integer): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclIntegerHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntegerMap }
    procedure Clear;
    function ContainsKey(Key: Integer): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(Key: Integer): TObject;
    function GetValue(Key: Integer): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Integer;
    function KeySet: IJclIntegerSet;
    function Keys: IJclIntegerCollection;
    function MapEquals(const AMap: IJclIntegerMap): Boolean;
    procedure PutAll(const AMap: IJclIntegerMap);
    procedure PutValue(Key: Integer; Value: TObject);
    function Remove(Key: Integer): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  end;

  TJclCardinalHashMapEntry = record
    Key: Cardinal;
    Value: TObject;
  end;

  TJclCardinalHashMapEntryArray = array of TJclCardinalHashMapEntry;

  TJclCardinalHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclCardinalHashMapEntryArray;
    procedure InitializeArrayAfterMove(var List: TJclCardinalHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure MoveArray(var List: TJclCardinalHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclCardinalHashMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclCardinalContainer, IJclContainer, IJclValueOwner,
    IJclCardinalMap)
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function KeysEqual(A, B: Cardinal): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclCardinalHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCardinalMap }
    procedure Clear;
    function ContainsKey(Key: Cardinal): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(Key: Cardinal): TObject;
    function GetValue(Key: Cardinal): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Cardinal;
    function KeySet: IJclCardinalSet;
    function Keys: IJclCardinalCollection;
    function MapEquals(const AMap: IJclCardinalMap): Boolean;
    procedure PutAll(const AMap: IJclCardinalMap);
    procedure PutValue(Key: Cardinal; Value: TObject);
    function Remove(Key: Cardinal): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  end;

  TJclInt64HashMapEntry = record
    Key: Int64;
    Value: TObject;
  end;

  TJclInt64HashMapEntryArray = array of TJclInt64HashMapEntry;

  TJclInt64HashMapBucket = class
  public
    Size: Integer;
    Entries: TJclInt64HashMapEntryArray;
    procedure InitializeArrayAfterMove(var List: TJclInt64HashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure MoveArray(var List: TJclInt64HashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclInt64HashMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclInt64Container, IJclContainer, IJclValueOwner,
    IJclInt64Map)
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function KeysEqual(const A, B: Int64): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclInt64HashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclInt64Map }
    procedure Clear;
    function ContainsKey(const Key: Int64): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: Int64): TObject;
    function GetValue(const Key: Int64): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Int64;
    function KeySet: IJclInt64Set;
    function Keys: IJclInt64Collection;
    function MapEquals(const AMap: IJclInt64Map): Boolean;
    procedure PutAll(const AMap: IJclInt64Map);
    procedure PutValue(const Key: Int64; Value: TObject);
    function Remove(const Key: Int64): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  end;

  TJclPtrHashMapEntry = record
    Key: Pointer;
    Value: TObject;
  end;

  TJclPtrHashMapEntryArray = array of TJclPtrHashMapEntry;

  TJclPtrHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclPtrHashMapEntryArray;
    procedure InitializeArrayAfterMove(var List: TJclPtrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure MoveArray(var List: TJclPtrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclPtrHashMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclPtrContainer, IJclContainer, IJclValueOwner,
    IJclPtrMap)
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function KeysEqual(A, B: Pointer): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclPtrHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclPtrMap }
    procedure Clear;
    function ContainsKey(Key: Pointer): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(Key: Pointer): TObject;
    function GetValue(Key: Pointer): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Pointer;
    function KeySet: IJclPtrSet;
    function Keys: IJclPtrCollection;
    function MapEquals(const AMap: IJclPtrMap): Boolean;
    procedure PutAll(const AMap: IJclPtrMap);
    procedure PutValue(Key: Pointer; Value: TObject);
    function Remove(Key: Pointer): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  end;

  TJclHashMapEntry = record
    Key: TObject;
    Value: TObject;
  end;

  TJclHashMapEntryArray = array of TJclHashMapEntry;

  TJclHashMapBucket = class
  public
    Size: Integer;
    Entries: TJclHashMapEntryArray;
    procedure InitializeArrayAfterMove(var List: TJclHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure MoveArray(var List: TJclHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclHashMap = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclContainer, IJclKeyOwner, IJclValueOwner,
    IJclMap)
  private
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function Hash(AObject: TObject): Integer;
    function KeysEqual(A, B: TObject): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    { IJclKeyOwner }
    function FreeKey(var Key: TObject): TObject;
    function GetOwnsKeys: Boolean;
    property OwnsKeys: Boolean read FOwnsKeys;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclHashMapBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclMap }
    procedure Clear;
    function ContainsKey(Key: TObject): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(Key: TObject): TObject;
    function GetValue(Key: TObject): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): TObject;
    function KeySet: IJclSet;
    function Keys: IJclCollection;
    function MapEquals(const AMap: IJclMap): Boolean;
    procedure PutAll(const AMap: IJclMap);
    procedure PutValue(Key: TObject; Value: TObject);
    function Remove(Key: TObject): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  end;


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
    procedure FinalizeArrayBeforeMove(var List: THashEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArray(var List: THashEntryArray; FromIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure InitializeArrayAfterMove(var List: THashEntryArray; FromIndex, ToIndex, Count: SizeInt);
      {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

    procedure MoveArray(var List: THashEntryArray; FromIndex, ToIndex, Count: SizeInt);
  end;

  TJclHashMap<TKey,TValue> = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclBaseContainer, IJclPairOwner<TKey, TValue>,
    IJclMap<TKey,TValue>)
  protected
    type
      TBucket = TJclBucket<TKey,TValue>;
  private
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
    function Hash(const AKey: TKey): Integer; virtual; abstract;
    function KeysEqual(const A, B: TKey): Boolean; virtual; abstract;
    function ValuesEqual(const A, B: TValue): Boolean; virtual; abstract;
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
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TBucket;
    FHashToRangeFunction: TJclHashToRangeFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
    destructor Destroy; override;
    property HashToRangeFunction: TJclHashToRangeFunction read FHashToRangeFunction write FHashToRangeFunction;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclMap<TKey,TValue> }
    procedure Clear;
    function ContainsKey(const Key: TKey): Boolean;
    function ContainsValue(const Value: TValue): Boolean;
    function Extract(const Key: TKey): TValue;
    function GetValue(const Key: TKey): TValue;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: TValue): TKey;
    function KeySet: IJclSet<TKey>;
    function Keys: IJclCollection<TKey>;
    function MapEquals(const AMap: IJclMap<TKey,TValue>): Boolean;
    procedure PutAll(const AMap: IJclMap<TKey,TValue>);
    procedure PutValue(const Key: TKey; const Value: TValue);
    function Remove(const Key: TKey): TValue;
    function Size: Integer;
    function Values: IJclCollection<TValue>;
  end;

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

//=== { TJclIntfIntfHashMapBucket } ==========================================

procedure TJclIntfIntfHashMapBucket.FinalizeArrayBeforeMove(var List: TJclIntfIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclIntfIntfHashMapBucket.InitializeArray(var List: TJclIntfIntfHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclIntfIntfHashMapBucket.InitializeArrayAfterMove(var List: TJclIntfIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclIntfIntfHashMapBucket.MoveArray(var List: TJclIntfIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclIntfIntfHashMap } ==========================================

constructor TJclIntfIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntfIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfIntfHashMapBucket;
  ADest: TJclIntfIntfHashMap;
  AMap: IJclIntfIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfIntfHashMap then
    begin
      ADest := TJclIntfIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfIntfHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfIntfHashMap then
    TJclIntfIntfHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntfIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfIntfHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclIntfIntfHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.Extract(const Key: IInterface): IInterface;
var
  Bucket: TJclIntfIntfHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclIntfIntfHashMap.GetValue(const Key: IInterface): IInterface;
var
  I: Integer;
  Bucket: TJclIntfIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfIntfHashMap.KeyOfValue(const Value: IInterface): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.Keys: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.MapEquals(const AMap: IJclIntfIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclIntfIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfIntfHashMapBucket;
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

procedure TJclIntfIntfHashMap.PutAll(const AMap: IJclIntfIntfMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.PutValue(const Key: IInterface; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclIntfIntfHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfIntfHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfIntfHashMap.Remove(const Key: IInterface): IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.SetCapacity(Value: Integer);
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

function TJclIntfIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfIntfHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclIntfIntfHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntfIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclAnsiStrIntfHashMapBucket } ==========================================

procedure TJclAnsiStrIntfHashMapBucket.FinalizeArrayBeforeMove(var List: TJclAnsiStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclAnsiStrIntfHashMapBucket.InitializeArray(var List: TJclAnsiStrIntfHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclAnsiStrIntfHashMapBucket.InitializeArrayAfterMove(var List: TJclAnsiStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclAnsiStrIntfHashMapBucket.MoveArray(var List: TJclAnsiStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclAnsiStrIntfHashMap } ==========================================

constructor TJclAnsiStrIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclAnsiStrIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclAnsiStrIntfHashMapBucket;
  ADest: TJclAnsiStrIntfHashMap;
  AMap: IJclAnsiStrIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclAnsiStrIntfHashMap then
    begin
      ADest := TJclAnsiStrIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclAnsiStrIntfHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclAnsiStrIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrIntfHashMap then
    TJclAnsiStrIntfHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclAnsiStrIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclAnsiStrIntfHashMap.ContainsKey(const Key: AnsiString): Boolean;
var
  I: Integer;
  Bucket: TJclAnsiStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclAnsiStrIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.Extract(const Key: AnsiString): IInterface;
var
  Bucket: TJclAnsiStrIntfHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclAnsiStrIntfHashMap.GetValue(const Key: AnsiString): IInterface;
var
  I: Integer;
  Bucket: TJclAnsiStrIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrIntfHashMap.KeyOfValue(const Value: IInterface): AnsiString;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.KeySet: IJclAnsiStrSet;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.Keys: IJclAnsiStrCollection;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.MapEquals(const AMap: IJclAnsiStrIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclAnsiStrIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclAnsiStrIntfHashMapBucket;
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

procedure TJclAnsiStrIntfHashMap.PutAll(const AMap: IJclAnsiStrIntfMap);
var
  It: IJclAnsiStrIterator;
  Key: AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfHashMap.PutValue(const Key: AnsiString; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclAnsiStrIntfHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclAnsiStrIntfHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclAnsiStrIntfHashMap.Remove(const Key: AnsiString): IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfHashMap.SetCapacity(Value: Integer);
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

function TJclAnsiStrIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrIntfHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclAnsiStrIntfHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclAnsiStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

//=== { TJclIntfAnsiStrHashMapBucket } ==========================================

procedure TJclIntfAnsiStrHashMapBucket.FinalizeArrayBeforeMove(var List: TJclIntfAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclIntfAnsiStrHashMapBucket.InitializeArray(var List: TJclIntfAnsiStrHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclIntfAnsiStrHashMapBucket.InitializeArrayAfterMove(var List: TJclIntfAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclIntfAnsiStrHashMapBucket.MoveArray(var List: TJclIntfAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclIntfAnsiStrHashMap } ==========================================

constructor TJclIntfAnsiStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntfAnsiStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfAnsiStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfAnsiStrHashMapBucket;
  ADest: TJclIntfAnsiStrHashMap;
  AMap: IJclIntfAnsiStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfAnsiStrHashMap then
    begin
      ADest := TJclIntfAnsiStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfAnsiStrHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfAnsiStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfAnsiStrHashMap then
    TJclIntfAnsiStrHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntfAnsiStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclIntfAnsiStrHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfAnsiStrHashMap.ContainsValue(const Value: AnsiString): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.Extract(const Key: IInterface): AnsiString;
var
  Bucket: TJclIntfAnsiStrHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := '';
          FreeKey(Bucket.Entries[I].Key);
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

function TJclIntfAnsiStrHashMap.GetValue(const Key: IInterface): AnsiString;
var
  I: Integer;
  Bucket: TJclIntfAnsiStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfAnsiStrHashMap.KeyOfValue(const Value: AnsiString): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.Keys: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.MapEquals(const AMap: IJclIntfAnsiStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclIntfAnsiStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfAnsiStrHashMapBucket;
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

procedure TJclIntfAnsiStrHashMap.PutAll(const AMap: IJclIntfAnsiStrMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrHashMap.PutValue(const Key: IInterface; const Value: AnsiString);
var
  Index: Integer;
  Bucket: TJclIntfAnsiStrHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, '')) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfAnsiStrHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfAnsiStrHashMap.Remove(const Key: IInterface): AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrHashMap.SetCapacity(Value: Integer);
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

function TJclIntfAnsiStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfAnsiStrHashMap.Values: IJclAnsiStrCollection;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfAnsiStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfAnsiStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfAnsiStrHashMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;

function TJclIntfAnsiStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := IntfSimpleHashConvert(AInterface);
end;

function TJclIntfAnsiStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

function TJclIntfAnsiStrHashMap.ValuesEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclAnsiStrAnsiStrHashMapBucket } ==========================================

procedure TJclAnsiStrAnsiStrHashMapBucket.FinalizeArrayBeforeMove(var List: TJclAnsiStrAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclAnsiStrAnsiStrHashMapBucket.InitializeArray(var List: TJclAnsiStrAnsiStrHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclAnsiStrAnsiStrHashMapBucket.InitializeArrayAfterMove(var List: TJclAnsiStrAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclAnsiStrAnsiStrHashMapBucket.MoveArray(var List: TJclAnsiStrAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclAnsiStrAnsiStrHashMap } ==========================================

constructor TJclAnsiStrAnsiStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclAnsiStrAnsiStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrAnsiStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclAnsiStrAnsiStrHashMapBucket;
  ADest: TJclAnsiStrAnsiStrHashMap;
  AMap: IJclAnsiStrAnsiStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclAnsiStrAnsiStrHashMap then
    begin
      ADest := TJclAnsiStrAnsiStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclAnsiStrAnsiStrHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclAnsiStrAnsiStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrAnsiStrHashMap then
    TJclAnsiStrAnsiStrHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclAnsiStrAnsiStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclAnsiStrAnsiStrHashMap.ContainsKey(const Key: AnsiString): Boolean;
var
  I: Integer;
  Bucket: TJclAnsiStrAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclAnsiStrAnsiStrHashMap.ContainsValue(const Value: AnsiString): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.Extract(const Key: AnsiString): AnsiString;
var
  Bucket: TJclAnsiStrAnsiStrHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := '';
          FreeKey(Bucket.Entries[I].Key);
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

function TJclAnsiStrAnsiStrHashMap.GetValue(const Key: AnsiString): AnsiString;
var
  I: Integer;
  Bucket: TJclAnsiStrAnsiStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrAnsiStrHashMap.KeyOfValue(const Value: AnsiString): AnsiString;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.KeySet: IJclAnsiStrSet;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.Keys: IJclAnsiStrCollection;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.MapEquals(const AMap: IJclAnsiStrAnsiStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclAnsiStrAnsiStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclAnsiStrAnsiStrHashMapBucket;
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

procedure TJclAnsiStrAnsiStrHashMap.PutAll(const AMap: IJclAnsiStrAnsiStrMap);
var
  It: IJclAnsiStrIterator;
  Key: AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrHashMap.PutValue(const Key: AnsiString; const Value: AnsiString);
var
  Index: Integer;
  Bucket: TJclAnsiStrAnsiStrHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, '')) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclAnsiStrAnsiStrHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclAnsiStrAnsiStrHashMap.Remove(const Key: AnsiString): AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrHashMap.SetCapacity(Value: Integer);
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

function TJclAnsiStrAnsiStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrAnsiStrHashMap.Values: IJclAnsiStrCollection;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrAnsiStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrAnsiStrHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrAnsiStrHashMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;

function TJclAnsiStrAnsiStrHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclAnsiStrAnsiStrHashMap.ValuesEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclWideStrIntfHashMapBucket } ==========================================

procedure TJclWideStrIntfHashMapBucket.FinalizeArrayBeforeMove(var List: TJclWideStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclWideStrIntfHashMapBucket.InitializeArray(var List: TJclWideStrIntfHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclWideStrIntfHashMapBucket.InitializeArrayAfterMove(var List: TJclWideStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclWideStrIntfHashMapBucket.MoveArray(var List: TJclWideStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclWideStrIntfHashMap } ==========================================

constructor TJclWideStrIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclWideStrIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclWideStrIntfHashMapBucket;
  ADest: TJclWideStrIntfHashMap;
  AMap: IJclWideStrIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclWideStrIntfHashMap then
    begin
      ADest := TJclWideStrIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclWideStrIntfHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclWideStrIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrIntfHashMap then
    TJclWideStrIntfHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclWideStrIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclWideStrIntfHashMap.ContainsKey(const Key: WideString): Boolean;
var
  I: Integer;
  Bucket: TJclWideStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclWideStrIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.Extract(const Key: WideString): IInterface;
var
  Bucket: TJclWideStrIntfHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclWideStrIntfHashMap.GetValue(const Key: WideString): IInterface;
var
  I: Integer;
  Bucket: TJclWideStrIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrIntfHashMap.KeyOfValue(const Value: IInterface): WideString;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.KeySet: IJclWideStrSet;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.Keys: IJclWideStrCollection;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.MapEquals(const AMap: IJclWideStrIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclWideStrIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclWideStrIntfHashMapBucket;
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

procedure TJclWideStrIntfHashMap.PutAll(const AMap: IJclWideStrIntfMap);
var
  It: IJclWideStrIterator;
  Key: WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfHashMap.PutValue(const Key: WideString; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclWideStrIntfHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclWideStrIntfHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclWideStrIntfHashMap.Remove(const Key: WideString): IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfHashMap.SetCapacity(Value: Integer);
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

function TJclWideStrIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclWideStrIntfHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclWideStrIntfHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclWideStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

//=== { TJclIntfWideStrHashMapBucket } ==========================================

procedure TJclIntfWideStrHashMapBucket.FinalizeArrayBeforeMove(var List: TJclIntfWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclIntfWideStrHashMapBucket.InitializeArray(var List: TJclIntfWideStrHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclIntfWideStrHashMapBucket.InitializeArrayAfterMove(var List: TJclIntfWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclIntfWideStrHashMapBucket.MoveArray(var List: TJclIntfWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclIntfWideStrHashMap } ==========================================

constructor TJclIntfWideStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntfWideStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfWideStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfWideStrHashMapBucket;
  ADest: TJclIntfWideStrHashMap;
  AMap: IJclIntfWideStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfWideStrHashMap then
    begin
      ADest := TJclIntfWideStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfWideStrHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfWideStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfWideStrHashMap then
    TJclIntfWideStrHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntfWideStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclIntfWideStrHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfWideStrHashMap.ContainsValue(const Value: WideString): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.Extract(const Key: IInterface): WideString;
var
  Bucket: TJclIntfWideStrHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := '';
          FreeKey(Bucket.Entries[I].Key);
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

function TJclIntfWideStrHashMap.GetValue(const Key: IInterface): WideString;
var
  I: Integer;
  Bucket: TJclIntfWideStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfWideStrHashMap.KeyOfValue(const Value: WideString): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.Keys: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.MapEquals(const AMap: IJclIntfWideStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclIntfWideStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfWideStrHashMapBucket;
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

procedure TJclIntfWideStrHashMap.PutAll(const AMap: IJclIntfWideStrMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrHashMap.PutValue(const Key: IInterface; const Value: WideString);
var
  Index: Integer;
  Bucket: TJclIntfWideStrHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, '')) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfWideStrHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfWideStrHashMap.Remove(const Key: IInterface): WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrHashMap.SetCapacity(Value: Integer);
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

function TJclIntfWideStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfWideStrHashMap.Values: IJclWideStrCollection;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfWideStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfWideStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfWideStrHashMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;

function TJclIntfWideStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := IntfSimpleHashConvert(AInterface);
end;

function TJclIntfWideStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

function TJclIntfWideStrHashMap.ValuesEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclWideStrWideStrHashMapBucket } ==========================================

procedure TJclWideStrWideStrHashMapBucket.FinalizeArrayBeforeMove(var List: TJclWideStrWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclWideStrWideStrHashMapBucket.InitializeArray(var List: TJclWideStrWideStrHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclWideStrWideStrHashMapBucket.InitializeArrayAfterMove(var List: TJclWideStrWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclWideStrWideStrHashMapBucket.MoveArray(var List: TJclWideStrWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclWideStrWideStrHashMap } ==========================================

constructor TJclWideStrWideStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclWideStrWideStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrWideStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclWideStrWideStrHashMapBucket;
  ADest: TJclWideStrWideStrHashMap;
  AMap: IJclWideStrWideStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclWideStrWideStrHashMap then
    begin
      ADest := TJclWideStrWideStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclWideStrWideStrHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclWideStrWideStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrWideStrHashMap then
    TJclWideStrWideStrHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclWideStrWideStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclWideStrWideStrHashMap.ContainsKey(const Key: WideString): Boolean;
var
  I: Integer;
  Bucket: TJclWideStrWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclWideStrWideStrHashMap.ContainsValue(const Value: WideString): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.Extract(const Key: WideString): WideString;
var
  Bucket: TJclWideStrWideStrHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := '';
          FreeKey(Bucket.Entries[I].Key);
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

function TJclWideStrWideStrHashMap.GetValue(const Key: WideString): WideString;
var
  I: Integer;
  Bucket: TJclWideStrWideStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrWideStrHashMap.KeyOfValue(const Value: WideString): WideString;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.KeySet: IJclWideStrSet;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.Keys: IJclWideStrCollection;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.MapEquals(const AMap: IJclWideStrWideStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclWideStrWideStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclWideStrWideStrHashMapBucket;
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

procedure TJclWideStrWideStrHashMap.PutAll(const AMap: IJclWideStrWideStrMap);
var
  It: IJclWideStrIterator;
  Key: WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrHashMap.PutValue(const Key: WideString; const Value: WideString);
var
  Index: Integer;
  Bucket: TJclWideStrWideStrHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, '')) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclWideStrWideStrHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclWideStrWideStrHashMap.Remove(const Key: WideString): WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrHashMap.SetCapacity(Value: Integer);
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

function TJclWideStrWideStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrWideStrHashMap.Values: IJclWideStrCollection;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrWideStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclWideStrWideStrHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrWideStrHashMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;

function TJclWideStrWideStrHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclWideStrWideStrHashMap.ValuesEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrIntfHashMapBucket } ==========================================

procedure TJclUnicodeStrIntfHashMapBucket.FinalizeArrayBeforeMove(var List: TJclUnicodeStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclUnicodeStrIntfHashMapBucket.InitializeArray(var List: TJclUnicodeStrIntfHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclUnicodeStrIntfHashMapBucket.InitializeArrayAfterMove(var List: TJclUnicodeStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclUnicodeStrIntfHashMapBucket.MoveArray(var List: TJclUnicodeStrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;


{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrIntfHashMap } ==========================================

constructor TJclUnicodeStrIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclUnicodeStrIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclUnicodeStrIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclUnicodeStrIntfHashMapBucket;
  ADest: TJclUnicodeStrIntfHashMap;
  AMap: IJclUnicodeStrIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclUnicodeStrIntfHashMap then
    begin
      ADest := TJclUnicodeStrIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclUnicodeStrIntfHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclUnicodeStrIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclUnicodeStrIntfHashMap then
    TJclUnicodeStrIntfHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclUnicodeStrIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrIntfHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclUnicodeStrIntfHashMap.ContainsKey(const Key: UnicodeString): Boolean;
var
  I: Integer;
  Bucket: TJclUnicodeStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclUnicodeStrIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrIntfHashMap.Extract(const Key: UnicodeString): IInterface;
var
  Bucket: TJclUnicodeStrIntfHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclUnicodeStrIntfHashMap.GetValue(const Key: UnicodeString): IInterface;
var
  I: Integer;
  Bucket: TJclUnicodeStrIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclUnicodeStrIntfHashMap.KeyOfValue(const Value: IInterface): UnicodeString;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrIntfHashMap.KeySet: IJclUnicodeStrSet;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclUnicodeStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrIntfHashMap.Keys: IJclUnicodeStrCollection;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclUnicodeStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrIntfHashMap.MapEquals(const AMap: IJclUnicodeStrIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclUnicodeStrIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclUnicodeStrIntfHashMapBucket;
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

procedure TJclUnicodeStrIntfHashMap.PutAll(const AMap: IJclUnicodeStrIntfMap);
var
  It: IJclUnicodeStrIterator;
  Key: UnicodeString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrIntfHashMap.PutValue(const Key: UnicodeString; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclUnicodeStrIntfHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclUnicodeStrIntfHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclUnicodeStrIntfHashMap.Remove(const Key: UnicodeString): IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrIntfHashMap.SetCapacity(Value: Integer);
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

function TJclUnicodeStrIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclUnicodeStrIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclUnicodeStrIntfHashMap.FreeKey(var Key: UnicodeString): UnicodeString;
begin
  Result := Key;
  Key := '';
end;

function TJclUnicodeStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclUnicodeStrIntfHashMap.KeysEqual(const A, B: UnicodeString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclUnicodeStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclIntfUnicodeStrHashMapBucket } ==========================================

procedure TJclIntfUnicodeStrHashMapBucket.FinalizeArrayBeforeMove(var List: TJclIntfUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclIntfUnicodeStrHashMapBucket.InitializeArray(var List: TJclIntfUnicodeStrHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclIntfUnicodeStrHashMapBucket.InitializeArrayAfterMove(var List: TJclIntfUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclIntfUnicodeStrHashMapBucket.MoveArray(var List: TJclIntfUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;


{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclIntfUnicodeStrHashMap } ==========================================

constructor TJclIntfUnicodeStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntfUnicodeStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfUnicodeStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfUnicodeStrHashMapBucket;
  ADest: TJclIntfUnicodeStrHashMap;
  AMap: IJclIntfUnicodeStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfUnicodeStrHashMap then
    begin
      ADest := TJclIntfUnicodeStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfUnicodeStrHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfUnicodeStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfUnicodeStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfUnicodeStrHashMap then
    TJclIntfUnicodeStrHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntfUnicodeStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfUnicodeStrHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclIntfUnicodeStrHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfUnicodeStrHashMap.ContainsValue(const Value: UnicodeString): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfUnicodeStrHashMap.Extract(const Key: IInterface): UnicodeString;
var
  Bucket: TJclIntfUnicodeStrHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := '';
          FreeKey(Bucket.Entries[I].Key);
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

function TJclIntfUnicodeStrHashMap.GetValue(const Key: IInterface): UnicodeString;
var
  I: Integer;
  Bucket: TJclIntfUnicodeStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfUnicodeStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfUnicodeStrHashMap.KeyOfValue(const Value: UnicodeString): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfUnicodeStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfUnicodeStrHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfUnicodeStrHashMap.Keys: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfUnicodeStrHashMap.MapEquals(const AMap: IJclIntfUnicodeStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclIntfUnicodeStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfUnicodeStrHashMapBucket;
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

procedure TJclIntfUnicodeStrHashMap.PutAll(const AMap: IJclIntfUnicodeStrMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfUnicodeStrHashMap.PutValue(const Key: IInterface; const Value: UnicodeString);
var
  Index: Integer;
  Bucket: TJclIntfUnicodeStrHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, '')) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfUnicodeStrHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfUnicodeStrHashMap.Remove(const Key: IInterface): UnicodeString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfUnicodeStrHashMap.SetCapacity(Value: Integer);
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

function TJclIntfUnicodeStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfUnicodeStrHashMap.Values: IJclUnicodeStrCollection;
var
  I, J: Integer;
  Bucket: TJclIntfUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclUnicodeStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfUnicodeStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfUnicodeStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfUnicodeStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfUnicodeStrHashMap.FreeValue(var Value: UnicodeString): UnicodeString;
begin
  Result := Value;
  Value := '';
end;

function TJclIntfUnicodeStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := IntfSimpleHashConvert(AInterface);
end;

function TJclIntfUnicodeStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

function TJclIntfUnicodeStrHashMap.ValuesEqual(const A, B: UnicodeString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrUnicodeStrHashMapBucket } ==========================================

procedure TJclUnicodeStrUnicodeStrHashMapBucket.FinalizeArrayBeforeMove(var List: TJclUnicodeStrUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclUnicodeStrUnicodeStrHashMapBucket.InitializeArray(var List: TJclUnicodeStrUnicodeStrHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclUnicodeStrUnicodeStrHashMapBucket.InitializeArrayAfterMove(var List: TJclUnicodeStrUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclUnicodeStrUnicodeStrHashMapBucket.MoveArray(var List: TJclUnicodeStrUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;


{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrUnicodeStrHashMap } ==========================================

constructor TJclUnicodeStrUnicodeStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclUnicodeStrUnicodeStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclUnicodeStrUnicodeStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclUnicodeStrUnicodeStrHashMapBucket;
  ADest: TJclUnicodeStrUnicodeStrHashMap;
  AMap: IJclUnicodeStrUnicodeStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclUnicodeStrUnicodeStrHashMap then
    begin
      ADest := TJclUnicodeStrUnicodeStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclUnicodeStrUnicodeStrHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclUnicodeStrUnicodeStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrUnicodeStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclUnicodeStrUnicodeStrHashMap then
    TJclUnicodeStrUnicodeStrHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclUnicodeStrUnicodeStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrUnicodeStrHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclUnicodeStrUnicodeStrHashMap.ContainsKey(const Key: UnicodeString): Boolean;
var
  I: Integer;
  Bucket: TJclUnicodeStrUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclUnicodeStrUnicodeStrHashMap.ContainsValue(const Value: UnicodeString): Boolean;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrUnicodeStrHashMap.Extract(const Key: UnicodeString): UnicodeString;
var
  Bucket: TJclUnicodeStrUnicodeStrHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := '';
          FreeKey(Bucket.Entries[I].Key);
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

function TJclUnicodeStrUnicodeStrHashMap.GetValue(const Key: UnicodeString): UnicodeString;
var
  I: Integer;
  Bucket: TJclUnicodeStrUnicodeStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrUnicodeStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclUnicodeStrUnicodeStrHashMap.KeyOfValue(const Value: UnicodeString): UnicodeString;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrUnicodeStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrUnicodeStrHashMap.KeySet: IJclUnicodeStrSet;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclUnicodeStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrUnicodeStrHashMap.Keys: IJclUnicodeStrCollection;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclUnicodeStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrUnicodeStrHashMap.MapEquals(const AMap: IJclUnicodeStrUnicodeStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclUnicodeStrUnicodeStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclUnicodeStrUnicodeStrHashMapBucket;
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

procedure TJclUnicodeStrUnicodeStrHashMap.PutAll(const AMap: IJclUnicodeStrUnicodeStrMap);
var
  It: IJclUnicodeStrIterator;
  Key: UnicodeString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrUnicodeStrHashMap.PutValue(const Key: UnicodeString; const Value: UnicodeString);
var
  Index: Integer;
  Bucket: TJclUnicodeStrUnicodeStrHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, '')) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclUnicodeStrUnicodeStrHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclUnicodeStrUnicodeStrHashMap.Remove(const Key: UnicodeString): UnicodeString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrUnicodeStrHashMap.SetCapacity(Value: Integer);
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

function TJclUnicodeStrUnicodeStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclUnicodeStrUnicodeStrHashMap.Values: IJclUnicodeStrCollection;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclUnicodeStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrUnicodeStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrUnicodeStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclUnicodeStrUnicodeStrHashMap.FreeKey(var Key: UnicodeString): UnicodeString;
begin
  Result := Key;
  Key := '';
end;

function TJclUnicodeStrUnicodeStrHashMap.FreeValue(var Value: UnicodeString): UnicodeString;
begin
  Result := Value;
  Value := '';
end;

function TJclUnicodeStrUnicodeStrHashMap.KeysEqual(const A, B: UnicodeString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclUnicodeStrUnicodeStrHashMap.ValuesEqual(const A, B: UnicodeString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$ENDIF SUPPORTS_UNICODE_STRING}

//=== { TJclSingleIntfHashMapBucket } ==========================================

procedure TJclSingleIntfHashMapBucket.FinalizeArrayBeforeMove(var List: TJclSingleIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclSingleIntfHashMapBucket.InitializeArray(var List: TJclSingleIntfHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclSingleIntfHashMapBucket.InitializeArrayAfterMove(var List: TJclSingleIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclSingleIntfHashMapBucket.MoveArray(var List: TJclSingleIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclSingleIntfHashMap } ==========================================

constructor TJclSingleIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclSingleIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSingleIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclSingleIntfHashMapBucket;
  ADest: TJclSingleIntfHashMap;
  AMap: IJclSingleIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclSingleIntfHashMap then
    begin
      ADest := TJclSingleIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclSingleIntfHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclSingleIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclSingleIntfHashMap then
    TJclSingleIntfHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclSingleIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclSingleIntfHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclSingleIntfHashMap.ContainsKey(const Key: Single): Boolean;
var
  I: Integer;
  Bucket: TJclSingleIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclSingleIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.Extract(const Key: Single): IInterface;
var
  Bucket: TJclSingleIntfHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclSingleIntfHashMap.GetValue(const Key: Single): IInterface;
var
  I: Integer;
  Bucket: TJclSingleIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleIntfHashMap.KeyOfValue(const Value: IInterface): Single;
var
  I, J: Integer;
  Bucket: TJclSingleIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.KeySet: IJclSingleSet;
var
  I, J: Integer;
  Bucket: TJclSingleIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.Keys: IJclSingleCollection;
var
  I, J: Integer;
  Bucket: TJclSingleIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.MapEquals(const AMap: IJclSingleIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclSingleIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclSingleIntfHashMapBucket;
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

procedure TJclSingleIntfHashMap.PutAll(const AMap: IJclSingleIntfMap);
var
  It: IJclSingleIterator;
  Key: Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleIntfHashMap.PutValue(const Key: Single; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclSingleIntfHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclSingleIntfHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclSingleIntfHashMap.Remove(const Key: Single): IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleIntfHashMap.SetCapacity(Value: Integer);
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

function TJclSingleIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclSingleIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclSingleIntfHashMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclSingleIntfHashMap.KeysEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclSingleIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

//=== { TJclIntfSingleHashMapBucket } ==========================================

procedure TJclIntfSingleHashMapBucket.FinalizeArrayBeforeMove(var List: TJclIntfSingleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclIntfSingleHashMapBucket.InitializeArray(var List: TJclIntfSingleHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclIntfSingleHashMapBucket.InitializeArrayAfterMove(var List: TJclIntfSingleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclIntfSingleHashMapBucket.MoveArray(var List: TJclIntfSingleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclIntfSingleHashMap } ==========================================

constructor TJclIntfSingleHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntfSingleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfSingleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfSingleHashMapBucket;
  ADest: TJclIntfSingleHashMap;
  AMap: IJclIntfSingleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfSingleHashMap then
    begin
      ADest := TJclIntfSingleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfSingleHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfSingleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSingleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfSingleHashMap then
    TJclIntfSingleHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntfSingleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfSingleHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclIntfSingleHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfSingleHashMap.ContainsValue(const Value: Single): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.Extract(const Key: IInterface): Single;
var
  Bucket: TJclIntfSingleHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := 0.0;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclIntfSingleHashMap.GetValue(const Key: IInterface): Single;
var
  I: Integer;
  Bucket: TJclIntfSingleHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfSingleHashMap.KeyOfValue(const Value: Single): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfSingleHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.Keys: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.MapEquals(const AMap: IJclIntfSingleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclIntfSingleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfSingleHashMapBucket;
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

procedure TJclIntfSingleHashMap.PutAll(const AMap: IJclIntfSingleMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSingleHashMap.PutValue(const Key: IInterface; const Value: Single);
var
  Index: Integer;
  Bucket: TJclIntfSingleHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0.0)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfSingleHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfSingleHashMap.Remove(const Key: IInterface): Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSingleHashMap.SetCapacity(Value: Integer);
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

function TJclIntfSingleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfSingleHashMap.Values: IJclSingleCollection;
var
  I, J: Integer;
  Bucket: TJclIntfSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfSingleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfSingleHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfSingleHashMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclIntfSingleHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := IntfSimpleHashConvert(AInterface);
end;

function TJclIntfSingleHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

function TJclIntfSingleHashMap.ValuesEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclSingleSingleHashMapBucket } ==========================================

procedure TJclSingleSingleHashMapBucket.InitializeArrayAfterMove(var List: TJclSingleSingleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Clean array }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
    else
     FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end;
end;

procedure TJclSingleSingleHashMapBucket.MoveArray(var List: TJclSingleSingleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclSingleSingleHashMap } ==========================================

constructor TJclSingleSingleHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclSingleSingleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSingleSingleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclSingleSingleHashMapBucket;
  ADest: TJclSingleSingleHashMap;
  AMap: IJclSingleSingleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclSingleSingleHashMap then
    begin
      ADest := TJclSingleSingleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclSingleSingleHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclSingleSingleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSingleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclSingleSingleHashMap then
    TJclSingleSingleHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclSingleSingleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclSingleSingleHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclSingleSingleHashMap.ContainsKey(const Key: Single): Boolean;
var
  I: Integer;
  Bucket: TJclSingleSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclSingleSingleHashMap.ContainsValue(const Value: Single): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.Extract(const Key: Single): Single;
var
  Bucket: TJclSingleSingleHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := 0.0;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclSingleSingleHashMap.GetValue(const Key: Single): Single;
var
  I: Integer;
  Bucket: TJclSingleSingleHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleSingleHashMap.KeyOfValue(const Value: Single): Single;
var
  I, J: Integer;
  Bucket: TJclSingleSingleHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.KeySet: IJclSingleSet;
var
  I, J: Integer;
  Bucket: TJclSingleSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.Keys: IJclSingleCollection;
var
  I, J: Integer;
  Bucket: TJclSingleSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.MapEquals(const AMap: IJclSingleSingleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclSingleSingleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclSingleSingleHashMapBucket;
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

procedure TJclSingleSingleHashMap.PutAll(const AMap: IJclSingleSingleMap);
var
  It: IJclSingleIterator;
  Key: Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSingleHashMap.PutValue(const Key: Single; const Value: Single);
var
  Index: Integer;
  Bucket: TJclSingleSingleHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, 0.0)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclSingleSingleHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclSingleSingleHashMap.Remove(const Key: Single): Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSingleHashMap.SetCapacity(Value: Integer);
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

function TJclSingleSingleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleSingleHashMap.Values: IJclSingleCollection;
var
  I, J: Integer;
  Bucket: TJclSingleSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleSingleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclSingleSingleHashMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleSingleHashMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclSingleSingleHashMap.KeysEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclSingleSingleHashMap.ValuesEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclDoubleIntfHashMapBucket } ==========================================

procedure TJclDoubleIntfHashMapBucket.FinalizeArrayBeforeMove(var List: TJclDoubleIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclDoubleIntfHashMapBucket.InitializeArray(var List: TJclDoubleIntfHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclDoubleIntfHashMapBucket.InitializeArrayAfterMove(var List: TJclDoubleIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclDoubleIntfHashMapBucket.MoveArray(var List: TJclDoubleIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclDoubleIntfHashMap } ==========================================

constructor TJclDoubleIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclDoubleIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclDoubleIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclDoubleIntfHashMapBucket;
  ADest: TJclDoubleIntfHashMap;
  AMap: IJclDoubleIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclDoubleIntfHashMap then
    begin
      ADest := TJclDoubleIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclDoubleIntfHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclDoubleIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclDoubleIntfHashMap then
    TJclDoubleIntfHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclDoubleIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclDoubleIntfHashMap.ContainsKey(const Key: Double): Boolean;
var
  I: Integer;
  Bucket: TJclDoubleIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclDoubleIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.Extract(const Key: Double): IInterface;
var
  Bucket: TJclDoubleIntfHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclDoubleIntfHashMap.GetValue(const Key: Double): IInterface;
var
  I: Integer;
  Bucket: TJclDoubleIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleIntfHashMap.KeyOfValue(const Value: IInterface): Double;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.KeySet: IJclDoubleSet;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.Keys: IJclDoubleCollection;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.MapEquals(const AMap: IJclDoubleIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclDoubleIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclDoubleIntfHashMapBucket;
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

procedure TJclDoubleIntfHashMap.PutAll(const AMap: IJclDoubleIntfMap);
var
  It: IJclDoubleIterator;
  Key: Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleIntfHashMap.PutValue(const Key: Double; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclDoubleIntfHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclDoubleIntfHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclDoubleIntfHashMap.Remove(const Key: Double): IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleIntfHashMap.SetCapacity(Value: Integer);
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

function TJclDoubleIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclDoubleIntfHashMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclDoubleIntfHashMap.KeysEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclDoubleIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

//=== { TJclIntfDoubleHashMapBucket } ==========================================

procedure TJclIntfDoubleHashMapBucket.FinalizeArrayBeforeMove(var List: TJclIntfDoubleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclIntfDoubleHashMapBucket.InitializeArray(var List: TJclIntfDoubleHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclIntfDoubleHashMapBucket.InitializeArrayAfterMove(var List: TJclIntfDoubleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclIntfDoubleHashMapBucket.MoveArray(var List: TJclIntfDoubleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclIntfDoubleHashMap } ==========================================

constructor TJclIntfDoubleHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntfDoubleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfDoubleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfDoubleHashMapBucket;
  ADest: TJclIntfDoubleHashMap;
  AMap: IJclIntfDoubleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfDoubleHashMap then
    begin
      ADest := TJclIntfDoubleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfDoubleHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfDoubleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfDoubleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfDoubleHashMap then
    TJclIntfDoubleHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntfDoubleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclIntfDoubleHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfDoubleHashMap.ContainsValue(const Value: Double): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.Extract(const Key: IInterface): Double;
var
  Bucket: TJclIntfDoubleHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := 0.0;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclIntfDoubleHashMap.GetValue(const Key: IInterface): Double;
var
  I: Integer;
  Bucket: TJclIntfDoubleHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfDoubleHashMap.KeyOfValue(const Value: Double): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.Keys: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.MapEquals(const AMap: IJclIntfDoubleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclIntfDoubleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfDoubleHashMapBucket;
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

procedure TJclIntfDoubleHashMap.PutAll(const AMap: IJclIntfDoubleMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfDoubleHashMap.PutValue(const Key: IInterface; const Value: Double);
var
  Index: Integer;
  Bucket: TJclIntfDoubleHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0.0)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfDoubleHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfDoubleHashMap.Remove(const Key: IInterface): Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfDoubleHashMap.SetCapacity(Value: Integer);
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

function TJclIntfDoubleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfDoubleHashMap.Values: IJclDoubleCollection;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfDoubleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfDoubleHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfDoubleHashMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclIntfDoubleHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := IntfSimpleHashConvert(AInterface);
end;

function TJclIntfDoubleHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

function TJclIntfDoubleHashMap.ValuesEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclDoubleDoubleHashMapBucket } ==========================================

procedure TJclDoubleDoubleHashMapBucket.InitializeArrayAfterMove(var List: TJclDoubleDoubleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Clean array }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
    else
     FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end;
end;

procedure TJclDoubleDoubleHashMapBucket.MoveArray(var List: TJclDoubleDoubleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclDoubleDoubleHashMap } ==========================================

constructor TJclDoubleDoubleHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclDoubleDoubleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclDoubleDoubleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclDoubleDoubleHashMapBucket;
  ADest: TJclDoubleDoubleHashMap;
  AMap: IJclDoubleDoubleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclDoubleDoubleHashMap then
    begin
      ADest := TJclDoubleDoubleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclDoubleDoubleHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclDoubleDoubleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleDoubleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclDoubleDoubleHashMap then
    TJclDoubleDoubleHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclDoubleDoubleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclDoubleDoubleHashMap.ContainsKey(const Key: Double): Boolean;
var
  I: Integer;
  Bucket: TJclDoubleDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclDoubleDoubleHashMap.ContainsValue(const Value: Double): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.Extract(const Key: Double): Double;
var
  Bucket: TJclDoubleDoubleHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := 0.0;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclDoubleDoubleHashMap.GetValue(const Key: Double): Double;
var
  I: Integer;
  Bucket: TJclDoubleDoubleHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleDoubleHashMap.KeyOfValue(const Value: Double): Double;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.KeySet: IJclDoubleSet;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.Keys: IJclDoubleCollection;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.MapEquals(const AMap: IJclDoubleDoubleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclDoubleDoubleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclDoubleDoubleHashMapBucket;
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

procedure TJclDoubleDoubleHashMap.PutAll(const AMap: IJclDoubleDoubleMap);
var
  It: IJclDoubleIterator;
  Key: Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleDoubleHashMap.PutValue(const Key: Double; const Value: Double);
var
  Index: Integer;
  Bucket: TJclDoubleDoubleHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, 0.0)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclDoubleDoubleHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclDoubleDoubleHashMap.Remove(const Key: Double): Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleDoubleHashMap.SetCapacity(Value: Integer);
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

function TJclDoubleDoubleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleDoubleHashMap.Values: IJclDoubleCollection;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleDoubleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclDoubleDoubleHashMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleDoubleHashMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclDoubleDoubleHashMap.KeysEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclDoubleDoubleHashMap.ValuesEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclExtendedIntfHashMapBucket } ==========================================

procedure TJclExtendedIntfHashMapBucket.FinalizeArrayBeforeMove(var List: TJclExtendedIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclExtendedIntfHashMapBucket.InitializeArray(var List: TJclExtendedIntfHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclExtendedIntfHashMapBucket.InitializeArrayAfterMove(var List: TJclExtendedIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclExtendedIntfHashMapBucket.MoveArray(var List: TJclExtendedIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclExtendedIntfHashMap } ==========================================

constructor TJclExtendedIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclExtendedIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclExtendedIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclExtendedIntfHashMapBucket;
  ADest: TJclExtendedIntfHashMap;
  AMap: IJclExtendedIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclExtendedIntfHashMap then
    begin
      ADest := TJclExtendedIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclExtendedIntfHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclExtendedIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclExtendedIntfHashMap then
    TJclExtendedIntfHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclExtendedIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclExtendedIntfHashMap.ContainsKey(const Key: Extended): Boolean;
var
  I: Integer;
  Bucket: TJclExtendedIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclExtendedIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.Extract(const Key: Extended): IInterface;
var
  Bucket: TJclExtendedIntfHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclExtendedIntfHashMap.GetValue(const Key: Extended): IInterface;
var
  I: Integer;
  Bucket: TJclExtendedIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedIntfHashMap.KeyOfValue(const Value: IInterface): Extended;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.KeySet: IJclExtendedSet;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.Keys: IJclExtendedCollection;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.MapEquals(const AMap: IJclExtendedIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclExtendedIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclExtendedIntfHashMapBucket;
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

procedure TJclExtendedIntfHashMap.PutAll(const AMap: IJclExtendedIntfMap);
var
  It: IJclExtendedIterator;
  Key: Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedIntfHashMap.PutValue(const Key: Extended; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclExtendedIntfHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclExtendedIntfHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclExtendedIntfHashMap.Remove(const Key: Extended): IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedIntfHashMap.SetCapacity(Value: Integer);
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

function TJclExtendedIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclExtendedIntfHashMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclExtendedIntfHashMap.KeysEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclExtendedIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

//=== { TJclIntfExtendedHashMapBucket } ==========================================

procedure TJclIntfExtendedHashMapBucket.FinalizeArrayBeforeMove(var List: TJclIntfExtendedHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclIntfExtendedHashMapBucket.InitializeArray(var List: TJclIntfExtendedHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclIntfExtendedHashMapBucket.InitializeArrayAfterMove(var List: TJclIntfExtendedHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclIntfExtendedHashMapBucket.MoveArray(var List: TJclIntfExtendedHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclIntfExtendedHashMap } ==========================================

constructor TJclIntfExtendedHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntfExtendedHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfExtendedHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfExtendedHashMapBucket;
  ADest: TJclIntfExtendedHashMap;
  AMap: IJclIntfExtendedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfExtendedHashMap then
    begin
      ADest := TJclIntfExtendedHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfExtendedHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfExtendedMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfExtendedHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfExtendedHashMap then
    TJclIntfExtendedHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntfExtendedHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclIntfExtendedHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfExtendedHashMap.ContainsValue(const Value: Extended): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.Extract(const Key: IInterface): Extended;
var
  Bucket: TJclIntfExtendedHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := 0.0;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclIntfExtendedHashMap.GetValue(const Key: IInterface): Extended;
var
  I: Integer;
  Bucket: TJclIntfExtendedHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfExtendedHashMap.KeyOfValue(const Value: Extended): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.Keys: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.MapEquals(const AMap: IJclIntfExtendedMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclIntfExtendedHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfExtendedHashMapBucket;
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

procedure TJclIntfExtendedHashMap.PutAll(const AMap: IJclIntfExtendedMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfExtendedHashMap.PutValue(const Key: IInterface; const Value: Extended);
var
  Index: Integer;
  Bucket: TJclIntfExtendedHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0.0)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfExtendedHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfExtendedHashMap.Remove(const Key: IInterface): Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfExtendedHashMap.SetCapacity(Value: Integer);
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

function TJclIntfExtendedHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfExtendedHashMap.Values: IJclExtendedCollection;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfExtendedHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfExtendedHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfExtendedHashMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclIntfExtendedHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := IntfSimpleHashConvert(AInterface);
end;

function TJclIntfExtendedHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

function TJclIntfExtendedHashMap.ValuesEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclExtendedExtendedHashMapBucket } ==========================================

procedure TJclExtendedExtendedHashMapBucket.InitializeArrayAfterMove(var List: TJclExtendedExtendedHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Clean array }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
    else
     FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end;
end;

procedure TJclExtendedExtendedHashMapBucket.MoveArray(var List: TJclExtendedExtendedHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclExtendedExtendedHashMap } ==========================================

constructor TJclExtendedExtendedHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclExtendedExtendedHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclExtendedExtendedHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclExtendedExtendedHashMapBucket;
  ADest: TJclExtendedExtendedHashMap;
  AMap: IJclExtendedExtendedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclExtendedExtendedHashMap then
    begin
      ADest := TJclExtendedExtendedHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclExtendedExtendedHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclExtendedExtendedMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedExtendedHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclExtendedExtendedHashMap then
    TJclExtendedExtendedHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclExtendedExtendedHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclExtendedExtendedHashMap.ContainsKey(const Key: Extended): Boolean;
var
  I: Integer;
  Bucket: TJclExtendedExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclExtendedExtendedHashMap.ContainsValue(const Value: Extended): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.Extract(const Key: Extended): Extended;
var
  Bucket: TJclExtendedExtendedHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := 0.0;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclExtendedExtendedHashMap.GetValue(const Key: Extended): Extended;
var
  I: Integer;
  Bucket: TJclExtendedExtendedHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedExtendedHashMap.KeyOfValue(const Value: Extended): Extended;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.KeySet: IJclExtendedSet;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.Keys: IJclExtendedCollection;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.MapEquals(const AMap: IJclExtendedExtendedMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclExtendedExtendedHashMap.Pack;
var
  I: Integer;
  Bucket: TJclExtendedExtendedHashMapBucket;
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

procedure TJclExtendedExtendedHashMap.PutAll(const AMap: IJclExtendedExtendedMap);
var
  It: IJclExtendedIterator;
  Key: Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedExtendedHashMap.PutValue(const Key: Extended; const Value: Extended);
var
  Index: Integer;
  Bucket: TJclExtendedExtendedHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, 0.0)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclExtendedExtendedHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclExtendedExtendedHashMap.Remove(const Key: Extended): Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedExtendedHashMap.SetCapacity(Value: Integer);
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

function TJclExtendedExtendedHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedExtendedHashMap.Values: IJclExtendedCollection;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedExtendedHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclExtendedExtendedHashMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedExtendedHashMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclExtendedExtendedHashMap.KeysEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclExtendedExtendedHashMap.ValuesEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclIntegerIntfHashMapBucket } ==========================================

procedure TJclIntegerIntfHashMapBucket.FinalizeArrayBeforeMove(var List: TJclIntegerIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclIntegerIntfHashMapBucket.InitializeArray(var List: TJclIntegerIntfHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclIntegerIntfHashMapBucket.InitializeArrayAfterMove(var List: TJclIntegerIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclIntegerIntfHashMapBucket.MoveArray(var List: TJclIntegerIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclIntegerIntfHashMap } ==========================================

constructor TJclIntegerIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntegerIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntegerIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntegerIntfHashMapBucket;
  ADest: TJclIntegerIntfHashMap;
  AMap: IJclIntegerIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntegerIntfHashMap then
    begin
      ADest := TJclIntegerIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntegerIntfHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntegerIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntegerIntfHashMap then
    TJclIntegerIntfHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntegerIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclIntegerIntfHashMap.ContainsKey(Key: Integer): Boolean;
var
  I: Integer;
  Bucket: TJclIntegerIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntegerIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.Extract(Key: Integer): IInterface;
var
  Bucket: TJclIntegerIntfHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclIntegerIntfHashMap.GetValue(Key: Integer): IInterface;
var
  I: Integer;
  Bucket: TJclIntegerIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerIntfHashMap.KeyOfValue(const Value: IInterface): Integer;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.KeySet: IJclIntegerSet;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.Keys: IJclIntegerCollection;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.MapEquals(const AMap: IJclIntegerIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclIntegerIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntegerIntfHashMapBucket;
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

procedure TJclIntegerIntfHashMap.PutAll(const AMap: IJclIntegerIntfMap);
var
  It: IJclIntegerIterator;
  Key: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntfHashMap.PutValue(Key: Integer; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclIntegerIntfHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntegerIntfHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntegerIntfHashMap.Remove(Key: Integer): IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntfHashMap.SetCapacity(Value: Integer);
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

function TJclIntegerIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntegerIntfHashMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclIntegerIntfHashMap.KeysEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntegerIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

//=== { TJclIntfIntegerHashMapBucket } ==========================================

procedure TJclIntfIntegerHashMapBucket.FinalizeArrayBeforeMove(var List: TJclIntfIntegerHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclIntfIntegerHashMapBucket.InitializeArray(var List: TJclIntfIntegerHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclIntfIntegerHashMapBucket.InitializeArrayAfterMove(var List: TJclIntfIntegerHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclIntfIntegerHashMapBucket.MoveArray(var List: TJclIntfIntegerHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclIntfIntegerHashMap } ==========================================

constructor TJclIntfIntegerHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntfIntegerHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfIntegerHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfIntegerHashMapBucket;
  ADest: TJclIntfIntegerHashMap;
  AMap: IJclIntfIntegerMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfIntegerHashMap then
    begin
      ADest := TJclIntfIntegerHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfIntegerHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfIntegerMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntegerHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfIntegerHashMap then
    TJclIntfIntegerHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntfIntegerHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclIntfIntegerHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfIntegerHashMap.ContainsValue(Value: Integer): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.Extract(const Key: IInterface): Integer;
var
  Bucket: TJclIntfIntegerHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := 0;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclIntfIntegerHashMap.GetValue(const Key: IInterface): Integer;
var
  I: Integer;
  Bucket: TJclIntfIntegerHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfIntegerHashMap.KeyOfValue(Value: Integer): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.Keys: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.MapEquals(const AMap: IJclIntfIntegerMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclIntfIntegerHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfIntegerHashMapBucket;
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

procedure TJclIntfIntegerHashMap.PutAll(const AMap: IJclIntfIntegerMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntegerHashMap.PutValue(const Key: IInterface; Value: Integer);
var
  Index: Integer;
  Bucket: TJclIntfIntegerHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfIntegerHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfIntegerHashMap.Remove(const Key: IInterface): Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntegerHashMap.SetCapacity(Value: Integer);
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

function TJclIntfIntegerHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfIntegerHashMap.Values: IJclIntegerCollection;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntegerHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfIntegerHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfIntegerHashMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntfIntegerHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := IntfSimpleHashConvert(AInterface);
end;

function TJclIntfIntegerHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

function TJclIntfIntegerHashMap.ValuesEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclIntegerIntegerHashMapBucket } ==========================================

procedure TJclIntegerIntegerHashMapBucket.InitializeArrayAfterMove(var List: TJclIntegerIntegerHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Clean array }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
    else
     FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end;
end;

procedure TJclIntegerIntegerHashMapBucket.MoveArray(var List: TJclIntegerIntegerHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclIntegerIntegerHashMap } ==========================================

constructor TJclIntegerIntegerHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntegerIntegerHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntegerIntegerHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntegerIntegerHashMapBucket;
  ADest: TJclIntegerIntegerHashMap;
  AMap: IJclIntegerIntegerMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntegerIntegerHashMap then
    begin
      ADest := TJclIntegerIntegerHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntegerIntegerHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntegerIntegerMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntegerHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntegerIntegerHashMap then
    TJclIntegerIntegerHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntegerIntegerHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclIntegerIntegerHashMap.ContainsKey(Key: Integer): Boolean;
var
  I: Integer;
  Bucket: TJclIntegerIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntegerIntegerHashMap.ContainsValue(Value: Integer): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.Extract(Key: Integer): Integer;
var
  Bucket: TJclIntegerIntegerHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := 0;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclIntegerIntegerHashMap.GetValue(Key: Integer): Integer;
var
  I: Integer;
  Bucket: TJclIntegerIntegerHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerIntegerHashMap.KeyOfValue(Value: Integer): Integer;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.KeySet: IJclIntegerSet;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.Keys: IJclIntegerCollection;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.MapEquals(const AMap: IJclIntegerIntegerMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclIntegerIntegerHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntegerIntegerHashMapBucket;
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

procedure TJclIntegerIntegerHashMap.PutAll(const AMap: IJclIntegerIntegerMap);
var
  It: IJclIntegerIterator;
  Key: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntegerHashMap.PutValue(Key: Integer; Value: Integer);
var
  Index: Integer;
  Bucket: TJclIntegerIntegerHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, 0)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntegerIntegerHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntegerIntegerHashMap.Remove(Key: Integer): Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntegerHashMap.SetCapacity(Value: Integer);
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

function TJclIntegerIntegerHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerIntegerHashMap.Values: IJclIntegerCollection;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntegerHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntegerIntegerHashMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerIntegerHashMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntegerIntegerHashMap.KeysEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntegerIntegerHashMap.ValuesEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclCardinalIntfHashMapBucket } ==========================================

procedure TJclCardinalIntfHashMapBucket.FinalizeArrayBeforeMove(var List: TJclCardinalIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclCardinalIntfHashMapBucket.InitializeArray(var List: TJclCardinalIntfHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclCardinalIntfHashMapBucket.InitializeArrayAfterMove(var List: TJclCardinalIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclCardinalIntfHashMapBucket.MoveArray(var List: TJclCardinalIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclCardinalIntfHashMap } ==========================================

constructor TJclCardinalIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclCardinalIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclCardinalIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclCardinalIntfHashMapBucket;
  ADest: TJclCardinalIntfHashMap;
  AMap: IJclCardinalIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclCardinalIntfHashMap then
    begin
      ADest := TJclCardinalIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclCardinalIntfHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclCardinalIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclCardinalIntfHashMap then
    TJclCardinalIntfHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclCardinalIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclCardinalIntfHashMap.ContainsKey(Key: Cardinal): Boolean;
var
  I: Integer;
  Bucket: TJclCardinalIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclCardinalIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.Extract(Key: Cardinal): IInterface;
var
  Bucket: TJclCardinalIntfHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclCardinalIntfHashMap.GetValue(Key: Cardinal): IInterface;
var
  I: Integer;
  Bucket: TJclCardinalIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalIntfHashMap.KeyOfValue(const Value: IInterface): Cardinal;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.KeySet: IJclCardinalSet;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.Keys: IJclCardinalCollection;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.MapEquals(const AMap: IJclCardinalIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclCardinalIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclCardinalIntfHashMapBucket;
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

procedure TJclCardinalIntfHashMap.PutAll(const AMap: IJclCardinalIntfMap);
var
  It: IJclCardinalIterator;
  Key: Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalIntfHashMap.PutValue(Key: Cardinal; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclCardinalIntfHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclCardinalIntfHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclCardinalIntfHashMap.Remove(Key: Cardinal): IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalIntfHashMap.SetCapacity(Value: Integer);
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

function TJclCardinalIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclCardinalIntfHashMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclCardinalIntfHashMap.KeysEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclCardinalIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

//=== { TJclIntfCardinalHashMapBucket } ==========================================

procedure TJclIntfCardinalHashMapBucket.FinalizeArrayBeforeMove(var List: TJclIntfCardinalHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclIntfCardinalHashMapBucket.InitializeArray(var List: TJclIntfCardinalHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclIntfCardinalHashMapBucket.InitializeArrayAfterMove(var List: TJclIntfCardinalHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclIntfCardinalHashMapBucket.MoveArray(var List: TJclIntfCardinalHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclIntfCardinalHashMap } ==========================================

constructor TJclIntfCardinalHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntfCardinalHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfCardinalHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfCardinalHashMapBucket;
  ADest: TJclIntfCardinalHashMap;
  AMap: IJclIntfCardinalMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfCardinalHashMap then
    begin
      ADest := TJclIntfCardinalHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfCardinalHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfCardinalMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfCardinalHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfCardinalHashMap then
    TJclIntfCardinalHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntfCardinalHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclIntfCardinalHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfCardinalHashMap.ContainsValue(Value: Cardinal): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.Extract(const Key: IInterface): Cardinal;
var
  Bucket: TJclIntfCardinalHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := 0;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclIntfCardinalHashMap.GetValue(const Key: IInterface): Cardinal;
var
  I: Integer;
  Bucket: TJclIntfCardinalHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfCardinalHashMap.KeyOfValue(Value: Cardinal): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.Keys: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.MapEquals(const AMap: IJclIntfCardinalMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclIntfCardinalHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfCardinalHashMapBucket;
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

procedure TJclIntfCardinalHashMap.PutAll(const AMap: IJclIntfCardinalMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfCardinalHashMap.PutValue(const Key: IInterface; Value: Cardinal);
var
  Index: Integer;
  Bucket: TJclIntfCardinalHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfCardinalHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfCardinalHashMap.Remove(const Key: IInterface): Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfCardinalHashMap.SetCapacity(Value: Integer);
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

function TJclIntfCardinalHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfCardinalHashMap.Values: IJclCardinalCollection;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfCardinalHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfCardinalHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfCardinalHashMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntfCardinalHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := IntfSimpleHashConvert(AInterface);
end;

function TJclIntfCardinalHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

function TJclIntfCardinalHashMap.ValuesEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclCardinalCardinalHashMapBucket } ==========================================

procedure TJclCardinalCardinalHashMapBucket.InitializeArrayAfterMove(var List: TJclCardinalCardinalHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Clean array }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
    else
     FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end;
end;

procedure TJclCardinalCardinalHashMapBucket.MoveArray(var List: TJclCardinalCardinalHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclCardinalCardinalHashMap } ==========================================

constructor TJclCardinalCardinalHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclCardinalCardinalHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclCardinalCardinalHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclCardinalCardinalHashMapBucket;
  ADest: TJclCardinalCardinalHashMap;
  AMap: IJclCardinalCardinalMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclCardinalCardinalHashMap then
    begin
      ADest := TJclCardinalCardinalHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclCardinalCardinalHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclCardinalCardinalMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalCardinalHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclCardinalCardinalHashMap then
    TJclCardinalCardinalHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclCardinalCardinalHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclCardinalCardinalHashMap.ContainsKey(Key: Cardinal): Boolean;
var
  I: Integer;
  Bucket: TJclCardinalCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclCardinalCardinalHashMap.ContainsValue(Value: Cardinal): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.Extract(Key: Cardinal): Cardinal;
var
  Bucket: TJclCardinalCardinalHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := 0;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclCardinalCardinalHashMap.GetValue(Key: Cardinal): Cardinal;
var
  I: Integer;
  Bucket: TJclCardinalCardinalHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalCardinalHashMap.KeyOfValue(Value: Cardinal): Cardinal;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.KeySet: IJclCardinalSet;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.Keys: IJclCardinalCollection;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.MapEquals(const AMap: IJclCardinalCardinalMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclCardinalCardinalHashMap.Pack;
var
  I: Integer;
  Bucket: TJclCardinalCardinalHashMapBucket;
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

procedure TJclCardinalCardinalHashMap.PutAll(const AMap: IJclCardinalCardinalMap);
var
  It: IJclCardinalIterator;
  Key: Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalCardinalHashMap.PutValue(Key: Cardinal; Value: Cardinal);
var
  Index: Integer;
  Bucket: TJclCardinalCardinalHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, 0)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclCardinalCardinalHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclCardinalCardinalHashMap.Remove(Key: Cardinal): Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalCardinalHashMap.SetCapacity(Value: Integer);
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

function TJclCardinalCardinalHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalCardinalHashMap.Values: IJclCardinalCollection;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalCardinalHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclCardinalCardinalHashMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalCardinalHashMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;

function TJclCardinalCardinalHashMap.KeysEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclCardinalCardinalHashMap.ValuesEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclInt64IntfHashMapBucket } ==========================================

procedure TJclInt64IntfHashMapBucket.FinalizeArrayBeforeMove(var List: TJclInt64IntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclInt64IntfHashMapBucket.InitializeArray(var List: TJclInt64IntfHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclInt64IntfHashMapBucket.InitializeArrayAfterMove(var List: TJclInt64IntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclInt64IntfHashMapBucket.MoveArray(var List: TJclInt64IntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclInt64IntfHashMap } ==========================================

constructor TJclInt64IntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclInt64IntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclInt64IntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclInt64IntfHashMapBucket;
  ADest: TJclInt64IntfHashMap;
  AMap: IJclInt64IntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclInt64IntfHashMap then
    begin
      ADest := TJclInt64IntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclInt64IntfHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclInt64IntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64IntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclInt64IntfHashMap then
    TJclInt64IntfHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclInt64IntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclInt64IntfHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclInt64IntfHashMap.ContainsKey(const Key: Int64): Boolean;
var
  I: Integer;
  Bucket: TJclInt64IntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclInt64IntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64IntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.Extract(const Key: Int64): IInterface;
var
  Bucket: TJclInt64IntfHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclInt64IntfHashMap.GetValue(const Key: Int64): IInterface;
var
  I: Integer;
  Bucket: TJclInt64IntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64IntfHashMap.KeyOfValue(const Value: IInterface): Int64;
var
  I, J: Integer;
  Bucket: TJclInt64IntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.KeySet: IJclInt64Set;
var
  I, J: Integer;
  Bucket: TJclInt64IntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.Keys: IJclInt64Collection;
var
  I, J: Integer;
  Bucket: TJclInt64IntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.MapEquals(const AMap: IJclInt64IntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64IntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclInt64IntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclInt64IntfHashMapBucket;
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

procedure TJclInt64IntfHashMap.PutAll(const AMap: IJclInt64IntfMap);
var
  It: IJclInt64Iterator;
  Key: Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64IntfHashMap.PutValue(const Key: Int64; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclInt64IntfHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclInt64IntfHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclInt64IntfHashMap.Remove(const Key: Int64): IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64IntfHashMap.SetCapacity(Value: Integer);
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

function TJclInt64IntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64IntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclInt64IntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64IntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclInt64IntfHashMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64IntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclInt64IntfHashMap.KeysEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclInt64IntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

//=== { TJclIntfInt64HashMapBucket } ==========================================

procedure TJclIntfInt64HashMapBucket.FinalizeArrayBeforeMove(var List: TJclIntfInt64HashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclIntfInt64HashMapBucket.InitializeArray(var List: TJclIntfInt64HashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclIntfInt64HashMapBucket.InitializeArrayAfterMove(var List: TJclIntfInt64HashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclIntfInt64HashMapBucket.MoveArray(var List: TJclIntfInt64HashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclIntfInt64HashMap } ==========================================

constructor TJclIntfInt64HashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntfInt64HashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfInt64HashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfInt64HashMapBucket;
  ADest: TJclIntfInt64HashMap;
  AMap: IJclIntfInt64Map;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfInt64HashMap then
    begin
      ADest := TJclIntfInt64HashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfInt64HashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfInt64Map, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfInt64HashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfInt64HashMap then
    TJclIntfInt64HashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntfInt64HashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfInt64HashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclIntfInt64HashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfInt64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfInt64HashMap.ContainsValue(const Value: Int64): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfInt64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.Extract(const Key: IInterface): Int64;
var
  Bucket: TJclIntfInt64HashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := 0;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclIntfInt64HashMap.GetValue(const Key: IInterface): Int64;
var
  I: Integer;
  Bucket: TJclIntfInt64HashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfInt64HashMap.KeyOfValue(const Value: Int64): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfInt64HashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfInt64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.Keys: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfInt64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.MapEquals(const AMap: IJclIntfInt64Map): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfInt64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclIntfInt64HashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfInt64HashMapBucket;
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

procedure TJclIntfInt64HashMap.PutAll(const AMap: IJclIntfInt64Map);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfInt64HashMap.PutValue(const Key: IInterface; const Value: Int64);
var
  Index: Integer;
  Bucket: TJclIntfInt64HashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfInt64HashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfInt64HashMap.Remove(const Key: IInterface): Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfInt64HashMap.SetCapacity(Value: Integer);
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

function TJclIntfInt64HashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfInt64HashMap.Values: IJclInt64Collection;
var
  I, J: Integer;
  Bucket: TJclIntfInt64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfInt64HashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfInt64HashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfInt64HashMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntfInt64HashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := IntfSimpleHashConvert(AInterface);
end;

function TJclIntfInt64HashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

function TJclIntfInt64HashMap.ValuesEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclInt64Int64HashMapBucket } ==========================================

procedure TJclInt64Int64HashMapBucket.InitializeArrayAfterMove(var List: TJclInt64Int64HashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Clean array }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
    else
     FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end;
end;

procedure TJclInt64Int64HashMapBucket.MoveArray(var List: TJclInt64Int64HashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclInt64Int64HashMap } ==========================================

constructor TJclInt64Int64HashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclInt64Int64HashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclInt64Int64HashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclInt64Int64HashMapBucket;
  ADest: TJclInt64Int64HashMap;
  AMap: IJclInt64Int64Map;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclInt64Int64HashMap then
    begin
      ADest := TJclInt64Int64HashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclInt64Int64HashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclInt64Int64Map, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Int64HashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclInt64Int64HashMap then
    TJclInt64Int64HashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclInt64Int64HashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclInt64Int64HashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclInt64Int64HashMap.ContainsKey(const Key: Int64): Boolean;
var
  I: Integer;
  Bucket: TJclInt64Int64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclInt64Int64HashMap.ContainsValue(const Value: Int64): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64Int64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.Extract(const Key: Int64): Int64;
var
  Bucket: TJclInt64Int64HashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := 0;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclInt64Int64HashMap.GetValue(const Key: Int64): Int64;
var
  I: Integer;
  Bucket: TJclInt64Int64HashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64Int64HashMap.KeyOfValue(const Value: Int64): Int64;
var
  I, J: Integer;
  Bucket: TJclInt64Int64HashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.KeySet: IJclInt64Set;
var
  I, J: Integer;
  Bucket: TJclInt64Int64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.Keys: IJclInt64Collection;
var
  I, J: Integer;
  Bucket: TJclInt64Int64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.MapEquals(const AMap: IJclInt64Int64Map): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64Int64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclInt64Int64HashMap.Pack;
var
  I: Integer;
  Bucket: TJclInt64Int64HashMapBucket;
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

procedure TJclInt64Int64HashMap.PutAll(const AMap: IJclInt64Int64Map);
var
  It: IJclInt64Iterator;
  Key: Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Int64HashMap.PutValue(const Key: Int64; const Value: Int64);
var
  Index: Integer;
  Bucket: TJclInt64Int64HashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, 0)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclInt64Int64HashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclInt64Int64HashMap.Remove(const Key: Int64): Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Int64HashMap.SetCapacity(Value: Integer);
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

function TJclInt64Int64HashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64Int64HashMap.Values: IJclInt64Collection;
var
  I, J: Integer;
  Bucket: TJclInt64Int64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Int64HashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclInt64Int64HashMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64Int64HashMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;

function TJclInt64Int64HashMap.KeysEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclInt64Int64HashMap.ValuesEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclPtrIntfHashMapBucket } ==========================================

procedure TJclPtrIntfHashMapBucket.FinalizeArrayBeforeMove(var List: TJclPtrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclPtrIntfHashMapBucket.InitializeArray(var List: TJclPtrIntfHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclPtrIntfHashMapBucket.InitializeArrayAfterMove(var List: TJclPtrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclPtrIntfHashMapBucket.MoveArray(var List: TJclPtrIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclPtrIntfHashMap } ==========================================

constructor TJclPtrIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclPtrIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclPtrIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclPtrIntfHashMapBucket;
  ADest: TJclPtrIntfHashMap;
  AMap: IJclPtrIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclPtrIntfHashMap then
    begin
      ADest := TJclPtrIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclPtrIntfHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclPtrIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclPtrIntfHashMap then
    TJclPtrIntfHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclPtrIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclPtrIntfHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclPtrIntfHashMap.ContainsKey(Key: Pointer): Boolean;
var
  I: Integer;
  Bucket: TJclPtrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclPtrIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.Extract(Key: Pointer): IInterface;
var
  Bucket: TJclPtrIntfHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclPtrIntfHashMap.GetValue(Key: Pointer): IInterface;
var
  I: Integer;
  Bucket: TJclPtrIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrIntfHashMap.KeyOfValue(const Value: IInterface): Pointer;
var
  I, J: Integer;
  Bucket: TJclPtrIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.KeySet: IJclPtrSet;
var
  I, J: Integer;
  Bucket: TJclPtrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.Keys: IJclPtrCollection;
var
  I, J: Integer;
  Bucket: TJclPtrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.MapEquals(const AMap: IJclPtrIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclPtrIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclPtrIntfHashMapBucket;
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

procedure TJclPtrIntfHashMap.PutAll(const AMap: IJclPtrIntfMap);
var
  It: IJclPtrIterator;
  Key: Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrIntfHashMap.PutValue(Key: Pointer; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclPtrIntfHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclPtrIntfHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclPtrIntfHashMap.Remove(Key: Pointer): IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrIntfHashMap.SetCapacity(Value: Integer);
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

function TJclPtrIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclPtrIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclPtrIntfHashMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclPtrIntfHashMap.KeysEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclPtrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

//=== { TJclIntfPtrHashMapBucket } ==========================================

procedure TJclIntfPtrHashMapBucket.FinalizeArrayBeforeMove(var List: TJclIntfPtrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclIntfPtrHashMapBucket.InitializeArray(var List: TJclIntfPtrHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclIntfPtrHashMapBucket.InitializeArrayAfterMove(var List: TJclIntfPtrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclIntfPtrHashMapBucket.MoveArray(var List: TJclIntfPtrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclIntfPtrHashMap } ==========================================

constructor TJclIntfPtrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntfPtrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfPtrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfPtrHashMapBucket;
  ADest: TJclIntfPtrHashMap;
  AMap: IJclIntfPtrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfPtrHashMap then
    begin
      ADest := TJclIntfPtrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfPtrHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfPtrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfPtrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfPtrHashMap then
    TJclIntfPtrHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntfPtrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfPtrHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclIntfPtrHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfPtrHashMap.ContainsValue(Value: Pointer): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.Extract(const Key: IInterface): Pointer;
var
  Bucket: TJclIntfPtrHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclIntfPtrHashMap.GetValue(const Key: IInterface): Pointer;
var
  I: Integer;
  Bucket: TJclIntfPtrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfPtrHashMap.KeyOfValue(Value: Pointer): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfPtrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.Keys: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.MapEquals(const AMap: IJclIntfPtrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclIntfPtrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfPtrHashMapBucket;
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

procedure TJclIntfPtrHashMap.PutAll(const AMap: IJclIntfPtrMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfPtrHashMap.PutValue(const Key: IInterface; Value: Pointer);
var
  Index: Integer;
  Bucket: TJclIntfPtrHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfPtrHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfPtrHashMap.Remove(const Key: IInterface): Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfPtrHashMap.SetCapacity(Value: Integer);
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

function TJclIntfPtrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfPtrHashMap.Values: IJclPtrCollection;
var
  I, J: Integer;
  Bucket: TJclIntfPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfPtrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfPtrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfPtrHashMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;

function TJclIntfPtrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := IntfSimpleHashConvert(AInterface);
end;

function TJclIntfPtrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := IntfSimpleEqualityCompare(A, B);
end;

function TJclIntfPtrHashMap.ValuesEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclPtrPtrHashMapBucket } ==========================================

procedure TJclPtrPtrHashMapBucket.InitializeArrayAfterMove(var List: TJclPtrPtrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Clean array }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
    else
     FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end;
end;

procedure TJclPtrPtrHashMapBucket.MoveArray(var List: TJclPtrPtrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclPtrPtrHashMap } ==========================================

constructor TJclPtrPtrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclPtrPtrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclPtrPtrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclPtrPtrHashMapBucket;
  ADest: TJclPtrPtrHashMap;
  AMap: IJclPtrPtrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclPtrPtrHashMap then
    begin
      ADest := TJclPtrPtrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclPtrPtrHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclPtrPtrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrPtrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclPtrPtrHashMap then
    TJclPtrPtrHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclPtrPtrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclPtrPtrHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclPtrPtrHashMap.ContainsKey(Key: Pointer): Boolean;
var
  I: Integer;
  Bucket: TJclPtrPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclPtrPtrHashMap.ContainsValue(Value: Pointer): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.Extract(Key: Pointer): Pointer;
var
  Bucket: TJclPtrPtrHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclPtrPtrHashMap.GetValue(Key: Pointer): Pointer;
var
  I: Integer;
  Bucket: TJclPtrPtrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrPtrHashMap.KeyOfValue(Value: Pointer): Pointer;
var
  I, J: Integer;
  Bucket: TJclPtrPtrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.KeySet: IJclPtrSet;
var
  I, J: Integer;
  Bucket: TJclPtrPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.Keys: IJclPtrCollection;
var
  I, J: Integer;
  Bucket: TJclPtrPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.MapEquals(const AMap: IJclPtrPtrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclPtrPtrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclPtrPtrHashMapBucket;
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

procedure TJclPtrPtrHashMap.PutAll(const AMap: IJclPtrPtrMap);
var
  It: IJclPtrIterator;
  Key: Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrPtrHashMap.PutValue(Key: Pointer; Value: Pointer);
var
  Index: Integer;
  Bucket: TJclPtrPtrHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclPtrPtrHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclPtrPtrHashMap.Remove(Key: Pointer): Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrPtrHashMap.SetCapacity(Value: Integer);
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

function TJclPtrPtrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrPtrHashMap.Values: IJclPtrCollection;
var
  I, J: Integer;
  Bucket: TJclPtrPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrPtrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclPtrPtrHashMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrPtrHashMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;

function TJclPtrPtrHashMap.KeysEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclPtrPtrHashMap.ValuesEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclIntfHashMapBucket } ==========================================

procedure TJclIntfHashMapBucket.FinalizeArrayBeforeMove(var List: TJclIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclIntfHashMapBucket.InitializeArray(var List: TJclIntfHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclIntfHashMapBucket.InitializeArrayAfterMove(var List: TJclIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclIntfHashMapBucket.MoveArray(var List: TJclIntfHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclIntfHashMap } ==========================================

constructor TJclIntfHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfHashMapBucket;
  ADest: TJclIntfHashMap;
  AMap: IJclIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfHashMap then
    begin
      ADest := TJclIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfHashMap then
    TJclIntfHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclIntfHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.Extract(const Key: IInterface): TObject;
var
  Bucket: TJclIntfHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclIntfHashMap.GetValue(const Key: IInterface): TObject;
var
  I: Integer;
  Bucket: TJclIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfHashMap.KeyOfValue(Value: TObject): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.Keys: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.MapEquals(const AMap: IJclIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfHashMapBucket;
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

procedure TJclIntfHashMap.PutAll(const AMap: IJclIntfMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.PutValue(const Key: IInterface; Value: TObject);
var
  Index: Integer;
  Bucket: TJclIntfHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfHashMap.Remove(const Key: IInterface): TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.SetCapacity(Value: Integer);
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

function TJclIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclIntfHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclIntfHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;

function TJclIntfHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclIntfHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntfHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := SimpleEqualityCompare(A, B);
end;

//=== { TJclAnsiStrHashMapBucket } ==========================================

procedure TJclAnsiStrHashMapBucket.FinalizeArrayBeforeMove(var List: TJclAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclAnsiStrHashMapBucket.InitializeArray(var List: TJclAnsiStrHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclAnsiStrHashMapBucket.InitializeArrayAfterMove(var List: TJclAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclAnsiStrHashMapBucket.MoveArray(var List: TJclAnsiStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclAnsiStrHashMap } ==========================================

constructor TJclAnsiStrHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclAnsiStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclAnsiStrHashMapBucket;
  ADest: TJclAnsiStrHashMap;
  AMap: IJclAnsiStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclAnsiStrHashMap then
    begin
      ADest := TJclAnsiStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclAnsiStrHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclAnsiStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrHashMap then
    TJclAnsiStrHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclAnsiStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclAnsiStrHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclAnsiStrHashMap.ContainsKey(const Key: AnsiString): Boolean;
var
  I: Integer;
  Bucket: TJclAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclAnsiStrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.Extract(const Key: AnsiString): TObject;
var
  Bucket: TJclAnsiStrHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclAnsiStrHashMap.GetValue(const Key: AnsiString): TObject;
var
  I: Integer;
  Bucket: TJclAnsiStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrHashMap.KeyOfValue(Value: TObject): AnsiString;
var
  I, J: Integer;
  Bucket: TJclAnsiStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.KeySet: IJclAnsiStrSet;
var
  I, J: Integer;
  Bucket: TJclAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.Keys: IJclAnsiStrCollection;
var
  I, J: Integer;
  Bucket: TJclAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.MapEquals(const AMap: IJclAnsiStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclAnsiStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclAnsiStrHashMapBucket;
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

procedure TJclAnsiStrHashMap.PutAll(const AMap: IJclAnsiStrMap);
var
  It: IJclAnsiStrIterator;
  Key: AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashMap.PutValue(const Key: AnsiString; Value: TObject);
var
  Index: Integer;
  Bucket: TJclAnsiStrHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclAnsiStrHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclAnsiStrHashMap.Remove(const Key: AnsiString): TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashMap.SetCapacity(Value: Integer);
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

function TJclAnsiStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclAnsiStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;

function TJclAnsiStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclAnsiStrHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclAnsiStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := SimpleEqualityCompare(A, B);
end;

//=== { TJclWideStrHashMapBucket } ==========================================

procedure TJclWideStrHashMapBucket.FinalizeArrayBeforeMove(var List: TJclWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclWideStrHashMapBucket.InitializeArray(var List: TJclWideStrHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclWideStrHashMapBucket.InitializeArrayAfterMove(var List: TJclWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclWideStrHashMapBucket.MoveArray(var List: TJclWideStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclWideStrHashMap } ==========================================

constructor TJclWideStrHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclWideStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclWideStrHashMapBucket;
  ADest: TJclWideStrHashMap;
  AMap: IJclWideStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclWideStrHashMap then
    begin
      ADest := TJclWideStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclWideStrHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclWideStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrHashMap then
    TJclWideStrHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclWideStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclWideStrHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclWideStrHashMap.ContainsKey(const Key: WideString): Boolean;
var
  I: Integer;
  Bucket: TJclWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclWideStrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.Extract(const Key: WideString): TObject;
var
  Bucket: TJclWideStrHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclWideStrHashMap.GetValue(const Key: WideString): TObject;
var
  I: Integer;
  Bucket: TJclWideStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrHashMap.KeyOfValue(Value: TObject): WideString;
var
  I, J: Integer;
  Bucket: TJclWideStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.KeySet: IJclWideStrSet;
var
  I, J: Integer;
  Bucket: TJclWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.Keys: IJclWideStrCollection;
var
  I, J: Integer;
  Bucket: TJclWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.MapEquals(const AMap: IJclWideStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclWideStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclWideStrHashMapBucket;
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

procedure TJclWideStrHashMap.PutAll(const AMap: IJclWideStrMap);
var
  It: IJclWideStrIterator;
  Key: WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashMap.PutValue(const Key: WideString; Value: TObject);
var
  Index: Integer;
  Bucket: TJclWideStrHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclWideStrHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclWideStrHashMap.Remove(const Key: WideString): TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashMap.SetCapacity(Value: Integer);
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

function TJclWideStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclWideStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclWideStrHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;

function TJclWideStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclWideStrHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclWideStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := SimpleEqualityCompare(A, B);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrHashMapBucket } ==========================================

procedure TJclUnicodeStrHashMapBucket.FinalizeArrayBeforeMove(var List: TJclUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclUnicodeStrHashMapBucket.InitializeArray(var List: TJclUnicodeStrHashMapEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclUnicodeStrHashMapBucket.InitializeArrayAfterMove(var List: TJclUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclUnicodeStrHashMapBucket.MoveArray(var List: TJclUnicodeStrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;


{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrHashMap } ==========================================

constructor TJclUnicodeStrHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclUnicodeStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclUnicodeStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclUnicodeStrHashMapBucket;
  ADest: TJclUnicodeStrHashMap;
  AMap: IJclUnicodeStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclUnicodeStrHashMap then
    begin
      ADest := TJclUnicodeStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclUnicodeStrHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclUnicodeStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclUnicodeStrHashMap then
    TJclUnicodeStrHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclUnicodeStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclUnicodeStrHashMap.ContainsKey(const Key: UnicodeString): Boolean;
var
  I: Integer;
  Bucket: TJclUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclUnicodeStrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashMap.Extract(const Key: UnicodeString): TObject;
var
  Bucket: TJclUnicodeStrHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclUnicodeStrHashMap.GetValue(const Key: UnicodeString): TObject;
var
  I: Integer;
  Bucket: TJclUnicodeStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclUnicodeStrHashMap.KeyOfValue(Value: TObject): UnicodeString;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashMap.KeySet: IJclUnicodeStrSet;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclUnicodeStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashMap.Keys: IJclUnicodeStrCollection;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclUnicodeStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashMap.MapEquals(const AMap: IJclUnicodeStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclUnicodeStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclUnicodeStrHashMapBucket;
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

procedure TJclUnicodeStrHashMap.PutAll(const AMap: IJclUnicodeStrMap);
var
  It: IJclUnicodeStrIterator;
  Key: UnicodeString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrHashMap.PutValue(const Key: UnicodeString; Value: TObject);
var
  Index: Integer;
  Bucket: TJclUnicodeStrHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclUnicodeStrHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclUnicodeStrHashMap.Remove(const Key: UnicodeString): TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrHashMap.SetCapacity(Value: Integer);
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

function TJclUnicodeStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclUnicodeStrHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclUnicodeStrHashMap.FreeKey(var Key: UnicodeString): UnicodeString;
begin
  Result := Key;
  Key := '';
end;

function TJclUnicodeStrHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;

function TJclUnicodeStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclUnicodeStrHashMap.KeysEqual(const A, B: UnicodeString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclUnicodeStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := SimpleEqualityCompare(A, B);
end;

{$ENDIF SUPPORTS_UNICODE_STRING}

//=== { TJclSingleHashMapBucket } ==========================================

procedure TJclSingleHashMapBucket.InitializeArrayAfterMove(var List: TJclSingleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Clean array }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
    else
     FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end;
end;

procedure TJclSingleHashMapBucket.MoveArray(var List: TJclSingleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclSingleHashMap } ==========================================

constructor TJclSingleHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclSingleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSingleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclSingleHashMapBucket;
  ADest: TJclSingleHashMap;
  AMap: IJclSingleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclSingleHashMap then
    begin
      ADest := TJclSingleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclSingleHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclSingleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclSingleHashMap then
    TJclSingleHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclSingleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclSingleHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclSingleHashMap.ContainsKey(const Key: Single): Boolean;
var
  I: Integer;
  Bucket: TJclSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclSingleHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.Extract(const Key: Single): TObject;
var
  Bucket: TJclSingleHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclSingleHashMap.GetValue(const Key: Single): TObject;
var
  I: Integer;
  Bucket: TJclSingleHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleHashMap.KeyOfValue(Value: TObject): Single;
var
  I, J: Integer;
  Bucket: TJclSingleHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.KeySet: IJclSingleSet;
var
  I, J: Integer;
  Bucket: TJclSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.Keys: IJclSingleCollection;
var
  I, J: Integer;
  Bucket: TJclSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.MapEquals(const AMap: IJclSingleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclSingleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclSingleHashMapBucket;
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

procedure TJclSingleHashMap.PutAll(const AMap: IJclSingleMap);
var
  It: IJclSingleIterator;
  Key: Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashMap.PutValue(const Key: Single; Value: TObject);
var
  Index: Integer;
  Bucket: TJclSingleHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclSingleHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclSingleHashMap.Remove(const Key: Single): TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashMap.SetCapacity(Value: Integer);
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

function TJclSingleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclSingleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclSingleHashMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;

function TJclSingleHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclSingleHashMap.KeysEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclSingleHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := SimpleEqualityCompare(A, B);
end;

//=== { TJclDoubleHashMapBucket } ==========================================

procedure TJclDoubleHashMapBucket.InitializeArrayAfterMove(var List: TJclDoubleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Clean array }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
    else
     FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end;
end;

procedure TJclDoubleHashMapBucket.MoveArray(var List: TJclDoubleHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclDoubleHashMap } ==========================================

constructor TJclDoubleHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclDoubleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclDoubleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclDoubleHashMapBucket;
  ADest: TJclDoubleHashMap;
  AMap: IJclDoubleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclDoubleHashMap then
    begin
      ADest := TJclDoubleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclDoubleHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclDoubleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclDoubleHashMap then
    TJclDoubleHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclDoubleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclDoubleHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclDoubleHashMap.ContainsKey(const Key: Double): Boolean;
var
  I: Integer;
  Bucket: TJclDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclDoubleHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.Extract(const Key: Double): TObject;
var
  Bucket: TJclDoubleHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclDoubleHashMap.GetValue(const Key: Double): TObject;
var
  I: Integer;
  Bucket: TJclDoubleHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleHashMap.KeyOfValue(Value: TObject): Double;
var
  I, J: Integer;
  Bucket: TJclDoubleHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.KeySet: IJclDoubleSet;
var
  I, J: Integer;
  Bucket: TJclDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.Keys: IJclDoubleCollection;
var
  I, J: Integer;
  Bucket: TJclDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.MapEquals(const AMap: IJclDoubleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclDoubleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclDoubleHashMapBucket;
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

procedure TJclDoubleHashMap.PutAll(const AMap: IJclDoubleMap);
var
  It: IJclDoubleIterator;
  Key: Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashMap.PutValue(const Key: Double; Value: TObject);
var
  Index: Integer;
  Bucket: TJclDoubleHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclDoubleHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclDoubleHashMap.Remove(const Key: Double): TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashMap.SetCapacity(Value: Integer);
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

function TJclDoubleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclDoubleHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclDoubleHashMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;

function TJclDoubleHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclDoubleHashMap.KeysEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclDoubleHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := SimpleEqualityCompare(A, B);
end;

//=== { TJclExtendedHashMapBucket } ==========================================

procedure TJclExtendedHashMapBucket.InitializeArrayAfterMove(var List: TJclExtendedHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Clean array }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
    else
     FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end;
end;

procedure TJclExtendedHashMapBucket.MoveArray(var List: TJclExtendedHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclExtendedHashMap } ==========================================

constructor TJclExtendedHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclExtendedHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclExtendedHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclExtendedHashMapBucket;
  ADest: TJclExtendedHashMap;
  AMap: IJclExtendedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclExtendedHashMap then
    begin
      ADest := TJclExtendedHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclExtendedHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclExtendedMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclExtendedHashMap then
    TJclExtendedHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclExtendedHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclExtendedHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclExtendedHashMap.ContainsKey(const Key: Extended): Boolean;
var
  I: Integer;
  Bucket: TJclExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclExtendedHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.Extract(const Key: Extended): TObject;
var
  Bucket: TJclExtendedHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclExtendedHashMap.GetValue(const Key: Extended): TObject;
var
  I: Integer;
  Bucket: TJclExtendedHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedHashMap.KeyOfValue(Value: TObject): Extended;
var
  I, J: Integer;
  Bucket: TJclExtendedHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.KeySet: IJclExtendedSet;
var
  I, J: Integer;
  Bucket: TJclExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.Keys: IJclExtendedCollection;
var
  I, J: Integer;
  Bucket: TJclExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.MapEquals(const AMap: IJclExtendedMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclExtendedHashMap.Pack;
var
  I: Integer;
  Bucket: TJclExtendedHashMapBucket;
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

procedure TJclExtendedHashMap.PutAll(const AMap: IJclExtendedMap);
var
  It: IJclExtendedIterator;
  Key: Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashMap.PutValue(const Key: Extended; Value: TObject);
var
  Index: Integer;
  Bucket: TJclExtendedHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclExtendedHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclExtendedHashMap.Remove(const Key: Extended): TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashMap.SetCapacity(Value: Integer);
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

function TJclExtendedHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclExtendedHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclExtendedHashMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;

function TJclExtendedHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclExtendedHashMap.KeysEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclExtendedHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := SimpleEqualityCompare(A, B);
end;

//=== { TJclIntegerHashMapBucket } ==========================================

procedure TJclIntegerHashMapBucket.InitializeArrayAfterMove(var List: TJclIntegerHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Clean array }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
    else
     FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end;
end;

procedure TJclIntegerHashMapBucket.MoveArray(var List: TJclIntegerHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclIntegerHashMap } ==========================================

constructor TJclIntegerHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclIntegerHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntegerHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntegerHashMapBucket;
  ADest: TJclIntegerHashMap;
  AMap: IJclIntegerMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntegerHashMap then
    begin
      ADest := TJclIntegerHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntegerHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntegerMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntegerHashMap then
    TJclIntegerHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclIntegerHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntegerHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclIntegerHashMap.ContainsKey(Key: Integer): Boolean;
var
  I: Integer;
  Bucket: TJclIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntegerHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.Extract(Key: Integer): TObject;
var
  Bucket: TJclIntegerHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclIntegerHashMap.GetValue(Key: Integer): TObject;
var
  I: Integer;
  Bucket: TJclIntegerHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerHashMap.KeyOfValue(Value: TObject): Integer;
var
  I, J: Integer;
  Bucket: TJclIntegerHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.KeySet: IJclIntegerSet;
var
  I, J: Integer;
  Bucket: TJclIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.Keys: IJclIntegerCollection;
var
  I, J: Integer;
  Bucket: TJclIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.MapEquals(const AMap: IJclIntegerMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclIntegerHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntegerHashMapBucket;
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

procedure TJclIntegerHashMap.PutAll(const AMap: IJclIntegerMap);
var
  It: IJclIntegerIterator;
  Key: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashMap.PutValue(Key: Integer; Value: TObject);
var
  Index: Integer;
  Bucket: TJclIntegerHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntegerHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntegerHashMap.Remove(Key: Integer): TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashMap.SetCapacity(Value: Integer);
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

function TJclIntegerHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclIntegerHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclIntegerHashMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;

function TJclIntegerHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclIntegerHashMap.KeysEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntegerHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := SimpleEqualityCompare(A, B);
end;

//=== { TJclCardinalHashMapBucket } ==========================================

procedure TJclCardinalHashMapBucket.InitializeArrayAfterMove(var List: TJclCardinalHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Clean array }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
    else
     FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end;
end;

procedure TJclCardinalHashMapBucket.MoveArray(var List: TJclCardinalHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclCardinalHashMap } ==========================================

constructor TJclCardinalHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclCardinalHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclCardinalHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclCardinalHashMapBucket;
  ADest: TJclCardinalHashMap;
  AMap: IJclCardinalMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclCardinalHashMap then
    begin
      ADest := TJclCardinalHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclCardinalHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclCardinalMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclCardinalHashMap then
    TJclCardinalHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclCardinalHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclCardinalHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclCardinalHashMap.ContainsKey(Key: Cardinal): Boolean;
var
  I: Integer;
  Bucket: TJclCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclCardinalHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.Extract(Key: Cardinal): TObject;
var
  Bucket: TJclCardinalHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclCardinalHashMap.GetValue(Key: Cardinal): TObject;
var
  I: Integer;
  Bucket: TJclCardinalHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalHashMap.KeyOfValue(Value: TObject): Cardinal;
var
  I, J: Integer;
  Bucket: TJclCardinalHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.KeySet: IJclCardinalSet;
var
  I, J: Integer;
  Bucket: TJclCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.Keys: IJclCardinalCollection;
var
  I, J: Integer;
  Bucket: TJclCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.MapEquals(const AMap: IJclCardinalMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclCardinalHashMap.Pack;
var
  I: Integer;
  Bucket: TJclCardinalHashMapBucket;
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

procedure TJclCardinalHashMap.PutAll(const AMap: IJclCardinalMap);
var
  It: IJclCardinalIterator;
  Key: Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashMap.PutValue(Key: Cardinal; Value: TObject);
var
  Index: Integer;
  Bucket: TJclCardinalHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclCardinalHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclCardinalHashMap.Remove(Key: Cardinal): TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashMap.SetCapacity(Value: Integer);
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

function TJclCardinalHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclCardinalHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclCardinalHashMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;

function TJclCardinalHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclCardinalHashMap.KeysEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclCardinalHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := SimpleEqualityCompare(A, B);
end;

//=== { TJclInt64HashMapBucket } ==========================================

procedure TJclInt64HashMapBucket.InitializeArrayAfterMove(var List: TJclInt64HashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Clean array }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
    else
     FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end;
end;

procedure TJclInt64HashMapBucket.MoveArray(var List: TJclInt64HashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclInt64HashMap } ==========================================

constructor TJclInt64HashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclInt64HashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclInt64HashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclInt64HashMapBucket;
  ADest: TJclInt64HashMap;
  AMap: IJclInt64Map;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclInt64HashMap then
    begin
      ADest := TJclInt64HashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclInt64HashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclInt64Map, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclInt64HashMap then
    TJclInt64HashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclInt64HashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclInt64HashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclInt64HashMap.ContainsKey(const Key: Int64): Boolean;
var
  I: Integer;
  Bucket: TJclInt64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclInt64HashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.Extract(const Key: Int64): TObject;
var
  Bucket: TJclInt64HashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclInt64HashMap.GetValue(const Key: Int64): TObject;
var
  I: Integer;
  Bucket: TJclInt64HashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64HashMap.KeyOfValue(Value: TObject): Int64;
var
  I, J: Integer;
  Bucket: TJclInt64HashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.KeySet: IJclInt64Set;
var
  I, J: Integer;
  Bucket: TJclInt64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.Keys: IJclInt64Collection;
var
  I, J: Integer;
  Bucket: TJclInt64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.MapEquals(const AMap: IJclInt64Map): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclInt64HashMap.Pack;
var
  I: Integer;
  Bucket: TJclInt64HashMapBucket;
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

procedure TJclInt64HashMap.PutAll(const AMap: IJclInt64Map);
var
  It: IJclInt64Iterator;
  Key: Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashMap.PutValue(const Key: Int64; Value: TObject);
var
  Index: Integer;
  Bucket: TJclInt64HashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclInt64HashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclInt64HashMap.Remove(const Key: Int64): TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashMap.SetCapacity(Value: Integer);
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

function TJclInt64HashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64HashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclInt64HashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64HashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclInt64HashMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64HashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;

function TJclInt64HashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclInt64HashMap.KeysEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclInt64HashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := SimpleEqualityCompare(A, B);
end;

//=== { TJclPtrHashMapBucket } ==========================================

procedure TJclPtrHashMapBucket.InitializeArrayAfterMove(var List: TJclPtrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Clean array }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
    else
     FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end;
end;

procedure TJclPtrHashMapBucket.MoveArray(var List: TJclPtrHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclPtrHashMap } ==========================================

constructor TJclPtrHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclPtrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclPtrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclPtrHashMapBucket;
  ADest: TJclPtrHashMap;
  AMap: IJclPtrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclPtrHashMap then
    begin
      ADest := TJclPtrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclPtrHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclPtrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclPtrHashMap then
    TJclPtrHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclPtrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclPtrHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclPtrHashMap.ContainsKey(Key: Pointer): Boolean;
var
  I: Integer;
  Bucket: TJclPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclPtrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.Extract(Key: Pointer): TObject;
var
  Bucket: TJclPtrHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclPtrHashMap.GetValue(Key: Pointer): TObject;
var
  I: Integer;
  Bucket: TJclPtrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrHashMap.KeyOfValue(Value: TObject): Pointer;
var
  I, J: Integer;
  Bucket: TJclPtrHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.KeySet: IJclPtrSet;
var
  I, J: Integer;
  Bucket: TJclPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.Keys: IJclPtrCollection;
var
  I, J: Integer;
  Bucket: TJclPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.MapEquals(const AMap: IJclPtrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclPtrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclPtrHashMapBucket;
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

procedure TJclPtrHashMap.PutAll(const AMap: IJclPtrMap);
var
  It: IJclPtrIterator;
  Key: Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashMap.PutValue(Key: Pointer; Value: TObject);
var
  Index: Integer;
  Bucket: TJclPtrHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclPtrHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclPtrHashMap.Remove(Key: Pointer): TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashMap.SetCapacity(Value: Integer);
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

function TJclPtrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclPtrHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclPtrHashMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;

function TJclPtrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclPtrHashMap.KeysEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclPtrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := SimpleEqualityCompare(A, B);
end;

//=== { TJclHashMapBucket } ==========================================

procedure TJclHashMapBucket.InitializeArrayAfterMove(var List: TJclHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Clean array }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
    else
     FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
  end;
end;

procedure TJclHashMapBucket.MoveArray(var List: TJclHashMapEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclHashMap } ==========================================

constructor TJclHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create;
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclHashMapBucket;
  ADest: TJclHashMap;
  AMap: IJclMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclHashMap then
    begin
      ADest := TJclHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclHashMapBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclHashMap then
    TJclHashMap(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclHashMapBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclHashMap.ContainsKey(Key: TObject): Boolean;
var
  I: Integer;
  Bucket: TJclHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.Extract(Key: TObject): TObject;
var
  Bucket: TJclHashMapBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := nil;
          FreeKey(Bucket.Entries[I].Key);
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

function TJclHashMap.GetValue(Key: TObject): TObject;
var
  I: Integer;
  Bucket: TJclHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclHashMap.KeyOfValue(Value: TObject): TObject;
var
  I, J: Integer;
  Bucket: TJclHashMapBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.KeySet: IJclSet;
var
  I, J: Integer;
  Bucket: TJclHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArraySet.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.Keys: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.MapEquals(const AMap: IJclMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclHashMap.Pack;
var
  I: Integer;
  Bucket: TJclHashMapBucket;
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

procedure TJclHashMap.PutAll(const AMap: IJclMap);
var
  It: IJclIterator;
  Key: TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.PutValue(Key: TObject; Value: TObject);
var
  Index: Integer;
  Bucket: TJclHashMapBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclHashMapBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclHashMap.Remove(Key: TObject): TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.SetCapacity(Value: Integer);
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

function TJclHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclHashMapBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMap.Create(FCapacity, False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMap.FreeKey(var Key: TObject): TObject;
begin
  if FOwnsKeys then
  begin
    Result := nil;
    FreeAndNil(Key);
  end
  else
  begin
    Result := Key;
    Key := nil;
  end;
end;

function TJclHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;

function TJclHashMap.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

function TJclHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclHashMap.Hash(AObject: TObject): Integer;
begin
  Result := SimpleHashConvert(AObject);
end;

function TJclHashMap.KeysEqual(A, B: TObject): Boolean;
begin
  Result := SimpleEqualityCompare(A, B);
end;

function TJclHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := SimpleEqualityCompare(A, B);
end;


{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

//=== { TJclBucket<TKey, TValue> } ==========================================

procedure TJclBucket<TKey, TValue>.FinalizeArrayBeforeMove(var List: THashEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  Assert(Count > 0);
  if FromIndex < ToIndex then
  begin
    if Count > (ToIndex - FromIndex) then
      Finalize(List[FromIndex + Count], ToIndex - FromIndex)
    else
      Finalize(List[ToIndex], Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if Count > (FromIndex - ToIndex) then
      Count := FromIndex - ToIndex;
    Finalize(List[ToIndex], Count)
  end;
end;

procedure TJclBucket<TKey, TValue>.InitializeArray(var List: THashEntryArray; FromIndex, Count: SizeInt);
begin
  {$IFDEF FPC}
  while Count > 0 do
  begin
    Initialize(List[FromIndex]);
    Inc(FromIndex);
    Dec(Count);
  end;
  {$ELSE ~FPC}
  Initialize(List[FromIndex], Count);
  {$ENDIF ~FPC}
end;

procedure TJclBucket<TKey, TValue>.InitializeArrayAfterMove(var List: THashEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  { Keep reference counting working }
  if FromIndex < ToIndex then
  begin
    if (ToIndex - FromIndex) < Count then
      Count := ToIndex - FromIndex;
    InitializeArray(List, FromIndex, Count);
  end
  else
  if FromIndex > ToIndex then
  begin
    if (FromIndex - ToIndex) < Count then
      InitializeArray(List, ToIndex + Count, FromIndex - ToIndex)
    else
      InitializeArray(List, FromIndex, Count);
  end;
end;

procedure TJclBucket<TKey, TValue>.MoveArray(var List: THashEntryArray; FromIndex, ToIndex, Count: SizeInt);
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

//=== { TJclHashMap<TKey, TValue> } ==========================================

constructor TJclHashMap<TKey, TValue>.Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create;

  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashToRangeFunction := JclSimpleHashToRange;
end;

destructor TJclHashMap<TKey, TValue>.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclHashMap<TKey, TValue>.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TBucket;
  ADest: TJclHashMap<TKey, TValue>;
  AMap: IJclMap<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclHashMap<TKey, TValue> then
    begin
      ADest := TJclHashMap<TKey, TValue>(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclMap<TKey, TValue>, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclHashMap<TKey, TValue> then
    TJclHashMap<TKey, TValue>(Dest).FHashToRangeFunction := FHashToRangeFunction;
end;

procedure TJclHashMap<TKey, TValue>.Clear;
var
  I, J: Integer;
  Bucket: TBucket;
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
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
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

function TJclHashMap<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
var
  I: Integer;
  Bucket: TBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclHashMap<TKey, TValue>.ContainsValue(const Value: TValue): Boolean;
var
  I, J: Integer;
  Bucket: TBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.Extract(const Key: TKey): TValue;
var
  Bucket: TBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Default(TValue);
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Bucket.Entries[I].Value := Default(TValue);
          FreeKey(Bucket.Entries[I].Key);
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

function TJclHashMap<TKey, TValue>.GetValue(const Key: TKey): TValue;
var
  I: Integer;
  Bucket: TBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := Default(TValue);
    Bucket := FBuckets[FHashToRangeFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclHashMap<TKey, TValue>.KeyOfValue(const Value: TValue): TKey;
var
  I, J: Integer;
  Bucket: TBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := Default(TKey);
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.KeySet: IJclSet<TKey>;
var
  I, J: Integer;
  Bucket: TBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyArraySet(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.Keys: IJclCollection<TKey>;
var
  I, J: Integer;
  Bucket: TBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyKeyList(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.MapEquals(const AMap: IJclMap<TKey, TValue>): Boolean;
var
  I, J: Integer;
  Bucket: TBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
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

procedure TJclHashMap<TKey, TValue>.Pack;
var
  I: Integer;
  Bucket: TBucket;
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

procedure TJclHashMap<TKey, TValue>.PutAll(const AMap: IJclMap<TKey, TValue>);
var
  It: IJclIterator<TKey>;
  Key: TKey;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.PutValue(const Key: TKey; const Value: TValue);
var
  Index: Integer;
  Bucket: TBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, Default(TKey)) and not ValuesEqual(Value, Default(TValue))) then
    begin
      Index := FHashToRangeFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclHashMap<TKey, TValue>.Remove(const Key: TKey): TValue;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Extract(Key);
    Result := FreeValue(Result);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.SetCapacity(Value: Integer);
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

function TJclHashMap<TKey, TValue>.Size: Integer;
begin
  Result := FSize;
end;

function TJclHashMap<TKey, TValue>.Values: IJclCollection<TValue>;
var
  I, J: Integer;
  Bucket: TBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyArrayList(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

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

