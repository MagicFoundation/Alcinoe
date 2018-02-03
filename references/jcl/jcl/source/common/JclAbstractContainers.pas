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
{ The Original Code is AbstractContainer.pas and DCL_Util.pas.                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Daniele Teti (dade2004)                                                                        }
{   Robert Marquardt (marquardt)                                                                   }
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

unit JclAbstractContainers;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  {$IFDEF HAS_UNITSCOPE}
  System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase, JclContainerIntf, JclSynch, JclSysUtils,
  JclWideStrings,
  JclAnsiStrings;

type
  // (OF) was moved to JclSysUtils
  // TJclIntfCriticalSection = JclSysUtils.TJclIntfCriticalSection;

  TJclAbstractLockable = class(TInterfacedObject {$IFDEF THREADSAFE}, IJclLockable {$ENDIF THREADSAFE})
  {$IFDEF THREADSAFE}
  protected
    FThreadSafe: Boolean;
    FSyncReaderWriter: TJclMultiReadExclusiveWrite;
  public
    constructor Create;
    destructor Destroy; override;
    property SyncReaderWriter: TJclMultiReadExclusiveWrite read FSyncReaderWriter;
    { IJclLockable }
    procedure ReadLock;
    procedure ReadUnlock;
    procedure WriteLock;
    procedure WriteUnlock;
  {$ENDIF THREADSAFE}
  end;

  TJclAbstractContainerBase = class(TJclAbstractLockable, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclBaseContainer)
  protected
    FAllowDefaultElements: Boolean;
    FDuplicates: TDuplicates;
    FRemoveSingleElement: Boolean;
    FReturnDefaultElements: Boolean;
    FReadOnly: Boolean;
    FCapacity: Integer;
    FSize: Integer;
    FAutoGrowParameter: Integer;
    FAutoGrowStrategy: TJclAutoGrowStrategy;
    FAutoPackParameter: Integer;
    FAutoPackStrategy: TJclAutoPackStrategy;
    procedure AutoGrow; virtual;
    procedure AutoPack; virtual;
    function CheckDuplicate: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; virtual; abstract;
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); virtual;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); virtual;
  public
    constructor Create;
    { IJclBaseContainer }
    procedure Assign(const Source: IJclBaseContainer);
    procedure AssignTo(const Dest: IJclBaseContainer);
    function GetAllowDefaultElements: Boolean; virtual;
    function GetContainerReference: TObject;
    function GetDuplicates: TDuplicates; virtual;
    function GetReadOnly: Boolean; virtual;
    function GetRemoveSingleElement: Boolean; virtual;
    function GetReturnDefaultElements: Boolean; virtual;
    function GetThreadSafe: Boolean; virtual;
    procedure SetAllowDefaultElements(Value: Boolean); virtual;
    procedure SetDuplicates(Value: TDuplicates); virtual;
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetRemoveSingleElement(Value: Boolean); virtual;
    procedure SetReturnDefaultElements(Value: Boolean); virtual;
    procedure SetThreadSafe(Value: Boolean); virtual;
    property AllowDefaultElements: Boolean read GetAllowDefaultElements write SetAllowDefaultElements;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property RemoveSingleElement: Boolean read GetRemoveSingleElement write SetRemoveSingleElement;
    property ReturnDefaultElements: Boolean read GetReturnDefaultElements write SetReturnDefaultElements;
    property ThreadSafe: Boolean read GetThreadSafe write SetThreadSafe;
    { IJclCloneable }
    function ObjectClone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    // IJclGrowable is not in interface list because some descendants won't use this code
    { IJclGrowable }
    function CalcGrowCapacity(ACapacity, ASize: Integer): Integer; virtual;
    function GetAutoGrowParameter: Integer; virtual;
    function GetAutoGrowStrategy: TJclAutoGrowStrategy; virtual;
    procedure Grow; virtual;
    procedure SetAutoGrowParameter(Value: Integer); virtual;
    procedure SetAutoGrowStrategy(Value: TJclAutoGrowStrategy); virtual;
    property AutoGrowParameter: Integer read GetAutoGrowParameter write SetAutoGrowParameter;
    property AutoGrowStrategy: TJclAutoGrowStrategy read GetAutoGrowStrategy write SetAutoGrowStrategy;
    // IJclPackable is not in interface list because some descendants won't use this code
    { IJclPackable }
    function CalcPackCapacity(ACapacity, ASize: Integer): Integer; virtual;
    function GetAutoPackParameter: Integer; virtual;
    function GetAutoPackStrategy: TJclAutoPackStrategy; virtual;
    function GetCapacity: Integer; virtual;
    procedure Pack; virtual;
    procedure SetAutoPackParameter(Value: Integer); virtual;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); virtual;
    procedure SetCapacity(Value: Integer); virtual;
    property AutoPackParameter: Integer read GetAutoPackParameter write SetAutoPackParameter;
    property AutoPackStrategy: TJclAutoPackStrategy read GetAutoPackStrategy write SetAutoPackStrategy;
  end;

  TJclAbstractIterator = class(TJclAbstractLockable, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclAbstractIterator)
  private
    FValid: Boolean;
  protected
    procedure CheckValid;
    function CreateEmptyIterator: TJclAbstractIterator; virtual; abstract;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); virtual;
  public
    constructor Create(AValid: Boolean);
    property Valid: Boolean read FValid write FValid;
    { IJclAbstractIterator }
    procedure Assign(const Source: IJclAbstractIterator);
    procedure AssignTo(const Dest: IJclAbstractIterator);
    function GetIteratorReference: TObject;
    { IJclCloneable }
    function ObjectClone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
  end;

  TJclIntfAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclBaseContainer, IJclIntfContainer,
    IJclIntfOwner, IJclIntfEqualityComparer, IJclIntfComparer, IJclIntfHashConverter)
  protected
    FEqualityCompare: TIntfEqualityCompare;
    FCompare: TIntfCompare;
    FHashConvert: TIntfHashConvert;
    FOnFreeObject: TFreeIntfEvent;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    { IJclIntfOwner }
    function GetOnFreeObject: TFreeIntfEvent;
    function FreeObject(var AInterface: IInterface): IInterface; virtual;
    procedure SetOnFreeObject(Value: TFreeIntfEvent);
    property OnFreeObject: TFreeIntfEvent read GetOnFreeObject write SetOnFreeObject;
    { IJclIntfEqualityComparer }
    function GetEqualityCompare: TIntfEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TIntfEqualityCompare); virtual;
    function ItemsEqual(const A, B: IInterface): Boolean; virtual;
    property EqualityCompare: TIntfEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    { IJclIntfComparer }
    function GetCompare: TIntfCompare; virtual;
    procedure SetCompare(Value: TIntfCompare); virtual;
    function ItemsCompare(const A, B: IInterface): Integer; virtual;
    property Compare: TIntfCompare read GetCompare write SetCompare;
    { IJclIntfHashConverter }
    function GetHashConvert: TIntfHashConvert; virtual;
    procedure SetHashConvert(Value: TIntfHashConvert); virtual;
    function Hash(const AInterface: IInterface): Integer; virtual;
    property HashConvert: TIntfHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclStrAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclBaseContainer, IJclStrBaseContainer)
  protected
    FCaseSensitive: Boolean;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    { IJclStrBaseContainer }
    function GetCaseSensitive: Boolean; virtual;
    procedure SetCaseSensitive(Value: Boolean); virtual;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
  end;

  TJclAnsiStrAbstractContainer = class(TJclStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclBaseContainer, IJclStrBaseContainer, IJclAnsiStrContainer,
    IJclAnsiStrOwner, IJclAnsiStrEqualityComparer, IJclAnsiStrComparer, IJclAnsiStrHashConverter)
  protected
    FEncoding: TJclAnsiStrEncoding;
    FEqualityCompare: TAnsiStrEqualityCompare;
    FCompare: TAnsiStrCompare;
    FHashConvert: TAnsiStrHashConvert;
    FOnFreeString: TFreeAnsiStrEvent;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    { IJclAnsiStrOwner }
    function GetOnFreeString: TFreeAnsiStrEvent;
    function FreeString(var AString: AnsiString): AnsiString; virtual;
    procedure SetOnFreeString(Value: TFreeAnsiStrEvent);
    property OnFreeString: TFreeAnsiStrEvent read GetOnFreeString write SetOnFreeString;
    { IJclAnsiStrContainer }
    function GetEncoding: TJclAnsiStrEncoding; virtual;
    procedure SetEncoding(Value: TJclAnsiStrEncoding); virtual;
    property Encoding: TJclAnsiStrEncoding read GetEncoding write SetEncoding;
    { IJclAnsiStrEqualityComparer }
    function GetEqualityCompare: TAnsiStrEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TAnsiStrEqualityCompare); virtual;
    function ItemsEqual(const A, B: AnsiString): Boolean; virtual;
    property EqualityCompare: TAnsiStrEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    { IJclAnsiStrComparer }
    function GetCompare: TAnsiStrCompare; virtual;
    procedure SetCompare(Value: TAnsiStrCompare); virtual;
    function ItemsCompare(const A, B: AnsiString): Integer; virtual;
    property Compare: TAnsiStrCompare read GetCompare write SetCompare;
    { IJclAnsiStrHashConverter }
    function GetHashConvert: TAnsiStrHashConvert; virtual;
    procedure SetHashConvert(Value: TAnsiStrHashConvert); virtual;
    function Hash(const AString: AnsiString): Integer; virtual;
    property HashConvert: TAnsiStrHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclWideStrAbstractContainer = class(TJclStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclBaseContainer, IJclStrBaseContainer, IJclWideStrContainer,
    IJclWideStrOwner, IJclWideStrEqualityComparer, IJclWideStrComparer, IJclWideStrHashConverter)
  protected
    FEncoding: TJclWideStrEncoding;
    FEqualityCompare: TWideStrEqualityCompare;
    FCompare: TWideStrCompare;
    FHashConvert: TWideStrHashConvert;
    FOnFreeString: TFreeWideStrEvent;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    { IJclWideStrOwner }
    function GetOnFreeString: TFreeWideStrEvent;
    function FreeString(var AString: WideString): WideString; virtual;
    procedure SetOnFreeString(Value: TFreeWideStrEvent);
    property OnFreeString: TFreeWideStrEvent read GetOnFreeString write SetOnFreeString;
    { IJclWideStrContainer }
    function GetEncoding: TJclWideStrEncoding; virtual;
    procedure SetEncoding(Value: TJclWideStrEncoding); virtual;
    property Encoding: TJclWideStrEncoding read GetEncoding write SetEncoding;
    { IJclWideStrEqualityComparer }
    function GetEqualityCompare: TWideStrEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TWideStrEqualityCompare); virtual;
    function ItemsEqual(const A, B: WideString): Boolean; virtual;
    property EqualityCompare: TWideStrEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    { IJclWideStrComparer }
    function GetCompare: TWideStrCompare; virtual;
    procedure SetCompare(Value: TWideStrCompare); virtual;
    function ItemsCompare(const A, B: WideString): Integer; virtual;
    property Compare: TWideStrCompare read GetCompare write SetCompare;
    { IJclWideStrHashConverter }
    function GetHashConvert: TWideStrHashConvert; virtual;
    procedure SetHashConvert(Value: TWideStrHashConvert); virtual;
    function Hash(const AString: WideString): Integer; virtual;
    property HashConvert: TWideStrHashConvert read GetHashConvert write SetHashConvert;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrAbstractContainer = class(TJclStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclBaseContainer, IJclStrBaseContainer, IJclUnicodeStrContainer,
    IJclUnicodeStrOwner, IJclUnicodeStrEqualityComparer, IJclUnicodeStrComparer, IJclUnicodeStrHashConverter)
  protected
    FEqualityCompare: TUnicodeStrEqualityCompare;
    FCompare: TUnicodeStrCompare;
    FHashConvert: TUnicodeStrHashConvert;
    FOnFreeString: TFreeUnicodeStrEvent;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    { IJclUnicodeStrOwner }
    function GetOnFreeString: TFreeUnicodeStrEvent;
    function FreeString(var AString: UnicodeString): UnicodeString; virtual;
    procedure SetOnFreeString(Value: TFreeUnicodeStrEvent);
    property OnFreeString: TFreeUnicodeStrEvent read GetOnFreeString write SetOnFreeString;
    { IJclUnicodeStrEqualityComparer }
    function GetEqualityCompare: TUnicodeStrEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TUnicodeStrEqualityCompare); virtual;
    function ItemsEqual(const A, B: UnicodeString): Boolean; virtual;
    property EqualityCompare: TUnicodeStrEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    { IJclUnicodeStrComparer }
    function GetCompare: TUnicodeStrCompare; virtual;
    procedure SetCompare(Value: TUnicodeStrCompare); virtual;
    function ItemsCompare(const A, B: UnicodeString): Integer; virtual;
    property Compare: TUnicodeStrCompare read GetCompare write SetCompare;
    { IJclUnicodeStrHashConverter }
    function GetHashConvert: TUnicodeStrHashConvert; virtual;
    procedure SetHashConvert(Value: TUnicodeStrHashConvert); virtual;
    function Hash(const AString: UnicodeString): Integer; virtual;
    property HashConvert: TUnicodeStrHashConvert read GetHashConvert write SetHashConvert;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  TJclSingleAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclBaseContainer, IJclSingleContainer,
    IJclSingleOwner, IJclSingleEqualityComparer, IJclSingleComparer, IJclSingleHashConverter)
  protected
    FPrecision: Single;
    FEqualityCompare: TSingleEqualityCompare;
    FCompare: TSingleCompare;
    FHashConvert: TSingleHashConvert;
    FOnFreeSingle: TFreeSingleEvent;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    { IJclSingleOwner }
    function GetOnFreeSingle: TFreeSingleEvent;
    function FreeSingle(var AValue: Single): Single; virtual;
    procedure SetOnFreeSingle(Value: TFreeSingleEvent);
    property OnFreeSingle: TFreeSingleEvent read GetOnFreeSingle write SetOnFreeSingle;
    { IJclSingleContainer }
    function GetPrecision: Single; virtual;
    procedure SetPrecision(const Value: Single); virtual;
    property Precision: Single read GetPrecision write SetPrecision;
    { IJclSingleEqualityComparer }
    function GetEqualityCompare: TSingleEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TSingleEqualityCompare); virtual;
    function ItemsEqual(const A, B: Single): Boolean; virtual;
    property EqualityCompare: TSingleEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    { IJclSingleComparer }
    function GetCompare: TSingleCompare; virtual;
    procedure SetCompare(Value: TSingleCompare); virtual;
    function ItemsCompare(const A, B: Single): Integer; virtual;
    property Compare: TSingleCompare read GetCompare write SetCompare;
    { IJclSingleHashConverter }
    function GetHashConvert: TSingleHashConvert; virtual;
    procedure SetHashConvert(Value: TSingleHashConvert); virtual;
    function Hash(const AValue: Single): Integer; virtual;
    property HashConvert: TSingleHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclDoubleAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclBaseContainer, IJclDoubleContainer,
    IJclDoubleOwner, IJclDoubleEqualityComparer, IJclDoubleComparer, IJclDoubleHashConverter)
  protected
    FPrecision: Double;
    FEqualityCompare: TDoubleEqualityCompare;
    FCompare: TDoubleCompare;
    FHashConvert: TDoubleHashConvert;
    FOnFreeDouble: TFreeDoubleEvent;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    { IJclDoubleOwner }
    function GetOnFreeDouble: TFreeDoubleEvent;
    function FreeDouble(var AValue: Double): Double; virtual;
    procedure SetOnFreeDouble(Value: TFreeDoubleEvent);
    property OnFreeDouble: TFreeDoubleEvent read GetOnFreeDouble write SetOnFreeDouble;
    { IJclDoubleContainer }
    function GetPrecision: Double; virtual;
    procedure SetPrecision(const Value: Double); virtual;
    property Precision: Double read GetPrecision write SetPrecision;
    { IJclDoubleEqualityComparer }
    function GetEqualityCompare: TDoubleEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TDoubleEqualityCompare); virtual;
    function ItemsEqual(const A, B: Double): Boolean; virtual;
    property EqualityCompare: TDoubleEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    { IJclDoubleComparer }
    function GetCompare: TDoubleCompare; virtual;
    procedure SetCompare(Value: TDoubleCompare); virtual;
    function ItemsCompare(const A, B: Double): Integer; virtual;
    property Compare: TDoubleCompare read GetCompare write SetCompare;
    { IJclDoubleHashConverter }
    function GetHashConvert: TDoubleHashConvert; virtual;
    procedure SetHashConvert(Value: TDoubleHashConvert); virtual;
    function Hash(const AValue: Double): Integer; virtual;
    property HashConvert: TDoubleHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclExtendedAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclBaseContainer, IJclExtendedContainer,
    IJclExtendedOwner, IJclExtendedEqualityComparer, IJclExtendedComparer, IJclExtendedHashConverter)
  protected
    FPrecision: Extended;
    FEqualityCompare: TExtendedEqualityCompare;
    FCompare: TExtendedCompare;
    FHashConvert: TExtendedHashConvert;
    FOnFreeExtended: TFreeExtendedEvent;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    { IJclExtendedOwner }
    function GetOnFreeExtended: TFreeExtendedEvent;
    function FreeExtended(var AValue: Extended): Extended; virtual;
    procedure SetOnFreeExtended(Value: TFreeExtendedEvent);
    property OnFreeExtended: TFreeExtendedEvent read GetOnFreeExtended write SetOnFreeExtended;
    { IJclExtendedContainer }
    function GetPrecision: Extended; virtual;
    procedure SetPrecision(const Value: Extended); virtual;
    property Precision: Extended read GetPrecision write SetPrecision;
    { IJclExtendedEqualityComparer }
    function GetEqualityCompare: TExtendedEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TExtendedEqualityCompare); virtual;
    function ItemsEqual(const A, B: Extended): Boolean; virtual;
    property EqualityCompare: TExtendedEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    { IJclExtendedComparer }
    function GetCompare: TExtendedCompare; virtual;
    procedure SetCompare(Value: TExtendedCompare); virtual;
    function ItemsCompare(const A, B: Extended): Integer; virtual;
    property Compare: TExtendedCompare read GetCompare write SetCompare;
    { IJclExtendedHashConverter }
    function GetHashConvert: TExtendedHashConvert; virtual;
    procedure SetHashConvert(Value: TExtendedHashConvert); virtual;
    function Hash(const AValue: Extended): Integer; virtual;
    property HashConvert: TExtendedHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclIntegerAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclBaseContainer, IJclIntegerContainer,
    IJclIntegerOwner, IJclIntegerEqualityComparer, IJclIntegerComparer, IJclIntegerHashConverter)
  protected
    FEqualityCompare: TIntegerEqualityCompare;
    FCompare: TIntegerCompare;
    FHashConvert: TIntegerHashConvert;
    FOnFreeInteger: TFreeIntegerEvent;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    { IJclIntegerOwner }
    function GetOnFreeInteger: TFreeIntegerEvent;
    function FreeInteger(var AValue: Integer): Integer; virtual;
    procedure SetOnFreeInteger(Value: TFreeIntegerEvent);
    property OnFreeInteger: TFreeIntegerEvent read GetOnFreeInteger write SetOnFreeInteger;
    { IJclIntegerEqualityComparer }
    function GetEqualityCompare: TIntegerEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TIntegerEqualityCompare); virtual;
    function ItemsEqual(A, B: Integer): Boolean; virtual;
    property EqualityCompare: TIntegerEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    { IJclIntegerComparer }
    function GetCompare: TIntegerCompare; virtual;
    procedure SetCompare(Value: TIntegerCompare); virtual;
    function ItemsCompare(A, B: Integer): Integer; virtual;
    property Compare: TIntegerCompare read GetCompare write SetCompare;
    { IJclIntegerHashConverter }
    function GetHashConvert: TIntegerHashConvert; virtual;
    procedure SetHashConvert(Value: TIntegerHashConvert); virtual;
    function Hash(AValue: Integer): Integer; virtual;
    property HashConvert: TIntegerHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclCardinalAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclBaseContainer, IJclCardinalContainer,
    IJclCardinalOwner, IJclCardinalEqualityComparer, IJclCardinalComparer, IJclCardinalHashConverter)
  protected
    FEqualityCompare: TCardinalEqualityCompare;
    FCompare: TCardinalCompare;
    FHashConvert: TCardinalHashConvert;
    FOnFreeCardinal: TFreeCardinalEvent;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    { IJclCardinalOwner }
    function GetOnFreeCardinal: TFreeCardinalEvent;
    function FreeCardinal(var AValue: Cardinal): Cardinal; virtual;
    procedure SetOnFreeCardinal(Value: TFreeCardinalEvent);
    property OnFreeCardinal: TFreeCardinalEvent read GetOnFreeCardinal write SetOnFreeCardinal;
    { IJclCardinalEqualityComparer }
    function GetEqualityCompare: TCardinalEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TCardinalEqualityCompare); virtual;
    function ItemsEqual(A, B: Cardinal): Boolean; virtual;
    property EqualityCompare: TCardinalEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    { IJclCardinalComparer }
    function GetCompare: TCardinalCompare; virtual;
    procedure SetCompare(Value: TCardinalCompare); virtual;
    function ItemsCompare(A, B: Cardinal): Integer; virtual;
    property Compare: TCardinalCompare read GetCompare write SetCompare;
    { IJclCardinalHashConverter }
    function GetHashConvert: TCardinalHashConvert; virtual;
    procedure SetHashConvert(Value: TCardinalHashConvert); virtual;
    function Hash(AValue: Cardinal): Integer; virtual;
    property HashConvert: TCardinalHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclInt64AbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclBaseContainer, IJclInt64Container,
    IJclInt64Owner, IJclInt64EqualityComparer, IJclInt64Comparer, IJclInt64HashConverter)
  protected
    FEqualityCompare: TInt64EqualityCompare;
    FCompare: TInt64Compare;
    FHashConvert: TInt64HashConvert;
    FOnFreeInt64: TFreeInt64Event;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    { IJclInt64Owner }
    function GetOnFreeInt64: TFreeInt64Event;
    function FreeInt64(var AValue: Int64): Int64; virtual;
    procedure SetOnFreeInt64(Value: TFreeInt64Event);
    property OnFreeInt64: TFreeInt64Event read GetOnFreeInt64 write SetOnFreeInt64;
    { IJclInt64EqualityComparer }
    function GetEqualityCompare: TInt64EqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TInt64EqualityCompare); virtual;
    function ItemsEqual(const A, B: Int64): Boolean; virtual;
    property EqualityCompare: TInt64EqualityCompare read GetEqualityCompare write SetEqualityCompare;
    { IJclInt64Comparer }
    function GetCompare: TInt64Compare; virtual;
    procedure SetCompare(Value: TInt64Compare); virtual;
    function ItemsCompare(const A, B: Int64): Integer; virtual;
    property Compare: TInt64Compare read GetCompare write SetCompare;
    { IJclInt64HashConverter }
    function GetHashConvert: TInt64HashConvert; virtual;
    procedure SetHashConvert(Value: TInt64HashConvert); virtual;
    function Hash(const AValue: Int64): Integer; virtual;
    property HashConvert: TInt64HashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclPtrAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclBaseContainer, IJclPtrContainer,
    IJclPtrOwner, IJclPtrEqualityComparer, IJclPtrComparer, IJclPtrHashConverter)
  protected
    FEqualityCompare: TPtrEqualityCompare;
    FCompare: TPtrCompare;
    FHashConvert: TPtrHashConvert;
    FOnFreePointer: TFreePtrEvent;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    { IJclPtrOwner }
    function GetOnFreePointer: TFreePtrEvent;
    function FreePointer(var APtr: Pointer): Pointer; virtual;
    procedure SetOnFreePointer(Value: TFreePtrEvent);
    property OnFreePointer: TFreePtrEvent read GetOnFreePointer write SetOnFreePointer;
    { IJclPtrEqualityComparer }
    function GetEqualityCompare: TPtrEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TPtrEqualityCompare); virtual;
    function ItemsEqual(A, B: Pointer): Boolean; virtual;
    property EqualityCompare: TPtrEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    { IJclPtrComparer }
    function GetCompare: TPtrCompare; virtual;
    procedure SetCompare(Value: TPtrCompare); virtual;
    function ItemsCompare(A, B: Pointer): Integer; virtual;
    property Compare: TPtrCompare read GetCompare write SetCompare;
    { IJclPtrHashConverter }
    function GetHashConvert: TPtrHashConvert; virtual;
    procedure SetHashConvert(Value: TPtrHashConvert); virtual;
    function Hash(APtr: Pointer): Integer; virtual;
    property HashConvert: TPtrHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclBaseContainer, IJclContainer,
    IJclObjectOwner, IJclEqualityComparer, IJclComparer, IJclHashConverter)
  protected
    FOwnsObjects: Boolean;
    FEqualityCompare: TEqualityCompare;
    FCompare: TCompare;
    FHashConvert: THashConvert;
    FOnFreeObject: TFreeObjectEvent;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(AOwnsObjects: Boolean);
    { IJclObjectOwner }
    function GetOnFreeObject: TFreeObjectEvent;
    function FreeObject(var AObject: TObject): TObject; virtual;
    procedure SetOnFreeObject(Value: TFreeObjectEvent);
    property OnFreeObject: TFreeObjectEvent read GetOnFreeObject write SetOnFreeObject;
    function GetOwnsObjects: Boolean; virtual;
    property OwnsObjects: Boolean read GetOwnsObjects;
    { IJclEqualityComparer }
    function GetEqualityCompare: TEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TEqualityCompare); virtual;
    function ItemsEqual(A, B: TObject): Boolean; virtual;
    property EqualityCompare: TEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    { IJclComparer }
    function GetCompare: TCompare; virtual;
    procedure SetCompare(Value: TCompare); virtual;
    function ItemsCompare(A, B: TObject): Integer; virtual;
    property Compare: TCompare read GetCompare write SetCompare;
    { IJclHashConverter }
    function GetHashConvert: THashConvert; virtual;
    procedure SetHashConvert(Value: THashConvert); virtual;
    function Hash(AObject: TObject): Integer; virtual;
    property HashConvert: THashConvert read GetHashConvert write SetHashConvert;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN

  TJclAbstractContainer<T> = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclBaseContainer, IJclContainer<T>,
    IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>, IJclHashConverter<T>)
  protected
    FOwnsItems: Boolean;
    FEqualityCompare: TEqualityCompare<T>;
    FCompare: TCompare<T>;
    FHashConvert: THashConvert<T>;
    FOnFreeItem: TFreeItemEvent<T>;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(AOwnsItems: Boolean);
    { IJclItemOwner<T> }
    function GetOnFreeItem: TFreeItemEvent<T>;
    function FreeItem(var AItem: T): T; virtual;
    procedure SetOnFreeItem(Value: TFreeItemEvent<T>);
    property OnFreeItem: TFreeItemEvent<T> read GetOnFreeItem write SetOnFreeItem;
    function GetOwnsItems: Boolean; virtual;
    property OwnsItems: Boolean read GetOwnsItems;
    { IJclEqualityComparer<T> }
    function GetEqualityCompare: TEqualityCompare<T>; virtual;
    procedure SetEqualityCompare(Value: TEqualityCompare<T>); virtual;
    function ItemsEqual(const A, B: T): Boolean; virtual;
    property EqualityCompare: TEqualityCompare<T> read GetEqualityCompare write SetEqualityCompare;
    { IJclComparer<T> }
    function GetCompare: TCompare<T>; virtual;
    procedure SetCompare(Value: TCompare<T>); virtual;
    function ItemsCompare(const A, B: T): Integer; virtual;
    property Compare: TCompare<T> read GetCompare write SetCompare;
    { IJclHashConverter<T> }
    function GetHashConvert: THashConvert<T>; virtual;
    procedure SetHashConvert(Value: THashConvert<T>); virtual;
    function Hash(const AItem: T): Integer; virtual;
    property HashConvert: THashConvert<T> read GetHashConvert write SetHashConvert;
  end;

  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  TJclAnsiStrAbstractCollection = class(TJclAnsiStrAbstractContainer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclCloneable, IJclIntfCloneable, IJclBaseContainer,
    IJclStrBaseContainer, IJclAnsiStrContainer, IJclAnsiStrFlatContainer, IJclAnsiStrCollection,
    IJclAnsiStrEqualityComparer, IJclAnsiStrComparer)
  public
    { IJclAnsiStrCollection }
    function Add(const AString: AnsiString): Boolean; virtual; abstract;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    function Contains(const AString: AnsiString): Boolean; virtual; abstract;
    function ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    function CollectionEquals(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    function Extract(const AString: AnsiString): Boolean; virtual; abstract;
    function ExtractAll(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    function First: IJclAnsiStrIterator; virtual; abstract;
    function IsEmpty: Boolean; virtual; abstract;
    function Last: IJclAnsiStrIterator; virtual; abstract;
    function Remove(const AString: AnsiString): Boolean; virtual; abstract;
    function RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    function RetainAll(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    function Size: Integer; virtual; abstract;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclAnsistrIterator; virtual; abstract;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclAnsiStrFlatContainer }
    procedure LoadFromStrings(Strings: TJclAnsiStrings);
    procedure SaveToStrings(Strings: TJclAnsiStrings);
    procedure AppendToStrings(Strings: TJclAnsiStrings);
    procedure AppendFromStrings(Strings: TJclAnsiStrings);
    function GetAsStrings: TJclAnsiStrings;
    function GetAsDelimited(const Separator: AnsiString = AnsiLineBreak): AnsiString;
    procedure AppendDelimited(const AString: AnsiString; const Separator: AnsiString = AnsiLineBreak);
    procedure LoadDelimited(const AString: AnsiString; const Separator: AnsiString = AnsiLineBreak);
  end;

  TJclWideStrAbstractCollection = class(TJclWideStrAbstractContainer,
    {$IFDEF THREADSAFE}IJclLockable,{$ENDIF THREADSAFE} IJclCloneable, IJclIntfCloneable, IJclBaseContainer,
    IJclStrBaseContainer, IJclWideStrContainer, IJclWideStrFlatContainer, IJclWideStrCollection,
    IJclWideStrEqualityComparer, IJclWideStrComparer)
  public
    { IJclWideStrCollection }
    function Add(const AString: WideString): Boolean; virtual; abstract;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    function Contains(const AString: WideString): Boolean; virtual; abstract;
    function ContainsAll(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    function CollectionEquals(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    function Extract(const AString: WideString): Boolean; virtual; abstract;
    function ExtractAll(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    function First: IJclWideStrIterator; virtual; abstract;
    function IsEmpty: Boolean; virtual; abstract;
    function Last: IJclWideStrIterator; virtual; abstract;
    function Remove(const AString: WideString): Boolean; virtual; abstract;
    function RemoveAll(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    function RetainAll(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    function Size: Integer; virtual; abstract;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclWideStrIterator; virtual; abstract;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclWideStrFlatContainer }
    procedure LoadFromStrings(Strings: TJclWideStrings);
    procedure SaveToStrings(Strings: TJclWideStrings);
    procedure AppendToStrings(Strings: TJclWideStrings);
    procedure AppendFromStrings(Strings: TJclWideStrings);
    function GetAsStrings: TJclWideStrings;
    function GetAsDelimited(const Separator: WideString = WideLineBreak): WideString;
    procedure AppendDelimited(const AString: WideString; const Separator: WideString = WideLineBreak);
    procedure LoadDelimited(const AString: WideString; const Separator: WideString = WideLineBreak);
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrAbstractCollection = class(TJclUnicodeStrAbstractContainer,
    {$IFDEF THREADSAFE}IJclLockable,{$ENDIF THREADSAFE} IJclCloneable, IJclIntfCloneable, IJclBaseContainer,
    IJclStrBaseContainer, IJclUnicodeStrContainer, IJclUnicodeStrFlatContainer, IJclUnicodeStrCollection,
    IJclUnicodeStrEqualityComparer, IJclUnicodeStrComparer)
  public
    { IJclUnicodeStrCollection }
    function Add(const AString: UnicodeString): Boolean; virtual; abstract;
    function AddAll(const ACollection: IJclUnicodeStrCollection): Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    function Contains(const AString: UnicodeString): Boolean; virtual; abstract;
    function ContainsAll(const ACollection: IJclUnicodeStrCollection): Boolean; virtual; abstract;
    function CollectionEquals(const ACollection: IJclUnicodeStrCollection): Boolean; virtual; abstract;
    function Extract(const AString: UnicodeString): Boolean; virtual; abstract;
    function ExtractAll(const ACollection: IJclUnicodeStrCollection): Boolean; virtual; abstract;
    function First: IJclUnicodeStrIterator; virtual; abstract;
    function IsEmpty: Boolean; virtual; abstract;
    function Last: IJclUnicodeStrIterator; virtual; abstract;
    function Remove(const AString: UnicodeString): Boolean; virtual; abstract;
    function RemoveAll(const ACollection: IJclUnicodeStrCollection): Boolean; virtual; abstract;
    function RetainAll(const ACollection: IJclUnicodeStrCollection): Boolean; virtual; abstract;
    function Size: Integer; virtual; abstract;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclUnicodeStrIterator; virtual; abstract;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclUnicodeStrFlatContainer }
    procedure LoadFromStrings(Strings: TJclUnicodeStrings);
    procedure SaveToStrings(Strings: TJclUnicodeStrings);
    procedure AppendToStrings(Strings: TJclUnicodeStrings);
    procedure AppendFromStrings(Strings: TJclUnicodeStrings);
    function GetAsStrings: TJclUnicodeStrings;
    function GetAsDelimited(const Separator: UnicodeString = WideLineBreak): UnicodeString;
    procedure AppendDelimited(const AString: UnicodeString; const Separator: UnicodeString = WideLineBreak);
    procedure LoadDelimited(const AString: UnicodeString; const Separator: UnicodeString = WideLineBreak);
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

const
  // table of byte permutations without inner loop
  BytePermTable: array [Byte] of Byte =
   ( 22,  133, 0,   244, 194, 193, 4,   164, 69,  211, 166, 235, 75,  110, 9,   140,
     125, 84,  64,  209, 57,  47,  197, 76,  237, 48,  189, 87,  221, 254, 20,  132,
     25,  162, 203, 225, 186, 165, 72,  228, 61,  208, 158, 185, 114, 173, 1,   66,
     202, 46,  198, 214, 27,  161, 178, 238, 8,   68,  97,  17,  199, 210, 96,  196,
     85,  240, 233, 71,  232, 142, 148, 70,  184, 152, 90,  206, 139, 182, 34,  101,
     104, 12,  143, 227, 24,  247, 175, 150, 39,  31,  36,  123, 62,  119, 236, 28,
     117, 100, 230, 223, 30,  154, 18,  153, 127, 192, 176, 19,  174, 134, 2,   216,
     218, 91,  45,  7,   128, 138, 126, 40,  16,  54,  207, 181, 11,  137, 60,  191,
     51,  231, 121, 213, 86,  111, 141, 172, 98,  226, 179, 249, 136, 58,  88,  93,
     201, 195, 118, 144, 146, 113, 212, 32,  21,  131, 177, 33,  151, 130, 205, 171,
     92,  251, 168, 29,  156, 124, 224, 200, 3,   187, 105, 52,  239, 147, 82,  94,
     26,  102, 243, 242, 145, 163, 49,  135, 43,  78,  112, 83,  63,  35,  170, 167,
     250, 159, 73,  37,  6,   79,  106, 215, 129, 74,  109, 42,  41,  120, 23,  160,
     107, 180, 103, 77,  53,  169, 89,  149, 44,  38,  81,  246, 188, 67,  15,  80,
     155, 99,  95,  5,   229, 108, 13,  255, 59,  241, 252, 245, 222, 248, 115, 55,
     217, 56,  65,  219, 204, 190, 10,  50,  253, 183, 234, 116, 122, 220, 14,  157);

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
  {$IFDEF HAS_UNIT_ANSISTRINGS}
  System.AnsiStrings,
  {$ENDIF HAS_UNIT_ANSISTRINGS}
  System.SysUtils,
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF HAS_UNIT_ANSISTRINGS}
  AnsiStrings,
  {$ENDIF HAS_UNIT_ANSISTRINGS}
  SysUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  JclStringConversions, JclUnicode, JclAlgorithms;

//=== { TJclAbstractLockable } ===============================================

{$IFDEF THREADSAFE}

constructor TJclAbstractLockable.Create;
begin
  inherited Create;
  FThreadSafe := True;
  FSyncReaderWriter := TJclMultiReadExclusiveWrite.Create(mpReaders);
end;

destructor TJclAbstractLockable.Destroy;
begin
  FSyncReaderWriter.Free;
  inherited Destroy;
end;

procedure TJclAbstractLockable.ReadLock;
begin
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
end;

procedure TJclAbstractLockable.ReadUnlock;
begin
  if FThreadSafe then
    SyncReaderWriter.EndRead;
end;

procedure TJclAbstractLockable.WriteLock;
begin
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
end;

procedure TJclAbstractLockable.WriteUnlock;
begin
  if FThreadSafe then
    SyncReaderWriter.EndWrite;
end;
{$ENDIF THREADSAFE}

//=== { TJclAbstractContainerBase } ==========================================

constructor TJclAbstractContainerBase.Create;
begin
  inherited Create;

  FAllowDefaultElements := True;
  FDuplicates := dupAccept;
  FRemoveSingleElement := True;
  FReturnDefaultElements := True;
  FAutoGrowStrategy := agsProportional;
  FAutoGrowParameter := 4;
  FAutoPackStrategy := apsDisabled;
  FAutoPackParameter := 4;
end;

procedure TJclAbstractContainerBase.Assign(const Source: IJclBaseContainer);
begin
  Source.AssignTo(Self);
end;

procedure TJclAbstractContainerBase.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  // override to customize
  if Dest.ReadOnly then
    raise EJclReadOnlyError.Create;
end;

procedure TJclAbstractContainerBase.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  // override to customize
  Dest.AllowDefaultElements := AllowDefaultElements;
  Dest.Duplicates := Duplicates;
  Dest.RemoveSingleElement := RemoveSingleElement;
  Dest.ReturnDefaultElements := ReturnDefaultElements;
  Dest.AutoGrowParameter := AutoGrowParameter;
  Dest.AutoGrowStrategy := AutoGrowStrategy;
  Dest.AutoPackParameter := AutoPackParameter;
  Dest.AutoPackStrategy := AutoPackStrategy;
end;

procedure TJclAbstractContainerBase.AssignTo(const Dest: IJclBaseContainer);
var
  DestObject: TObject;
begin
  DestObject := Dest.GetContainerReference;
  if DestObject is TJclAbstractContainerBase then
  begin
    AssignPropertiesTo(TJclAbstractContainerBase(DestObject));
    AssignDataTo(TJclAbstractContainerBase(DestObject));
  end
  else
    raise EJclAssignError.Create;
end;

procedure TJclAbstractContainerBase.AutoGrow;
begin
  SetCapacity(CalcGrowCapacity(FCapacity, FSize));
end;

procedure TJclAbstractContainerBase.AutoPack;
begin
  SetCapacity(CalcPackCapacity(FCapacity, FSize));
end;

function TJclAbstractContainerBase.CalcGrowCapacity(ACapacity, ASize: Integer): Integer;
var
  Increment: Integer;
begin
  Result := ACapacity;
  if ASize = ACapacity then
  begin
    case FAutoGrowStrategy of
      agsDisabled: ;
      agsAgressive:
        Result := ACapacity + 1;
      agsProportional:
        begin
          Increment := ACapacity div FAutoGrowParameter;
          if Increment = 0 then
            Increment := 1;
          Result := ACapacity + Increment;
        end;
      agsIncremental:
        Result := ACapacity + FAutoGrowParameter;
    end;
  end;
end;

function TJclAbstractContainerBase.CalcPackCapacity(ACapacity, ASize: Integer): Integer;
var
  Decrement: Integer;
begin
  Result := ACapacity;
  if ASize < ACapacity then
  begin
    case FAutoPackStrategy of
      apsDisabled:
        Decrement := 0;
      apsAgressive:
        Decrement := 1;
      apsProportional:
        Decrement := ACapacity div FAutoPackParameter;
      apsIncremental:
        Decrement := FAutoPackParameter;
    else
      Decrement := 0;
    end;
    if (Decrement > 0) and ((ASize + Decrement) <= ACapacity) then
      Result := ASize;
  end;
end;

function TJclAbstractContainerBase.CheckDuplicate: Boolean;
begin
  case FDuplicates of
    dupIgnore:
      Result := False;
    dupAccept:
      Result := True;
    //dupError: ;
  else
    raise EJclDuplicateElementError.Create;
  end;
end;

function TJclAbstractContainerBase.ObjectClone: TObject;
var
  NewContainer: TJclAbstractContainerBase;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewContainer := CreateEmptyContainer;
    AssignDataTo(NewContainer);
    Result := NewContainer;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAbstractContainerBase.GetAllowDefaultElements: Boolean;
begin
  Result := FAllowDefaultElements;
end;

function TJclAbstractContainerBase.GetAutoGrowParameter: Integer;
begin
  Result := FAutoGrowParameter;
end;

function TJclAbstractContainerBase.GetAutoGrowStrategy: TJclAutoGrowStrategy;
begin
  Result := FAutoGrowStrategy;
end;

function TJclAbstractContainerBase.GetAutoPackParameter: Integer;
begin
  Result := FAutoPackParameter;
end;

function TJclAbstractContainerBase.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := FAutoPackStrategy;
end;

function TJclAbstractContainerBase.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclAbstractContainerBase.GetContainerReference: TObject;
begin
  Result := Self;
end;

function TJclAbstractContainerBase.GetDuplicates: TDuplicates;
begin
  Result := FDuplicates;
end;

function TJclAbstractContainerBase.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TJclAbstractContainerBase.GetRemoveSingleElement: Boolean;
begin
  Result := FRemoveSingleElement;
end;

function TJclAbstractContainerBase.GetReturnDefaultElements: Boolean;
begin
  Result := FReturnDefaultElements;
end;

function TJclAbstractContainerBase.GetThreadSafe: Boolean;
begin
  {$IFDEF THREADSAFE}
  Result := FThreadSafe;
  {$ELSE ~THREADSAFE}
  Result := False;
  {$ENDIF ~THREADSAFE}
end;

procedure TJclAbstractContainerBase.Grow;
begin
  // override to customize
  SetCapacity(CalcGrowCapacity(FCapacity, FSize));
end;

function TJclAbstractContainerBase.IntfClone: IInterface;
var
  NewContainer: TJclAbstractContainerBase;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewContainer := CreateEmptyContainer;
    AssignDataTo(NewContainer);
    Result := NewContainer;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAbstractContainerBase.Pack;
begin
  // override to customize
  SetCapacity(CalcPackCapacity(FCapacity, FSize));
end;

procedure TJclAbstractContainerBase.SetAllowDefaultElements(Value: Boolean);
begin
  FAllowDefaultElements := Value;
end;

procedure TJclAbstractContainerBase.SetAutoGrowParameter(Value: Integer);
begin
  FAutoGrowParameter := Value;
end;

procedure TJclAbstractContainerBase.SetAutoGrowStrategy(Value: TJclAutoGrowStrategy);
begin
  FAutoGrowStrategy := Value;
end;

procedure TJclAbstractContainerBase.SetAutoPackParameter(Value: Integer);
begin
  FAutoPackParameter := Value;
end;

procedure TJclAbstractContainerBase.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  FAutoPackStrategy := Value;
end;

procedure TJclAbstractContainerBase.SetCapacity(Value: Integer);
begin
  FCapacity := Value;
end;

procedure TJclAbstractContainerBase.SetDuplicates(Value: TDuplicates);
begin
  FDuplicates := Value;
end;

procedure TJclAbstractContainerBase.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TJclAbstractContainerBase.SetRemoveSingleElement(Value: Boolean);
begin
  FRemoveSingleElement := Value;
end;

procedure TJclAbstractContainerBase.SetReturnDefaultElements(Value: Boolean);
begin
  FReturnDefaultElements := Value;
end;

procedure TJclAbstractContainerBase.SetThreadSafe(Value: Boolean);
begin
  {$IFDEF THREADSAFE}
  FThreadSafe := Value;
  {$ELSE ~THREADSAFE}
  if Value then
    raise EJclOperationNotSupportedError.Create;
  {$ENDIF ~THREADSAFE}
end;

//=== { TJclAbstractIterator } ===============================================

constructor TJclAbstractIterator.Create(AValid: Boolean);
begin
  inherited Create;
  FValid := AValid;
end;

procedure TJclAbstractIterator.Assign(const Source: IJclAbstractIterator);
begin
  Source.AssignTo(Self);
end;

procedure TJclAbstractIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
begin
  Dest.FValid := FValid;
end;

procedure TJclAbstractIterator.AssignTo(const Dest: IJclAbstractIterator);
var
  DestObject: TObject;
begin
  DestObject := Dest.GetIteratorReference;
  if DestObject is TJclAbstractIterator then
    AssignPropertiesTo(TJclAbstractIterator(DestObject))
  else
    raise EJclAssignError.Create;
end;

procedure TJclAbstractIterator.CheckValid;
begin
  if not Valid then
    raise EJclIllegalStateOperationError.Create;
end;

function TJclAbstractIterator.ObjectClone: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyIterator;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAbstractIterator.GetIteratorReference: TObject;
begin
  Result := Self;
end;

function TJclAbstractIterator.IntfClone: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyIterator;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclIntfAbstractContainer } ==========================================

procedure TJclIntfAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntfAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclIntfAbstractContainer then
  begin
    ADest := TJclIntfAbstractContainer(Dest);
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclIntfAbstractContainer.FreeObject(var AInterface: IInterface): IInterface;
begin
  if Assigned(FOnFreeObject) then
    Result := FOnFreeObject(AInterface)
  else
  begin
    Result := AInterface;
    AInterface := nil;
  end;
end;

function TJclIntfAbstractContainer.GetCompare: TIntfCompare;
begin
  Result := FCompare;
end;

function TJclIntfAbstractContainer.GetEqualityCompare: TIntfEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclIntfAbstractContainer.GetHashConvert: TIntfHashConvert;
begin
  Result := FHashConvert;
end;

function TJclIntfAbstractContainer.GetOnFreeObject: TFreeIntfEvent;
begin
  Result := FOnFreeObject;
end;

function TJclIntfAbstractContainer.Hash(const AInterface: IInterface): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AInterface)
  else
    Result := IntfSimpleHashConvert(AInterface);
end;

function TJclIntfAbstractContainer.ItemsCompare(const A, B: IInterface): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := IntfSimpleCompare(A, B);
end;

function TJclIntfAbstractContainer.ItemsEqual(const A, B: IInterface): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := IntfSimpleEqualityCompare(A, B);
end;

procedure TJclIntfAbstractContainer.SetCompare(Value: TIntfCompare);
begin
  FCompare := Value;
end;

procedure TJclIntfAbstractContainer.SetEqualityCompare(Value: TIntfEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclIntfAbstractContainer.SetHashConvert(Value: TIntfHashConvert);
begin
  FHashConvert := Value;
end;

procedure TJclIntfAbstractContainer.SetOnFreeObject(Value: TFreeIntfEvent);
begin
  FOnFreeObject := Value;
end;

//=== { TJclStrAbstractContainer } ===========================================

procedure TJclStrAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclStrAbstractContainer then
    TJclStrAbstractContainer(Dest).SetCaseSensitive(GetCaseSensitive);
end;

function TJclStrAbstractContainer.GetCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

procedure TJclStrAbstractContainer.SetCaseSensitive(Value: Boolean);
begin
  FCaseSensitive := Value;
end;

//=== { TJclAnsiStrAbstractContainer } =======================================

procedure TJclAnsiStrAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclAnsiStrAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclAnsiStrAbstractContainer then
  begin
    ADest := TJclAnsiStrAbstractContainer(Dest);
    ADest.Encoding := Encoding;
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclAnsiStrAbstractContainer.FreeString(var AString: AnsiString): AnsiString;
begin
  if Assigned(FOnFreeString) then
    Result := FOnFreeString(AString)
  else
  begin
    Result := AString;
    AString := '';
  end;
end;

function TJclAnsiStrAbstractContainer.GetCompare: TAnsiStrCompare;
begin
  Result := FCompare;
end;

function TJclAnsiStrAbstractContainer.GetEncoding: TJclAnsiStrEncoding;
begin
  Result := FEncoding;
end;

function TJclAnsiStrAbstractContainer.GetEqualityCompare: TAnsiStrEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclAnsiStrAbstractContainer.GetHashConvert: TAnsiStrHashConvert;
begin
  Result := FHashConvert;
end;

function TJclAnsiStrAbstractContainer.GetOnFreeString: TFreeAnsiStrEvent;
begin
  Result := FOnFreeString;
end;

function TJclAnsiStrAbstractContainer.Hash(const AString: AnsiString): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AString)
  else
  begin
    case FEncoding of
      seISO:
        if FCaseSensitive then
          Result := AnsiStrSimpleHashConvert(AString)
        else
          Result := AnsiStrSimpleHashConvertI(AString);
      seUTF8:
        if FCaseSensitive then
          Result := AnsiStrSimpleHashConvertU(AString)
        else
          Result := AnsiStrSimpleHashConvertUI(AString);
    else
      raise EJclOperationNotSupportedError.Create;
    end;
  end;
end;

function TJclAnsiStrAbstractContainer.ItemsCompare(const A, B: AnsiString): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  begin
    case FEncoding of
      seISO, seUTF8:
        if FCaseSensitive then
          Result := AnsiStrSimpleCompare(A, B)
        else
          Result := AnsiStrSimpleCompareI(A, B);
    else
      raise EJclOperationNotSupportedError.Create;
    end;
  end;
end;

function TJclAnsiStrAbstractContainer.ItemsEqual(const A, B: AnsiString): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
  begin
    case FEncoding of
      seISO, seUTF8:
        if FCaseSensitive then
          Result := AnsiStrSimpleEqualityCompare(A, B)
        else
          Result := AnsiStrSimpleEqualityCompareI(A, B);
    else
      raise EJclOperationNotSupportedError.Create;
    end;
  end;
end;

procedure TJclAnsiStrAbstractContainer.SetCompare(Value: TAnsiStrCompare);
begin
  FCompare := Value;
end;

procedure TJclAnsiStrAbstractContainer.SetEncoding(Value: TJclAnsiStrEncoding);
begin
  FEncoding := Value;
end;

procedure TJclAnsiStrAbstractContainer.SetEqualityCompare(Value: TAnsiStrEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclAnsiStrAbstractContainer.SetHashConvert(Value: TAnsiStrHashConvert);
begin
  FHashConvert := Value;
end;

procedure TJclAnsiStrAbstractContainer.SetOnFreeString(
  Value: TFreeAnsiStrEvent);
begin
  FOnFreeString := Value;
end;

//=== { TJclWideStrContainer } ===============================================

procedure TJclWideStrAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclWideStrAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclWideStrAbstractContainer then
  begin
    ADest := TJclWideStrAbstractContainer(Dest);
    ADest.Encoding := Encoding;
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclWideStrAbstractContainer.FreeString(var AString: WideString): WideString;
begin
  if Assigned(FOnFreeString) then
    Result := FOnFreeString(AString)
  else
  begin
    Result := AString;
    AString := '';
  end;
end;

function TJclWideStrAbstractContainer.GetCompare: TWideStrCompare;
begin
  Result := FCompare;
end;

function TJclWideStrAbstractContainer.GetEncoding: TJclWideStrEncoding;
begin
  Result := FEncoding;
end;

function TJclWideStrAbstractContainer.GetEqualityCompare: TWideStrEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclWideStrAbstractContainer.GetHashConvert: TWideStrHashConvert;
begin
  Result := FHashConvert;
end;

function TJclWideStrAbstractContainer.GetOnFreeString: TFreeWideStrEvent;
begin
  Result := FOnFreeString;
end;

function TJclWideStrAbstractContainer.Hash(const AString: WideString): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AString)
  else
  begin
    case FEncoding of
      seUTF16:
        if FCaseSensitive then
          Result := WideStrSimpleHashConvert(AString)
        else
          Result := WideStrSimpleHashConvertI(AString);
    else
      raise EJclOperationNotSupportedError.Create;
    end;
  end;
end;

function TJclWideStrAbstractContainer.ItemsCompare(const A, B: WideString): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  begin
    case FEncoding of
      seUTF16:
        if FCaseSensitive then
          Result := WideStrSimpleCompare(A, B)
        else
          Result := WideStrSimpleCompareI(A, B);
    else
      raise EJclOperationNotSupportedError.Create;
    end;
  end;
end;

function TJclWideStrAbstractContainer.ItemsEqual(const A, B: WideString): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
  begin
    case FEncoding of
      seUTF16:
        if FCaseSensitive then
          Result := WideStrSimpleEqualityCompare(A, B)
        else
          Result := WideStrSimpleEqualityCompareI(A, B);
    else
      raise EJclOperationNotSupportedError.Create;
    end;
  end;
end;

procedure TJclWideStrAbstractContainer.SetCompare(Value: TWideStrCompare);
begin
  FCompare := Value;
end;

procedure TJclWideStrAbstractContainer.SetEncoding(Value: TJclWideStrEncoding);
begin
  FEncoding := Value;
end;

procedure TJclWideStrAbstractContainer.SetEqualityCompare(Value: TWideStrEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclWideStrAbstractContainer.SetHashConvert(Value: TWideStrHashConvert);
begin
  FHashConvert := Value;
end;

procedure TJclWideStrAbstractContainer.SetOnFreeString(
  Value: TFreeWideStrEvent);
begin
  FOnFreeString := Value;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrContainer } ===============================================

procedure TJclUnicodeStrAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclUnicodeStrAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclUnicodeStrAbstractContainer then
  begin
    ADest := TJclUnicodeStrAbstractContainer(Dest);
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclUnicodeStrAbstractContainer.FreeString(var AString: UnicodeString): UnicodeString;
begin
  if Assigned(FOnFreeString) then
    Result := FOnFreeString(AString)
  else
  begin
    Result := AString;
    AString := '';
  end;
end;

function TJclUnicodeStrAbstractContainer.GetCompare: TUnicodeStrCompare;
begin
  Result := FCompare;
end;

function TJclUnicodeStrAbstractContainer.GetEqualityCompare: TUnicodeStrEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclUnicodeStrAbstractContainer.GetHashConvert: TUnicodeStrHashConvert;
begin
  Result := FHashConvert;
end;

function TJclUnicodeStrAbstractContainer.GetOnFreeString: TFreeUnicodeStrEvent;
begin
  Result := FOnFreeString;
end;

function TJclUnicodeStrAbstractContainer.Hash(const AString: UnicodeString): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AString)
  else
  if FCaseSensitive then
    Result := UnicodeStrSimpleHashConvert(AString)
  else
    Result := UnicodeStrSimpleHashConvertI(AString);
end;

function TJclUnicodeStrAbstractContainer.ItemsCompare(const A, B: UnicodeString): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  if FCaseSensitive then
    Result := UnicodeStrSimpleCompare(A, B)
  else
    Result := UnicodeStrSimpleCompareI(A, B);
end;

function TJclUnicodeStrAbstractContainer.ItemsEqual(const A, B: UnicodeString): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
  if FCaseSensitive then
    Result := UnicodeStrSimpleEqualityCompare(A, B)
  else
    Result := UnicodeStrSimpleEqualityCompareI(A, B);
end;

procedure TJclUnicodeStrAbstractContainer.SetCompare(Value: TUnicodeStrCompare);
begin
  FCompare := Value;
end;

procedure TJclUnicodeStrAbstractContainer.SetEqualityCompare(Value: TUnicodeStrEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclUnicodeStrAbstractContainer.SetHashConvert(Value: TUnicodeStrHashConvert);
begin
  FHashConvert := Value;
end;

procedure TJclUnicodeStrAbstractContainer.SetOnFreeString(Value: TFreeUnicodeStrEvent);
begin
  FOnFreeString := Value;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

//=== { TJclSingleAbstractContainer } ========================================

procedure TJclSingleAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclSingleAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclSingleAbstractContainer then
  begin
    ADest := TJclSingleAbstractContainer(Dest);
    ADest.Precision := Precision;
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclSingleAbstractContainer.FreeSingle(var AValue: Single): Single;
begin
  if Assigned(FOnFreeSingle) then
    Result := FOnFreeSingle(AValue)
  else
  begin
    Result := AValue;
    AValue := 0.0;
  end;
end;

function TJclSingleAbstractContainer.GetCompare: TSingleCompare;
begin
  Result := FCompare;
end;

function TJclSingleAbstractContainer.GetEqualityCompare: TSingleEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclSingleAbstractContainer.GetHashConvert: TSingleHashConvert;
begin
  Result := FHashConvert;
end;

function TJclSingleAbstractContainer.GetOnFreeSingle: TFreeSingleEvent;
begin
  Result := FOnFreeSingle;
end;

function TJclSingleAbstractContainer.GetPrecision: Single;
begin
  Result := FPrecision;
end;

function TJclSingleAbstractContainer.Hash(const AValue: Single): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AValue)
  else
    Result := SingleSimpleHashConvert(AValue);
end;

function TJclSingleAbstractContainer.ItemsCompare(const A, B: Single): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  if Abs(A - B) <= FPrecision then
    Result := 0
  else
  if A > B then
    Result := 1
  else
    Result := -1;
end;

function TJclSingleAbstractContainer.ItemsEqual(const A, B: Single): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := Abs(A - B) <= FPrecision;
end;

procedure TJclSingleAbstractContainer.SetCompare(Value: TSingleCompare);
begin
  FCompare := Value;
end;

procedure TJclSingleAbstractContainer.SetEqualityCompare(Value: TSingleEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclSingleAbstractContainer.SetHashConvert(Value: TSingleHashConvert);
begin
  FHashConvert := Value;
end;

procedure TJclSingleAbstractContainer.SetOnFreeSingle(Value: TFreeSingleEvent);
begin
  FOnFreeSingle := Value;
end;

procedure TJclSingleAbstractContainer.SetPrecision(const Value: Single);
begin
  FPrecision := Value;
end;

//=== { TJclDoubleAbstractContainer } ========================================

procedure TJclDoubleAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclDoubleAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclDoubleAbstractContainer then
  begin
    ADest := TJclDoubleAbstractContainer(Dest);
    ADest.Precision := Precision;
    ADest.Compare := Compare;
    ADest.EqualityCompare := EqualityCompare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclDoubleAbstractContainer.FreeDouble(var AValue: Double): Double;
begin
  if Assigned(FOnFreeDouble) then
    Result := FOnFreeDouble(AValue)
  else
  begin
    Result := AValue;
    AValue := 0.0;
  end;
end;

function TJclDoubleAbstractContainer.GetCompare: TDoubleCompare;
begin
  Result := FCompare;
end;

function TJclDoubleAbstractContainer.GetEqualityCompare: TDoubleEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclDoubleAbstractContainer.GetHashConvert: TDoubleHashConvert;
begin
  Result := FHashConvert;
end;

function TJclDoubleAbstractContainer.GetOnFreeDouble: TFreeDoubleEvent;
begin
  Result := FOnFreeDouble;
end;

function TJclDoubleAbstractContainer.GetPrecision: Double;
begin
  Result := FPrecision;
end;

function TJclDoubleAbstractContainer.Hash(const AValue: Double): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AValue)
  else
    Result := DoubleSimpleHashConvert(AValue);
end;

function TJclDoubleAbstractContainer.ItemsCompare(const A, B: Double): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  if Abs(A - B) <= FPrecision then
    Result := 0
  else
  if A > B then
    Result := 1
  else
    Result := -1;
end;

function TJclDoubleAbstractContainer.ItemsEqual(const A, B: Double): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := Abs(A - B) <= FPrecision;
end;

procedure TJclDoubleAbstractContainer.SetCompare(Value: TDoubleCompare);
begin
  FCompare := Value;
end;

procedure TJclDoubleAbstractContainer.SetEqualityCompare(Value: TDoubleEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclDoubleAbstractContainer.SetHashConvert(Value: TDoubleHashConvert);
begin
  FHashConvert := Value;
end;

procedure TJclDoubleAbstractContainer.SetOnFreeDouble(Value: TFreeDoubleEvent);
begin
  FOnFreeDouble := Value;
end;

procedure TJclDoubleAbstractContainer.SetPrecision(const Value: Double);
begin
  FPrecision := Value;
end;

//=== { TJclExtendedAbstractContainer } ======================================

procedure TJclExtendedAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclExtendedAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclExtendedAbstractContainer then
  begin
    ADest := TJclExtendedAbstractContainer(Dest);
    ADest.Precision := Precision;
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclExtendedAbstractContainer.FreeExtended(var AValue: Extended): Extended;
begin
  if Assigned(FOnFreeExtended) then
    Result := FOnFreeExtended(AValue)
  else
  begin
    Result := AValue;
    AValue := 0.0;
  end;
end;

function TJclExtendedAbstractContainer.GetCompare: TExtendedCompare;
begin
  Result := FCompare;
end;

function TJclExtendedAbstractContainer.GetEqualityCompare: TExtendedEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclExtendedAbstractContainer.GetHashConvert: TExtendedHashConvert;
begin
  Result := FHashConvert;
end;

function TJclExtendedAbstractContainer.GetOnFreeExtended: TFreeExtendedEvent;
begin
  Result := FOnFreeExtended;
end;

function TJclExtendedAbstractContainer.GetPrecision: Extended;
begin
  Result := FPrecision;
end;

function TJclExtendedAbstractContainer.Hash(const AValue: Extended): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AValue)
  else
    Result := ExtendedSimpleHashConvert(AValue);
end;

function TJclExtendedAbstractContainer.ItemsCompare(const A, B: Extended): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  if Abs(A - B) <= FPrecision then
    Result := 0
  else
  if A > B then
    Result := 1
  else
    Result := -1;
end;

function TJclExtendedAbstractContainer.ItemsEqual(const A, B: Extended): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := Abs(A - B) <= FPrecision;
end;

procedure TJclExtendedAbstractContainer.SetCompare(Value: TExtendedCompare);
begin
  FCompare := Value;
end;

procedure TJclExtendedAbstractContainer.SetEqualityCompare(Value: TExtendedEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclExtendedAbstractContainer.SetHashConvert(Value: TExtendedHashConvert);
begin
  FHashConvert := Value;
end;

procedure TJclExtendedAbstractContainer.SetOnFreeExtended(
  Value: TFreeExtendedEvent);
begin
  FOnFreeExtended := Value;
end;

procedure TJclExtendedAbstractContainer.SetPrecision(const Value: Extended);
begin
  FPrecision := Value;
end;

//=== { TJclIntegerAbstractContainer } =======================================

procedure TJclIntegerAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntegerAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclIntegerAbstractContainer then
  begin
    ADest := TJclIntegerAbstractContainer(Dest);
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclIntegerAbstractContainer.FreeInteger(var AValue: Integer): Integer;
begin
  if Assigned(FOnFreeInteger) then
    Result := FOnFreeInteger(AValue)
  else
  begin
    Result := AValue;
    AValue := 0;
  end;
end;

function TJclIntegerAbstractContainer.GetCompare: TIntegerCompare;
begin
  Result := FCompare;
end;

function TJclIntegerAbstractContainer.GetEqualityCompare: TIntegerEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclIntegerAbstractContainer.GetHashConvert: TIntegerHashConvert;
begin
  Result := FHashConvert;
end;

function TJclIntegerAbstractContainer.GetOnFreeInteger: TFreeIntegerEvent;
begin
  Result := FOnFreeInteger;
end;

function TJclIntegerAbstractContainer.Hash(AValue: Integer): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AValue)
  else
    Result := IntegerSimpleHashConvert(AValue);
end;

function TJclIntegerAbstractContainer.ItemsCompare(A, B: Integer): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := IntegerSimpleCompare(A, B);
end;

function TJclIntegerAbstractContainer.ItemsEqual(A, B: Integer): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := IntegerSimpleEqualityCompare(A, B);
end;

procedure TJclIntegerAbstractContainer.SetCompare(Value: TIntegerCompare);
begin
  FCompare := Value;
end;

procedure TJclIntegerAbstractContainer.SetEqualityCompare(Value: TIntegerEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclIntegerAbstractContainer.SetHashConvert(Value: TIntegerHashConvert);
begin
  FHashConvert := Value;
end;

procedure TJclIntegerAbstractContainer.SetOnFreeInteger(
  Value: TFreeIntegerEvent);
begin
  FOnFreeInteger := Value;
end;

//=== { TJclCardinalAbstractContainer } ======================================

procedure TJclCardinalAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclCardinalAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclCardinalAbstractContainer then
  begin
    ADest := TJclCardinalAbstractContainer(Dest);
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclCardinalAbstractContainer.FreeCardinal(var AValue: Cardinal): Cardinal;
begin
  if Assigned(FOnFreeCardinal) then
    Result := FOnFreeCardinal(AValue)
  else
  begin
    Result := AValue;
    AValue := 0;
  end;
end;

function TJclCardinalAbstractContainer.GetCompare: TCardinalCompare;
begin
  Result := FCompare;
end;

function TJclCardinalAbstractContainer.GetEqualityCompare: TCardinalEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclCardinalAbstractContainer.GetHashConvert: TCardinalHashConvert;
begin
  Result := FHashConvert;
end;

function TJclCardinalAbstractContainer.GetOnFreeCardinal: TFreeCardinalEvent;
begin
  Result := FOnFreeCardinal;
end;

function TJclCardinalAbstractContainer.Hash(AValue: Cardinal): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AValue)
  else
    Result := CardinalSimpleHashConvert(AValue);
end;

function TJclCardinalAbstractContainer.ItemsCompare(A, B: Cardinal): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := CardinalSimpleCompare(A, B);
end;

function TJclCardinalAbstractContainer.ItemsEqual(A, B: Cardinal): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := CardinalSimpleEqualityCompare(A, B);
end;

procedure TJclCardinalAbstractContainer.SetCompare(Value: TCardinalCompare);
begin
  FCompare := Value;
end;

procedure TJclCardinalAbstractContainer.SetEqualityCompare(Value: TCardinalEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclCardinalAbstractContainer.SetHashConvert(Value: TCardinalHashConvert);
begin
  FHashConvert := Value;
end;

procedure TJclCardinalAbstractContainer.SetOnFreeCardinal(
  Value: TFreeCardinalEvent);
begin
  FOnFreeCardinal := Value;
end;

//=== { TJclInt64AbstractContainer } =========================================

procedure TJclInt64AbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclInt64AbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclInt64AbstractContainer then
  begin
    ADest := TJclInt64AbstractContainer(Dest);
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclInt64AbstractContainer.FreeInt64(var AValue: Int64): Int64;
begin
  if Assigned(FOnFreeInt64) then
    Result := FOnFreeInt64(AValue)
  else
  begin
    Result := AValue;
    AValue := 0;
  end;
end;

function TJclInt64AbstractContainer.GetCompare: TInt64Compare;
begin
  Result := FCompare;
end;

function TJclInt64AbstractContainer.GetEqualityCompare: TInt64EqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclInt64AbstractContainer.GetHashConvert: TInt64HashConvert;
begin
  Result := FHashConvert;
end;

function TJclInt64AbstractContainer.GetOnFreeInt64: TFreeInt64Event;
begin
  Result := FOnFreeInt64;
end;

function TJclInt64AbstractContainer.Hash(const AValue: Int64): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AValue)
  else
    Result := Int64SimpleHashConvert(AValue);
end;

function TJclInt64AbstractContainer.ItemsCompare(const A, B: Int64): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := Int64SimpleCompare(A, B);
end;

function TJclInt64AbstractContainer.ItemsEqual(const A, B: Int64): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := Int64SimpleEqualityCompare(A, B);
end;

procedure TJclInt64AbstractContainer.SetCompare(Value: TInt64Compare);
begin
  FCompare := Value;
end;

procedure TJclInt64AbstractContainer.SetEqualityCompare(Value: TInt64EqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclInt64AbstractContainer.SetHashConvert(Value: TInt64HashConvert);
begin
  FHashConvert := Value;
end;

procedure TJclInt64AbstractContainer.SetOnFreeInt64(Value: TFreeInt64Event);
begin
  FOnFreeInt64 := Value;
end;

//=== { TJclPtrAbstractContainer } ===========================================

procedure TJclPtrAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclPtrAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclPtrAbstractContainer then
  begin
    ADest := TJclPtrAbstractContainer(Dest);
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclPtrAbstractContainer.FreePointer(var APtr: Pointer): Pointer;
begin
  if Assigned(FOnFreePointer) then
    Result := FOnFreePointer(APtr)
  else
  begin
    Result := APtr;
    APtr := nil;
  end;
end;

function TJclPtrAbstractContainer.GetCompare: TPtrCompare;
begin
  Result := FCompare;
end;

function TJclPtrAbstractContainer.GetEqualityCompare: TPtrEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclPtrAbstractContainer.GetHashConvert: TPtrHashConvert;
begin
  Result := FHashConvert;
end;

function TJclPtrAbstractContainer.GetOnFreePointer: TFreePtrEvent;
begin
  Result := FOnFreePointer;
end;

function TJclPtrAbstractContainer.Hash(APtr: Pointer): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(APtr)
  else
    Result := PtrSimpleHashConvert(APtr);
end;

function TJclPtrAbstractContainer.ItemsCompare(A, B: Pointer): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := PtrSimpleCompare(A, B);
end;

function TJclPtrAbstractContainer.ItemsEqual(A, B: Pointer): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := PtrSimpleEqualityCompare(A, B);
end;
procedure TJclPtrAbstractContainer.SetCompare(Value: TPtrCompare);
begin
  FCompare := Value;
end;

procedure TJclPtrAbstractContainer.SetEqualityCompare(Value: TPtrEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclPtrAbstractContainer.SetHashConvert(Value: TPtrHashConvert);
begin
  FHashConvert := Value;
end;

procedure TJclPtrAbstractContainer.SetOnFreePointer(Value: TFreePtrEvent);
begin
  FOnFreePointer := Value;
end;

//=== { TJclAbstractContainer } ==============================================

constructor TJclAbstractContainer.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

procedure TJclAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclAbstractContainer then
  begin
    ADest := TJclAbstractContainer(Dest);
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclAbstractContainer.FreeObject(var AObject: TObject): TObject;
begin
  if Assigned(FOnFreeObject) then
    Result := FOnFreeObject(AObject)
  else
  if FOwnsObjects then
  begin
    Result := nil;
    FreeAndNil(AObject);
  end
  else
  begin
    Result := AObject;
    AObject := nil;
  end;
end;

function TJclAbstractContainer.GetCompare: TCompare;
begin
  Result := FCompare;
end;

function TJclAbstractContainer.GetEqualityCompare: TEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclAbstractContainer.GetHashConvert: THashConvert;
begin
  Result := FHashConvert;
end;

function TJclAbstractContainer.GetOnFreeObject: TFreeObjectEvent;
begin
  Result := FOnFreeObject;
end;

function TJclAbstractContainer.GetOwnsObjects: Boolean;
begin
  Result := FOwnsObjects;
end;

function TJclAbstractContainer.Hash(AObject: TObject): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AObject)
  else
    Result := SimpleHashConvert(AObject);
end;

function TJclAbstractContainer.ItemsCompare(A, B: TObject): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := SimpleCompare(A, B);
end;

function TJclAbstractContainer.ItemsEqual(A, B: TObject): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := SimpleEqualityCompare(A, B);
end;

procedure TJclAbstractContainer.SetCompare(Value: TCompare);
begin
  FCompare := Value;
end;

procedure TJclAbstractContainer.SetEqualityCompare(Value: TEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclAbstractContainer.SetHashConvert(Value: THashConvert);
begin
  FHashConvert := Value;
end;

procedure TJclAbstractContainer.SetOnFreeObject(Value: TFreeObjectEvent);
begin
  FOnFreeObject := Value;
end;

{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

//=== { TJclAbstractContainer<T> } ===========================================

constructor TJclAbstractContainer<T>.Create(AOwnsItems: Boolean);
begin
  inherited Create;
  FOwnsItems := AOwnsItems;
end;

procedure TJclAbstractContainer<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclAbstractContainer<T>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclAbstractContainer<T> then
  begin
    ADest := TJclAbstractContainer<T>(Dest);
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclAbstractContainer<T>.FreeItem(var AItem: T): T;
begin
  if Assigned(FOnFreeItem) then
    Result := FOnFreeItem(AItem)
  else
  if FOwnsItems then
  begin
    Result := Default(T);
    FreeAndNil(AItem);
  end
  else
  begin
    Result := AItem;
    AItem := Default(T);
  end;
end;

function TJclAbstractContainer<T>.GetCompare: TCompare<T>;
begin
  Result := FCompare;
end;

function TJclAbstractContainer<T>.GetEqualityCompare: TEqualityCompare<T>;
begin
  Result := FEqualityCompare;
end;

function TJclAbstractContainer<T>.GetHashConvert: THashConvert<T>;
begin
  Result := FHashConvert;
end;

function TJclAbstractContainer<T>.GetOnFreeItem: TFreeItemEvent<T>;
begin
  Result := FOnFreeItem;
end;

function TJclAbstractContainer<T>.GetOwnsItems: Boolean;
begin
  Result := FOwnsItems;
end;

function TJclAbstractContainer<T>.Hash(const AItem: T): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AItem)
  else
    raise EJclOperationNotSupportedError.Create;
end;

function TJclAbstractContainer<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    raise EJclOperationNotSupportedError.Create;
end;

function TJclAbstractContainer<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAbstractContainer<T>.SetCompare(Value: TCompare<T>);
begin
  FCompare := Value;
end;

procedure TJclAbstractContainer<T>.SetEqualityCompare(Value: TEqualityCompare<T>);
begin
  FEqualityCompare := Value;
end;

procedure TJclAbstractContainer<T>.SetHashConvert(Value: THashConvert<T>);
begin
  FHashConvert := Value;
end;

procedure TJclAbstractContainer<T>.SetOnFreeItem(Value: TFreeItemEvent<T>);
begin
  FOnFreeItem := Value;
end;

//DOM-IGNORE-END
{$ENDIF SUPPORTS_GENERICS}

//=== { TJclAnsiStrAbstractCollection } ======================================

// TODO: common implementation, need a function to search for a string starting from
// a predefined index
procedure TJclAnsiStrAbstractCollection.AppendDelimited(const AString, Separator: AnsiString);
var
  Item: AnsiString;
  SepLen: Integer;
  PString, PSep, PPos: PAnsiChar;
begin
  PString := PAnsiChar(AString);
  PSep := PAnsiChar(Separator);
  PPos := StrPosA(PString, PSep);
  if PPos <> nil then
  begin
    SepLen := StrLenA(PSep);
    repeat
      //SetLength(Item, PPos - PString + 1);
      SetLength(Item, PPos - PString);
      Move(PString^, Item[1], (PPos - PString) * SizeOf(AnsiChar));
      //Item[PPos - PString + 1] := #0;
      Add(Item);
      PString := PPos + SepLen;
      PPos := StrPosA(PString, PSep);
    until PPos = nil;
    if StrLenA(PString) > 0 then //ex. hello#world
      Add(PString);
  end
  else //There isnt a Separator in AString
    Add(AString);
end;

procedure TJclAnsiStrAbstractCollection.AppendFromStrings(Strings: TJclAnsiStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Add(Strings[I]);
end;

procedure TJclAnsiStrAbstractCollection.AppendToStrings(Strings: TJclAnsiStrings);
var
  It: IJclAnsiStrIterator;
begin
  It := First;
  Strings.BeginUpdate;
  try
    while It.HasNext do
      Strings.Add(It.Next);
  finally
    Strings.EndUpdate;
  end;
end;

function TJclAnsiStrAbstractCollection.GetAsDelimited(const Separator: AnsiString): AnsiString;
var
  It: IJclAnsiStrIterator;
begin
  It := First;
  Result := '';
  if It.HasNext then
    Result := It.Next;
  while It.HasNext do
    Result := Result + Separator + It.Next;
end;

function TJclAnsiStrAbstractCollection.GetAsStrings: TJclAnsiStrings;
begin
  Result := TJclAnsiStringList.Create;
  try
    AppendToStrings(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TJclAnsiStrAbstractCollection.LoadDelimited(const AString, Separator: AnsiString);
begin
  Clear;
  AppendDelimited(AString, Separator);
end;

procedure TJclAnsiStrAbstractCollection.LoadFromStrings(Strings: TJclAnsiStrings);
begin
  Clear;
  AppendFromStrings(Strings);
end;

procedure TJclAnsiStrAbstractCollection.SaveToStrings(Strings: TJclAnsiStrings);
begin
  Strings.Clear;
  AppendToStrings(Strings);
end;

//=== { TJclWideStrAbstractCollection } ======================================

// TODO: common implementation, need a function to search for a string starting from
// a predefined index
procedure TJclWideStrAbstractCollection.AppendDelimited(const AString, Separator: WideString);
var
  Item: WideString;
  SepLen: Integer;
  PString, PSep, PPos: PWideChar;
begin
  PString := PWideChar(AString);
  PSep := PWideChar(Separator);
  PPos := StrPosW(PString, PSep);
  if PPos <> nil then
  begin
    SepLen := StrLenW(PSep);
    repeat
      //SetLength(Item, PPos - PString + 1);
      SetLength(Item, PPos - PString);
      Move(PString^, Item[1], (PPos - PString) * SizeOf(WideChar));
      //Item[PPos - PString + 1] := #0;
      Add(Item);
      PString := PPos + SepLen;
      PPos := StrPosW(PString, PSep);
    until PPos = nil;
    if StrLenW(PString) > 0 then //ex. hello#world
      Add(PString);
  end
  else //There isnt a Separator in AString
    Add(AString);
end;

procedure TJclWideStrAbstractCollection.AppendFromStrings(Strings: TJclWideStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Add(Strings[I]);
end;

procedure TJclWideStrAbstractCollection.AppendToStrings(Strings: TJclWideStrings);
var
  It: IJclWideStrIterator;
begin
  It := First;
  Strings.BeginUpdate;
  try
    while It.HasNext do
      Strings.Add(It.Next);
  finally
    Strings.EndUpdate;
  end;
end;

function TJclWideStrAbstractCollection.GetAsDelimited(const Separator: WideString): WideString;
var
  It: IJclWideStrIterator;
begin
  It := First;
  Result := '';
  if It.HasNext then
    Result := It.Next;
  while It.HasNext do
    Result := Result + Separator + It.Next;
end;

function TJclWideStrAbstractCollection.GetAsStrings: TJclWideStrings;
begin
  Result := TJclWideStringList.Create;
  try
    AppendToStrings(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TJclWideStrAbstractCollection.LoadDelimited(const AString, Separator: WideString);
begin
  Clear;
  AppendDelimited(AString, Separator);
end;

procedure TJclWideStrAbstractCollection.LoadFromStrings(Strings: TJclWideStrings);
begin
  Clear;
  AppendFromStrings(Strings);
end;

procedure TJclWideStrAbstractCollection.SaveToStrings(Strings: TJclWideStrings);
begin
  Strings.Clear;
  AppendToStrings(Strings);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrAbstractCollection } ===================================

// TODO: common implementation, need a function to search for a string starting from
// a predefined index
procedure TJclUnicodeStrAbstractCollection.AppendDelimited(const AString, Separator: UnicodeString);
var
  Item: UnicodeString;
  SepLen: Integer;
  PString, PSep, PPos: PWideChar;
begin
  PString := PWideChar(AString);
  PSep := PWideChar(Separator);
  PPos := StrPos(PString, PSep);
  if PPos <> nil then
  begin
    SepLen := StrLen(PSep);
    repeat
      //SetLength(Item, PPos - PString + 1);
      SetLength(Item, PPos - PString);
      Move(PString^, Item[1], (PPos - PString) * SizeOf(WideChar));
      //Item[PPos - PString + 1] := #0;
      Add(Item);
      PString := PPos + SepLen;
      PPos := StrPos(PString, PSep);
    until PPos = nil;
    if StrLen(PString) > 0 then //ex. hello#world
      Add(PString);
  end
  else //There isnt a Separator in AString
    Add(AString);
end;

procedure TJclUnicodeStrAbstractCollection.AppendFromStrings(Strings: TJclUnicodeStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Add(Strings[I]);
end;

procedure TJclUnicodeStrAbstractCollection.AppendToStrings(Strings: TJclUnicodeStrings);
var
  It: IJclUnicodeStrIterator;
begin
  It := First;
  Strings.BeginUpdate;
  try
    while It.HasNext do
      Strings.Add(It.Next);
  finally
    Strings.EndUpdate;
  end;
end;

function TJclUnicodeStrAbstractCollection.GetAsDelimited(const Separator: UnicodeString): UnicodeString;
var
  It: IJclUnicodeStrIterator;
begin
  It := First;
  Result := '';
  if It.HasNext then
    Result := It.Next;
  while It.HasNext do
    Result := Result + Separator + It.Next;
end;

function TJclUnicodeStrAbstractCollection.GetAsStrings: TJclUnicodeStrings;
begin
  Result := TJclUnicodeStringList.Create;
  try
    AppendToStrings(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TJclUnicodeStrAbstractCollection.LoadDelimited(const AString, Separator: UnicodeString);
begin
  Clear;
  AppendDelimited(AString, Separator);
end;

procedure TJclUnicodeStrAbstractCollection.LoadFromStrings(Strings: TJclUnicodeStrings);
begin
  Clear;
  AppendFromStrings(Strings);
end;

procedure TJclUnicodeStrAbstractCollection.SaveToStrings(Strings: TJclUnicodeStrings);
begin
  Strings.Clear;
  AppendToStrings(Strings);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

