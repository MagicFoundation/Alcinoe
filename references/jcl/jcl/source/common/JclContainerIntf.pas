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
{ The Original Code is DCL_intf.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
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

unit JclContainerIntf;

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
  JclBase,
  JclAnsiStrings,
  JclWideStrings;

{$IFDEF BCB6}
{$DEFINE BUGGY_DEFAULT_INDEXED_PROP}
{$ENDIF BCB6}
{$IFDEF BCB10}
{$DEFINE BUGGY_DEFAULT_INDEXED_PROP}
{$ENDIF BCB10}
{$IFDEF BCB11}
{$DEFINE BUGGY_DEFAULT_INDEXED_PROP}
{$ENDIF BCB11}

const
  DefaultContainerCapacity = 16;

type
  // function pointer types

  // iterate functions Type -> (void)
  TIntfIterateProcedure = procedure(const AInterface: IInterface);
  TAnsiStrIterateProcedure = procedure(const AString: AnsiString);
  TWideStrIterateProcedure = procedure(const AString: WideString);
  {$IFDEF SUPPORTS_UNICODE_STRING}
  TUnicodeStrIterateProcedure = procedure(const AString: UnicodeString);
  {$ENDIF SUPPORTS_UNICODE_STRING}
  {$IFDEF CONTAINER_ANSISTR}
  TStrIterateProcedure = TAnsiStrIterateProcedure;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TStrIterateProcedure = TWideStrIterateProcedure;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TStrIterateProcedure = TUnicodeStrIterateProcedure;
  {$ENDIF CONTAINER_UNICODESTR}
  TSingleIterateProcedure = procedure(const AValue: Single);
  TDoubleIterateProcedure = procedure(const AValue: Double);
  TExtendedIterateProcedure = procedure(const AValue: Extended);
  {$IFDEF MATH_SINGLE_PRECISION}
  TFloatIterateProcedure = TSingleIterateProcedure;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TFloatIterateProcedure = TDoubleIterateProcedure;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TFloatIterateProcedure = TExtendedIterateProcedure;
  {$ENDIF MATH_EXTENDED_PRECISION}
  TIntegerIterateProcedure = procedure(AValue: Integer);
  TCardinalIterateProcedure = procedure(AValue: Cardinal);
  TInt64IterateProcedure = procedure(const AValue: Int64);
  TPtrIterateProcedure = procedure(APtr: Pointer);
  TIterateProcedure = procedure(AObject: TObject);

  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  TIterateProcedure<T> = procedure(const AItem: T);
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  // apply functions Type -> Type
  TIntfApplyFunction = function(const AInterface: IInterface): IInterface;
  TAnsiStrApplyFunction = function(const AString: AnsiString): AnsiString;
  TWideStrApplyFunction = function(const AString: WideString): WideString;
  {$IFDEF SUPPORTS_UNICODE_STRING}
  TUnicodeStrApplyFunction = function(const AString: UnicodeString): UnicodeString;
  {$ENDIF SUPPORTS_UNICODE_STRING}
  {$IFDEF CONTAINER_ANSISTR}
  TStrApplyFunction = TAnsiStrApplyFunction;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TStrApplyFunction = TWideStrApplyFunction;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TStrApplyFunction = TUnicodeStrApplyFunction;
  {$ENDIF CONTAINER_UNICODESTR}
  TSingleApplyFunction = function(const AValue: Single): Single;
  TDoubleApplyFunction = function(const AValue: Double): Double;
  TExtendedApplyFunction = function(const AValue: Extended): Extended;
  {$IFDEF MATH_SINGLE_PRECISION}
  TFloatApplyFunction = TSingleApplyFunction;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TFloatApplyFunction = TDoubleApplyFunction;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TFloatApplyFunction = TExtendedApplyFunction;
  {$ENDIF MATH_EXTENDED_PRECISION}
  TIntegerApplyFunction = function(AValue: Integer): Integer;
  TCardinalApplyFunction = function(AValue: Cardinal): Cardinal;
  TInt64ApplyFunction = function(const AValue: Int64): Int64;
  TPtrApplyFunction = function(APtr: Pointer): Pointer;
  TApplyFunction = function(AObject: TObject): TObject;

  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  TApplyFunction<T> = function(const AItem: T): T;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  // comparison functions Type -> Type -> Integer
  TIntfCompare = function(const Obj1, Obj2: IInterface): Integer;
  TAnsiStrCompare = function(const Obj1, Obj2: AnsiString): Integer;
  TWideStrCompare = function(const Obj1, Obj2: WideString): Integer;
  {$IFDEF SUPPORTS_UNICODE_STRING}
  TUnicodeStrCompare = function(const Obj1, Obj2: UnicodeString): Integer;
  {$ENDIF SUPPORTS_UNICODE_STRING}
  {$IFDEF CONTAINER_ANSISTR}
  TStrCompare = TAnsiStrCompare;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TStrCompare = TWideStrCompare;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TStrCompare = TUnicodeStrCompare;
  {$ENDIF CONTAINER_UNICODESTR}
  TSingleCompare = function(const Obj1, Obj2: Single): Integer;
  TDoubleCompare = function(const Obj1, Obj2: Double): Integer;
  TExtendedCompare = function(const Obj1, Obj2: Extended): Integer;
  {$IFDEF MATH_SINGLE_PRECISION}
  TFloatCompare = TSingleCompare;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TFloatCompare = TDoubleCompare;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TFloatCompare = TExtendedCompare;
  {$ENDIF MATH_EXTENDED_PRECISION}
  TIntegerCompare = function(Obj1, Obj2: Integer): Integer;
  TCardinalCompare = function(Obj1, Obj2: Cardinal): Integer;
  TInt64Compare = function(const Obj1, Obj2: Int64): Integer;
  TPtrCompare = function(Obj1, Obj2: Pointer): Integer;
  TCompare = function(Obj1, Obj2: TObject): Integer;

  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  TCompare<T> = function(const Obj1, Obj2: T): Integer;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  // comparison for equality functions Type -> Type -> Boolean
  TIntfEqualityCompare = function(const Obj1, Obj2: IInterface): Boolean;
  TAnsiStrEqualityCompare = function(const Obj1, Obj2: AnsiString): Boolean;
  TWideStrEqualityCompare = function(const Obj1, Obj2: WideString): Boolean;
  {$IFDEF SUPPORTS_UNICODE_STRING}
  TUnicodeStrEqualityCompare = function(const Obj1, Obj2: UnicodeString): Boolean;
  {$ENDIF SUPPORTS_UNICODE_STRING}
  {$IFDEF CONTAINER_ANSISTR}
  TStrEqualityCompare = TAnsiStrEqualityCompare;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TStrEqualityCompare = TWideStrEqualityCompare;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TStrEqualityCompare = TUnicodeStrEqualityCompare;
  {$ENDIF CONTAINER_UNICODESTR}
  TSingleEqualityCompare = function(const Obj1, Obj2: Single): Boolean;
  TDoubleEqualityCompare = function(const Obj1, Obj2: Double): Boolean;
  TExtendedEqualityCompare = function(const Obj1, Obj2: Extended): Boolean;
  {$IFDEF MATH_SINGLE_PRECISION}
  TFloatEqualityCompare = TSingleEqualityCompare;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TFloatEqualityCompare = TDoubleEqualityCompare;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TFloatEqualityCompare = TExtendedEqualityCompare;
  {$ENDIF MATH_EXTENDED_PRECISION}
  TIntegerEqualityCompare = function(Obj1, Obj2: Integer): Boolean;
  TCardinalEqualityCompare = function(Obj1, Obj2: Cardinal): Boolean;
  TInt64EqualityCompare = function(const Obj1, Obj2: Int64): Boolean;
  TPtrEqualityCompare = function(Obj1, Obj2: Pointer): Boolean;
  TEqualityCompare = function(Obj1, Obj2: TObject): Boolean;

  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  TEqualityCompare<T> = function(const Obj1, Obj2: T): Boolean;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  // hash functions Type -> Integer
  TIntfHashConvert = function(const AInterface: IInterface): Integer;
  TAnsiStrHashConvert = function(const AString: AnsiString): Integer;
  TWideStrHashConvert = function(const AString: WideString): Integer;
  {$IFDEF SUPPORTS_UNICODE_STRING}
  TUnicodeStrHashConvert = function(const AString: UnicodeString): Integer;
  {$ENDIF SUPPORTS_UNICODE_STRING}
  {$IFDEF CONTAINER_ANSISTR}
  TStrHashConvert = TAnsiStrHashConvert;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TStrHashConvert = TWideStrHashConvert;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TStrHashConvert = TUnicodeStrHashConvert;
  {$ENDIF CONTAINER_UNICODESTR}
  TSingleHashConvert = function(const AValue: Single): Integer;
  TDoubleHashConvert = function(const AValue: Double): Integer;
  TExtendedHashConvert = function(const AValue: Extended): Integer;
  {$IFDEF MATH_SINGLE_PRECISION}
  TFloatHashConvert = TSingleHashConvert;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TFloatHashConvert = TDoubleHashConvert;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TFloatHashConvert = TExtendedHashConvert;
  {$ENDIF MATH_EXTENDED_PRECISION}
  TIntegerHashConvert = function(AValue: Integer): Integer;
  TCardinalHashConvert = function(AValue: Cardinal): Integer;
  TInt64HashConvert = function(const AValue: Int64): Integer;
  TPtrHashConvert = function(APtr: Pointer): Integer;
  THashConvert = function(AObject: TObject): Integer;

  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  THashConvert<T> = function(const AItem: T): Integer;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclLockable = interface
    ['{524AD65E-AE1B-4BC6-91C8-8181F0198BA9}']
    procedure ReadLock;
    procedure ReadUnlock;
    procedure WriteLock;
    procedure WriteUnlock;
  end;

  IJclAbstractIterator = interface{$IFDEF THREADSAFE}(IJclLockable){$ENDIF THREADSAFE}
    ['{1064D0B4-D9FC-475D-88BE-520490013B46}']
    procedure Assign(const Source: IJclAbstractIterator);
    procedure AssignTo(const Dest: IJclAbstractIterator);
    function GetIteratorReference: TObject;
  end;

  IJclBaseContainer = interface{$IFDEF THREADSAFE}(IJclLockable){$ENDIF THREADSAFE}
    ['{C517175A-028E-486A-BF27-5EF7FC3101D9}']
    procedure Assign(const Source: IJclBaseContainer);
    procedure AssignTo(const Dest: IJclBaseContainer);
    function GetAllowDefaultElements: Boolean;
    function GetContainerReference: TObject;
    function GetDuplicates: TDuplicates;
    function GetReadOnly: Boolean;
    function GetRemoveSingleElement: Boolean;
    function GetReturnDefaultElements: Boolean;
    function GetThreadSafe: Boolean;
    procedure SetAllowDefaultElements(Value: Boolean);
    procedure SetDuplicates(Value: TDuplicates);
    procedure SetReadOnly(Value: Boolean);
    procedure SetRemoveSingleElement(Value: Boolean);
    procedure SetReturnDefaultElements(Value: Boolean);
    procedure SetThreadSafe(Value: Boolean);
    property AllowDefaultElements: Boolean read GetAllowDefaultElements write SetAllowDefaultElements;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property RemoveSingleElement: Boolean read GetRemoveSingleElement write SetRemoveSingleElement;
    property ReturnDefaultElements: Boolean read GetReturnDefaultElements write SetReturnDefaultElements;
    property ThreadSafe: Boolean read GetThreadSafe write SetThreadSafe;
  end;

  IJclIntfContainer = interface(IJclBaseContainer)
    ['{44F10075-9702-4DCA-9731-D8990F234A74}']
  end;

  IJclIntfFlatContainer = interface(IJclIntfContainer)
    ['{15116007-6BB8-4D9D-8249-C2F49D4AB3EA}']
  end;

  IJclStrBaseContainer = interface(IJclBaseContainer)
    ['{9753E1D7-F093-4D5C-8B32-40403F6F700E}']
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(Value: Boolean);
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
  end;

  TJclAnsiStrEncoding = (seISO, seUTF8);

  IJclAnsiStrContainer = interface(IJclStrBaseContainer)
    ['{F8239357-B96F-46F1-A48E-B5DF25B5F1FA}']
    function GetEncoding: TJclAnsiStrEncoding;
    procedure SetEncoding(Value: TJclAnsiStrEncoding);
    property Encoding: TJclAnsiStrEncoding read GetEncoding write SetEncoding;
  end;

  IJclAnsiStrFlatContainer = interface(IJclAnsiStrContainer)
    ['{8A45A4D4-6317-4CDF-8314-C3E5CC6899F4}']
    procedure LoadFromStrings(Strings: TJclAnsiStrings);
    procedure SaveToStrings(Strings: TJclAnsiStrings);
    procedure AppendToStrings(Strings: TJclAnsiStrings);
    procedure AppendFromStrings(Strings: TJclAnsiStrings);
    function GetAsStrings: TJclAnsiStrings;
    function GetAsDelimited(const Separator: AnsiString = AnsiLineBreak): AnsiString;
    procedure AppendDelimited(const AString: AnsiString; const Separator: AnsiString = AnsiLineBreak);
    procedure LoadDelimited(const AString: AnsiString; const Separator: AnsiString = AnsiLineBreak);
  end;

  TJclWideStrEncoding = (seUTF16);

  IJclWideStrContainer = interface(IJclStrBaseContainer)
    ['{875E1AC4-CA22-46BC-8999-048E5B9BF11D}']
    function GetEncoding: TJclWideStrEncoding;
    procedure SetEncoding(Value: TJclWideStrEncoding);
    property Encoding: TJclWideStrEncoding read GetEncoding write SetEncoding;
  end;

  IJclWideStrFlatContainer = interface(IJclWideStrContainer)
    ['{5B001B93-CA1C-47A8-98B8-451CCB444930}']
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
  IJclUnicodeStrContainer = interface(IJclStrBaseContainer)
    ['{619BA29F-5E05-464D-B472-1C8453DBC707}']
  end;

  IJclUnicodeStrFlatContainer = interface(IJclUnicodeStrContainer)
    ['{3343D73E-4ADC-458E-8289-A4B83D1479D1}']
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

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrContainer = IJclAnsiStrContainer;
  IJclStrFlatContainer = IJclAnsiStrFlatContainer;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrContainer = IJclWideStrContainer;
  IJclStrFlatContainer = IJclWideStrFlatContainer;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrContainer = IJclUnicodeStrContainer;
  IJclStrFlatContainer = IJclUnicodeStrFlatContainer;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleContainer = interface(IJclBaseContainer)
    ['{22BE88BD-87D1-4B4D-9FAB-F1B6D555C6A9}']
    function GetPrecision: Single;
    procedure SetPrecision(const Value: Single);
    property Precision: Single read GetPrecision write SetPrecision;
  end;

  IJclSingleFlatContainer = interface(IJclSingleContainer)
    ['{F16955E8-94D2-4201-809B-CC2EA39B5FDD}']
  end;

  IJclDoubleContainer = interface(IJclBaseContainer)
    ['{372B9354-DF6D-4CAA-A5A9-C50E1FEE5525}']
    function GetPrecision: Double;
    procedure SetPrecision(const Value: Double);
    property Precision: Double read GetPrecision write SetPrecision;
  end;

  IJclDoubleFlatContainer = interface(IJclDoubleContainer)
    ['{2F0252CE-7471-45CA-8C8D-FD3925507C00}']
  end;

  IJclExtendedContainer = interface(IJclBaseContainer)
    ['{431A6482-FD5C-45A7-BE53-339A3CF75AC9}']
    function GetPrecision: Extended;
    procedure SetPrecision(const Value: Extended);
    property Precision: Extended read GetPrecision write SetPrecision;
  end;

  IJclExtendedFlatContainer = interface(IJclExtendedContainer)
    ['{1D3F48A2-001E-48F7-8A54-B9F4CE837523}']
  end;

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatContainer = IJclExtendedContainer;
  IJclFloatFlatContainer = IJclExtendedFlatContainer;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatContainer = IJclDoubleContainer;
  IJclFloatFlatContainer = IJclDoubleFlatContainer;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatContainer = IJclSingleContainer;
  IJclFloatFlatContainer = IJclSingleFlatContainer;
  {$ENDIF MATH_SINGLE_PRECISION}

  IJclIntegerContainer = interface(IJclBaseContainer)
    ['{3BAF5447-9835-43A4-9FF3-E5EA7D43A7D1}']
  end;

  IJclIntegerFlatContainer = interface(IJclIntegerContainer)
    ['{EF4EFCD9-60CB-4525-9D20-18E55291F7CF}']
  end;

  IJclCardinalContainer = interface(IJclBaseContainer)
    ['{01DF05CF-62E9-46B3-8BC1-2830EEF43644}']
  end;

  IJclCardinalFlatContainer = interface(IJclCardinalContainer)
    ['{79E48B80-3215-47D0-A1B5-D74C495AC9D1}']
  end;

  IJclInt64Container = interface(IJclBaseContainer)
    ['{B560B2B6-F8C7-45F0-A5E5-920AA61C1540}']
  end;

  IJclInt64FlatContainer = interface(IJclInt64Container)
    ['{E740B9EF-7342-4CEF-B7FB-96C5267F5738}']
  end;

  IJclPtrContainer = interface(IJclBaseContainer)
    ['{E8DD2A85-1E12-4605-B517-7E3121C5624F}']
  end;

  IJclPtrFlatContainer = interface(IJclPtrContainer)
    ['{43C41789-DE71-4DA5-B4AC-3F53EB9459CD}']
  end;

  IJclContainer = interface(IJclBaseContainer)
    ['{A9EBED03-4993-426A-8449-30D98DC2AC90}']
  end;

  IJclFlatContainer = interface(IJclContainer)
    ['{0A070B6F-54A1-4B3D-A4E4-CFFAE2C7C57B}']
  end;

  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IJclContainer<T> = interface(IJclBaseContainer)
    ['{19599A90-F392-430D-878D-A73E096C04AF}']
  end;

  IJclFlatContainer<T> = interface(IJclContainer<T>)
    ['{F562ECFB-98DC-4A82-A806-ED978B9D1667}']
  end;

  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfEqualityComparer = interface
    ['{5CC2DF51-BE56-4D02-A171-31BAAC097632}']
    function GetEqualityCompare: TIntfEqualityCompare;
    procedure SetEqualityCompare(Value: TIntfEqualityCompare);
    function ItemsEqual(const A, B: IInterface): Boolean;
    property EqualityCompare: TIntfEqualityCompare read GetEqualityCompare write SetEqualityCompare;
  end;

  IJclAnsiStrEqualityComparer = interface
    ['{E3DB9016-F0D0-4CE0-B156-4C5DCA47FD3B}']
    function GetEqualityCompare: TAnsiStrEqualityCompare;
    procedure SetEqualityCompare(Value: TAnsiStrEqualityCompare);
    function ItemsEqual(const A, B: AnsiString): Boolean;
    property EqualityCompare: TAnsiStrEqualityCompare read GetEqualityCompare write SetEqualityCompare;
  end;

  IJclWideStrEqualityComparer = interface
    ['{2E5696C9-8374-4347-9DC9-B3722F47F5FB}']
    function GetEqualityCompare: TWideStrEqualityCompare;
    procedure SetEqualityCompare(Value: TWideStrEqualityCompare);
    function ItemsEqual(const A, B: WideString): Boolean;
    property EqualityCompare: TWideStrEqualityCompare read GetEqualityCompare write SetEqualityCompare;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrEqualityComparer = interface
    ['{EDFCC1C7-79DB-4F58-BD64-5016B44EEAC0}']
    function GetEqualityCompare: TUnicodeStrEqualityCompare;
    procedure SetEqualityCompare(Value: TUnicodeStrEqualityCompare);
    function ItemsEqual(const A, B: UnicodeString): Boolean;
    property EqualityCompare: TUnicodeStrEqualityCompare read GetEqualityCompare write SetEqualityCompare;
  end;

  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrEqualityComparer = IJclAnsiStrEqualityComparer;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrEqualityComparer = IJclWideStrEqualityComparer;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrEqualityComparer = IJclUnicodeStrEqualityComparer;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleEqualityComparer = interface
    ['{4835BC5B-1A87-4864-BFE1-778F3BAF26B1}']
    function GetEqualityCompare: TSingleEqualityCompare;
    procedure SetEqualityCompare(Value: TSingleEqualityCompare);
    function ItemsEqual(const A, B: Single): Boolean;
    property EqualityCompare: TSingleEqualityCompare read GetEqualityCompare write SetEqualityCompare;
  end;

  IJclDoubleEqualityComparer = interface
    ['{15F0A9F0-D5DC-4978-8CDB-53B6E510262C}']
    function GetEqualityCompare: TDoubleEqualityCompare;
    procedure SetEqualityCompare(Value: TDoubleEqualityCompare);
    function ItemsEqual(const A, B: Double): Boolean;
    property EqualityCompare: TDoubleEqualityCompare read GetEqualityCompare write SetEqualityCompare;
  end;

  IJclExtendedEqualityComparer = interface
    ['{149883D5-4138-4570-8C5C-99F186B7E646}']
    function GetEqualityCompare: TExtendedEqualityCompare;
    procedure SetEqualityCompare(Value: TExtendedEqualityCompare);
    function ItemsEqual(const A, B: Extended): Boolean;
    property EqualityCompare: TExtendedEqualityCompare read GetEqualityCompare write SetEqualityCompare;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatEqualityComparer = IJclSingleEqualityComparer;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatEqualityComparer = IJclDoubleEqualityComparer;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatEqualityComparer = IJclExtendedEqualityComparer;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerEqualityComparer = interface
    ['{AABC35E6-A779-4A44-B748-27BFCB34FDFB}']
    function GetEqualityCompare: TIntegerEqualityCompare;
    procedure SetEqualityCompare(Value: TIntegerEqualityCompare);
    function ItemsEqual(A, B: Integer): Boolean;
    property EqualityCompare: TIntegerEqualityCompare read GetEqualityCompare write SetEqualityCompare;
  end;

  IJclCardinalEqualityComparer = interface
    ['{B2DECF81-6ECE-4D9F-80E1-C8C884DB407C}']
    function GetEqualityCompare: TCardinalEqualityCompare;
    procedure SetEqualityCompare(Value: TCardinalEqualityCompare);
    function ItemsEqual(A, B: Cardinal): Boolean;
    property EqualityCompare: TCardinalEqualityCompare read GetEqualityCompare write SetEqualityCompare;
  end;

  IJclInt64EqualityComparer = interface
    ['{8B2825E2-0C81-42BA-AC0D-104344CE7E56}']
    function GetEqualityCompare: TInt64EqualityCompare;
    procedure SetEqualityCompare(Value: TInt64EqualityCompare);
    function ItemsEqual(const A, B: Int64): Boolean;
    property EqualityCompare: TInt64EqualityCompare read GetEqualityCompare write SetEqualityCompare;
  end;

  IJclPtrEqualityComparer = interface
    ['{C6B7CBF9-ECD9-4D70-85CC-4E2367A1D806}']
    function GetEqualityCompare: TPtrEqualityCompare;
    procedure SetEqualityCompare(Value: TPtrEqualityCompare);
    function ItemsEqual(A, B: Pointer): Boolean;
    property EqualityCompare: TPtrEqualityCompare read GetEqualityCompare write SetEqualityCompare;
  end;

  IJclEqualityComparer = interface
    ['{82C67986-8365-44AB-8D56-7B0CF4F6B918}']
    function GetEqualityCompare: TEqualityCompare;
    procedure SetEqualityCompare(Value: TEqualityCompare);
    function ItemsEqual(A, B: TObject): Boolean;
    property EqualityCompare: TEqualityCompare read GetEqualityCompare write SetEqualityCompare;
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IJclEqualityComparer<T> = interface
    ['{4AF79AD6-D9F4-424B-BEAA-68857F9222B4}']
    function GetEqualityCompare: TEqualityCompare<T>;
    procedure SetEqualityCompare(Value: TEqualityCompare<T>);
    function ItemsEqual(const A, B: T): Boolean;
    property EqualityCompare: TEqualityCompare<T> read GetEqualityCompare write SetEqualityCompare;
  end;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfComparer = interface
    ['{EB41B843-184B-420D-B5DA-27D055B4CD55}']
    function GetCompare: TIntfCompare;
    procedure SetCompare(Value: TIntfCompare);
    function ItemsCompare(const A, B: IInterface): Integer;
    property Compare: TIntfCompare read GetCompare write SetCompare;
  end;

  IJclAnsiStrComparer = interface
    ['{09063CBB-9226-4734-B2A0-A178C2343176}']
    function GetCompare: TAnsiStrCompare;
    procedure SetCompare(Value: TAnsiStrCompare);
    function ItemsCompare(const A, B: AnsiString): Integer;
    property Compare: TAnsiStrCompare read GetCompare write SetCompare;
  end;

  IJclWideStrComparer = interface
    ['{7A24AEDA-25B1-4E73-B2E9-5D74011E4C9C}']
    function GetCompare: TWideStrCompare;
    procedure SetCompare(Value: TWideStrCompare);
    function ItemsCompare(const A, B: WideString): Integer;
    property Compare: TWideStrCompare read GetCompare write SetCompare;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrComparer = interface
    ['{E81E2705-0CA0-4DBD-BECC-5F9AA623A6E4}']
    function GetCompare: TUnicodeStrCompare;
    procedure SetCompare(Value: TUnicodeStrCompare);
    function ItemsCompare(const A, B: UnicodeString): Integer;
    property Compare: TUnicodeStrCompare read GetCompare write SetCompare;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrComparer = IJclAnsiStrComparer;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrComparer = IJclWideStrComparer;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrComparer = IJclUnicodeStrComparer;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleComparer = interface
    ['{008225CE-075E-4450-B9DE-9863CB6D347C}']
    function GetCompare: TSingleCompare;
    procedure SetCompare(Value: TSingleCompare);
    function ItemsCompare(const A, B: Single): Integer;
    property Compare: TSingleCompare read GetCompare write SetCompare;
  end;

  IJclDoubleComparer = interface
    ['{BC245D7F-7EB9-43D0-81B4-EE215486A5AA}']
    function GetCompare: TDoubleCompare;
    procedure SetCompare(Value: TDoubleCompare);
    function ItemsCompare(const A, B: Double): Integer;
    property Compare: TDoubleCompare read GetCompare write SetCompare;
  end;

  IJclExtendedComparer = interface
    ['{92657C66-C18D-4BF8-A538-A3B0140320BB}']
    function GetCompare: TExtendedCompare;
    procedure SetCompare(Value: TExtendedCompare);
    function ItemsCompare(const A, B: Extended): Integer;
    property Compare: TExtendedCompare read GetCompare write SetCompare;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatComparer = IJclSingleComparer;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatComparer = IJclDoubleComparer;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatComparer = IJclExtendedComparer;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerComparer = interface
    ['{362C3A6A-CBC1-4D5F-8652-158913DC9865}']
    function GetCompare: TIntegerCompare;
    procedure SetCompare(Value: TIntegerCompare);
    function ItemsCompare(A, B: Integer): Integer;
    property Compare: TIntegerCompare read GetCompare write SetCompare;
  end;

  IJclCardinalComparer = interface
    ['{56E44725-00B9-4530-8CC2-72DCA9171EE0}']
    function GetCompare: TCardinalCompare;
    procedure SetCompare(Value: TCardinalCompare);
    function ItemsCompare(A, B: Cardinal): Integer;
    property Compare: TCardinalCompare read GetCompare write SetCompare;
  end;

  IJclInt64Comparer = interface
    ['{87C935BF-3A42-4F1F-A474-9C823939EE1C}']
    function GetCompare: TInt64Compare;
    procedure SetCompare(Value: TInt64Compare);
    function ItemsCompare(const A, B: Int64): Integer;
    property Compare: TInt64Compare read GetCompare write SetCompare;
  end;

  IJclPtrComparer = interface
    ['{85557D4C-A036-477E-BA73-B5EEF43A8696}']
    function GetCompare: TPtrCompare;
    procedure SetCompare(Value: TPtrCompare);
    function ItemsCompare(A, B: Pointer): Integer;
    property Compare: TPtrCompare read GetCompare write SetCompare;
  end;

  IJclComparer = interface
    ['{7B376028-56DC-4C4A-86A9-1AC19E3EDF75}']
    function GetCompare: TCompare;
    procedure SetCompare(Value: TCompare);
    function ItemsCompare(A, B: TObject): Integer;
    property Compare: TCompare read GetCompare write SetCompare;
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IJclComparer<T> = interface
    ['{830AFC8C-AA06-46F5-AABD-8EB46B2A9986}']
    function GetCompare: TCompare<T>;
    procedure SetCompare(Value: TCompare<T>);
    function ItemsCompare(const A, B: T): Integer;
    property Compare: TCompare<T> read GetCompare write SetCompare;
  end;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfHashConverter = interface
    ['{7BAA0791-3B45-4D0F-9CD8-D13B81694786}']
    function GetHashConvert: TIntfHashConvert;
    procedure SetHashConvert(Value: TIntfHashConvert);
    function Hash(const AInterface: IInterface): Integer;
    property HashConvert: TIntfHashConvert read GetHashConvert write SetHashConvert;
  end;

  IJclAnsiStrHashConverter = interface
    ['{9841014E-8A31-4C79-8AD5-EB03C4E85533}']
    function GetHashConvert: TAnsiStrHashConvert;
    procedure SetHashConvert(Value: TAnsiStrHashConvert);
    function Hash(const AString: AnsiString): Integer;
    property HashConvert: TAnsiStrHashConvert read GetHashConvert write SetHashConvert;
  end;

  IJclWideStrHashConverter = interface
    ['{2584118F-19AE-443E-939B-0DB18BCD0117}']
    function GetHashConvert: TWideStrHashConvert;
    procedure SetHashConvert(Value: TWideStrHashConvert);
    function Hash(const AString: WideString): Integer;
    property HashConvert: TWideStrHashConvert read GetHashConvert write SetHashConvert;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrHashConverter = interface
    ['{08CD8171-DBAF-405F-9802-46D955C8BBE6}']
    function GetHashConvert: TUnicodeStrHashConvert;
    procedure SetHashConvert(Value: TUnicodeStrHashConvert);
    function Hash(const AString: UnicodeString): Integer;
    property HashConvert: TUnicodeStrHashConvert read GetHashConvert write SetHashConvert;
  end;

  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrHashConverter = IJclAnsiStrHashConverter;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrHashConverter = IJclWideStrHashConverter;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrHashConverter = IJclUnicodeStrHashConverter;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleHashConverter = interface
    ['{20F0E481-F1D2-48B6-A95D-FBB56AF119F5}']
    function GetHashConvert: TSingleHashConvert;
    procedure SetHashConvert(Value: TSingleHashConvert);
    function Hash(const AValue: Single): Integer;
    property HashConvert: TSingleHashConvert read GetHashConvert write SetHashConvert;
  end;

  IJclDoubleHashConverter = interface
    ['{193A2881-535B-4AF4-B0C3-6845A2800F80}']
    function GetHashConvert: TDoubleHashConvert;
    procedure SetHashConvert(Value: TDoubleHashConvert);
    function Hash(const AValue: Double): Integer;
    property HashConvert: TDoubleHashConvert read GetHashConvert write SetHashConvert;
  end;

  IJclExtendedHashConverter = interface
    ['{77CECDB9-2774-4FDC-8E5A-A80325626434}']
    function GetHashConvert: TExtendedHashConvert;
    procedure SetHashConvert(Value: TExtendedHashConvert);
    function Hash(const AValue: Extended): Integer;
    property HashConvert: TExtendedHashConvert read GetHashConvert write SetHashConvert;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatHashConverter = IJclSingleHashConverter;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatHashConverter = IJclDoubleHashConverter;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatHashConverter = IJclExtendedHashConverter;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerHashConverter = interface
    ['{92C540B2-C16C-47E4-995A-644BE71878B1}']
    function GetHashConvert: TIntegerHashConvert;
    procedure SetHashConvert(Value: TIntegerHashConvert);
    function Hash(AValue: Integer): Integer;
    property HashConvert: TIntegerHashConvert read GetHashConvert write SetHashConvert;
  end;

  IJclCardinalHashConverter = interface
    ['{2DF04C8A-16B8-4712-BC5D-AD35014EC9F7}']
    function GetHashConvert: TCardinalHashConvert;
    procedure SetHashConvert(Value: TCardinalHashConvert);
    function Hash(AValue: Cardinal): Integer;
    property HashConvert: TCardinalHashConvert read GetHashConvert write SetHashConvert;
  end;

  IJclInt64HashConverter = interface
    ['{96CF2A71-9185-4E26-B283-457ABC3584E7}']
    function GetHashConvert: TInt64HashConvert;
    procedure SetHashConvert(Value: TInt64HashConvert);
    function Hash(const AValue: Int64): Integer;
    property HashConvert: TInt64HashConvert read GetHashConvert write SetHashConvert;
  end;

  IJclPtrHashConverter = interface
    ['{D704CC67-CFED-44E6-9504-65D5E468FCAF}']
    function GetHashConvert: TPtrHashConvert;
    procedure SetHashConvert(Value: TPtrHashConvert);
    function Hash(APtr: Pointer): Integer;
    property HashConvert: TPtrHashConvert read GetHashConvert write SetHashConvert;
  end;

  IJclHashConverter = interface
    ['{2D0DD6F4-162E-41D6-8A34-489E7EACABCD}']
    function GetHashConvert: THashConvert;
    procedure SetHashConvert(Value: THashConvert);
    function Hash(AObject: TObject): Integer;
    property HashConvert: THashConvert read GetHashConvert write SetHashConvert;
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IJclHashConverter<T> = interface
    ['{300AEA0E-7433-4C3E-99A6-E533212ACF42}']
    function GetHashConvert: THashConvert<T>;
    procedure SetHashConvert(Value: THashConvert<T>);
    function Hash(const AItem: T): Integer;
    property HashConvert: THashConvert<T> read GetHashConvert write SetHashConvert;
  end;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfCloneable = interface
    ['{BCF77740-FB60-4306-9BD1-448AADE5FF4E}']
    function IntfClone: IInterface;
  end;

  IJclCloneable = interface
    ['{D224AE70-2C93-4998-9479-1D513D75F2B2}']
    function ObjectClone: TObject;
  end;

  TJclAutoPackStrategy = (apsDisabled, apsAgressive, apsProportional, apsIncremental);

  // parameter signification depends on strategy
  //  - Disabled = unused (arrays are never packed)
  //  - Agressive = unused (arrays are always packed)
  //  - Proportional = ratio of empty slots before the array is packed
  //    number of empty slots is computed by this formula: Capacity div Parameter
  //  - Incremental = amount of empty slots before the array is packed

  IJclPackable = interface
    ['{03802D2B-E0AB-4300-A777-0B8A2BD993DF}']
    function CalcGrowCapacity(ACapacity, ASize: Integer): Integer;
    function GetAutoPackParameter: Integer;
    function GetAutoPackStrategy: TJclAutoPackStrategy;
    function GetCapacity: Integer;
    procedure Pack; // reduce used memory by eliminating empty storage area (force)
    procedure SetAutoPackParameter(Value: Integer);
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy);
    procedure SetCapacity(Value: Integer);
    property AutoPackParameter: Integer read GetAutoPackParameter write SetAutoPackParameter;
    property AutoPackStrategy: TJclAutoPackStrategy read GetAutoPackStrategy write SetAutoPackStrategy;
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  TJclAutoGrowStrategy = (agsDisabled, agsAgressive, agsProportional, agsIncremental);

  // parameter signification depends on strategy
  //  - Disabled = unused (arrays never grow)
  //  - Agressive = unused (arrays always grow by 1 element)
  //  - Proportional = ratio of empty slots to add to the array
  //    number of empty slots is computed by this formula: Capacity div Parameter
  //  - Incremental = amount of empty slots to add to the array

  IJclGrowable = interface(IJclPackable)
    ['{C71E8586-5688-444C-9BDD-9969D988123B}']
    function CalcPackCapacity(ACapacity, ASize: Integer): Integer;
    function GetAutoGrowParameter: Integer;
    function GetAutoGrowStrategy: TJclAutoGrowStrategy;
    procedure Grow;
    procedure SetAutoGrowParameter(Value: Integer);
    procedure SetAutoGrowStrategy(Value: TJclAutoGrowStrategy);
    property AutoGrowParameter: Integer read GetAutoGrowParameter write SetAutoGrowParameter;
    property AutoGrowStrategy: TJclAutoGrowStrategy read GetAutoGrowStrategy write SetAutoGrowStrategy;
  end;

  TFreeIntfEvent = function (var AInterface: IInterface): IInterface of object;

  IJclIntfOwner = interface(IInterface)
    ['{17C1D3FB-BB32-48F2-BD1C-D43EA05A86A8}']
    function GetOnFreeObject: TFreeIntfEvent;
    function FreeObject(var AInterface: IInterface): IInterface;
    procedure SetOnFreeObject(Value: TFreeIntfEvent);
    property OnFreeObject: TFreeIntfEvent read GetOnFreeObject write SetOnFreeObject;
  end;

  TFreeAnsiStrEvent = function (var AString: AnsiString): AnsiString of object;

  IJclAnsiStrOwner = interface(IInterface)
    ['{4F64F1F6-766A-4CFA-B51B-654116E308A8}']
    function GetOnFreeString: TFreeAnsiStrEvent;
    function FreeString(var AString: AnsiString): AnsiString;
    procedure SetOnFreeString(Value: TFreeAnsiStrEvent);
    property OnFreeString: TFreeAnsiStrEvent read GetOnFreeString write SetOnFreeString;
  end;

  TFreeWideStrEvent = function (var AString: WideString): WideString of object;

  IJclWideStrOwner = interface(IInterface)
    ['{282B7A64-BCD0-4EAE-8776-4EF92D7E3D8B}']
    function GetOnFreeString: TFreeWideStrEvent;
    function FreeString(var AString: WideString): WideString;
    procedure SetOnFreeString(Value: TFreeWideStrEvent);
    property OnFreeString: TFreeWideStrEvent read GetOnFreeString write SetOnFreeString;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TFreeUnicodeStrEvent = function (var AString: UnicodeString): UnicodeString of object;

  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrOwner = interface(IInterface)
    ['{07F402E6-DD97-4AA4-83D8-4CCD419FCCFC}']
    function GetOnFreeString: TFreeUnicodeStrEvent;
    function FreeString(var AString: UnicodeString): UnicodeString;
    procedure SetOnFreeString(Value: TFreeUnicodeStrEvent);
    property OnFreeString: TFreeUnicodeStrEvent read GetOnFreeString write SetOnFreeString;
  end;

  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TFreeStrEvent = TFreeAnsiStrEvent;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TFreeStrEvent = TFreeWideStrEvent;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TFreeStrEvent = TFreeUnicodeStrEvent;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrOwner = IJclAnsiStrOwner;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrOwner = IJclWideStrOwner;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrOwner = IJclUnicodeStrOwner;
  {$ENDIF CONTAINER_UNICODESTR}

  TFreeSingleEvent = function (var AValue: Single): Single of object;

  IJclSingleOwner = interface(IInterface)
    ['{B002C201-70D7-4FA8-B44A-6D18E82580E5}']
    function GetOnFreeSingle: TFreeSingleEvent;
    function FreeSingle(var AValue: Single): Single;
    procedure SetOnFreeSingle(Value: TFreeSingleEvent);
    property OnFreeSingle: TFreeSingleEvent read GetOnFreeSingle write SetOnFreeSingle;
  end;

  TFreeDoubleEvent = function (var AValue: Double): Double of object;

  IJclDoubleOwner = interface(IInterface)
    ['{3BEFEDB0-C904-4400-ABEF-40FC928BB258}']
    function GetOnFreeDouble: TFreeDoubleEvent;
    function FreeDouble(var AValue: Double): Double;
    procedure SetOnFreeDouble(Value: TFreeDoubleEvent);
    property OnFreeDouble: TFreeDoubleEvent read GetOnFreeDouble write SetOnFreeDouble;
  end;

  TFreeExtendedEvent = function (var AValue: Extended): Extended of object;

  IJclExtendedOwner = interface(IInterface)
    ['{4501B203-6784-479D-8A8E-FBE3E1249CCF}']
    function GetOnFreeExtended: TFreeExtendedEvent;
    function FreeExtended(var AValue: Extended): Extended;
    procedure SetOnFreeExtended(Value: TFreeExtendedEvent);
    property OnFreeExtended: TFreeExtendedEvent read GetOnFreeExtended write SetOnFreeExtended;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  TFreeFloatEvent = TFreeSingleEvent;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TFreeFloatEvent = TFreeDoubleEvent;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TFreeFloatEvent = TFreeExtendedEvent;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatOwner = IJclSingleOwner;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatOwner = IJclDoubleOwner;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatOwner = IJclExtendedOwner;
  {$ENDIF MATH_EXTENDED_PRECISION}

  TFreeIntegerEvent = function (var AValue: Integer): Integer of object;

  IJclIntegerOwner = interface(IInterface)
    ['{00E37ECB-0FF0-4833-8143-EB7FBEF9E208}']
    function GetOnFreeInteger: TFreeIntegerEvent;
    function FreeInteger(var AValue: Integer): Integer;
    procedure SetOnFreeInteger(Value: TFreeIntegerEvent);
    property OnFreeInteger: TFreeIntegerEvent read GetOnFreeInteger write SetOnFreeInteger;
  end;

  TFreeCardinalEvent = function (var AValue: Cardinal): Cardinal of object;

  IJclCardinalOwner = interface(IInterface)
    ['{27B3EDEF-0ACD-4592-95F2-52A1DF5E7A39}']
    function GetOnFreeCardinal: TFreeCardinalEvent;
    function FreeCardinal(var AValue: Cardinal): Cardinal;
    procedure SetOnFreeCardinal(Value: TFreeCardinalEvent);
    property OnFreeCardinal: TFreeCardinalEvent read GetOnFreeCardinal write SetOnFreeCardinal;
  end;

  TFreeInt64Event = function (var AValue: Int64): Int64 of object;

  IJclInt64Owner = interface(IInterface)
    ['{7D4A1375-057A-42B8-8DAA-52DE30058864}']
    function GetOnFreeInt64: TFreeInt64Event;
    function FreeInt64(var AValue: Int64): Int64;
    procedure SetOnFreeInt64(Value: TFreeInt64Event);
    property OnFreeInt64: TFreeInt64Event read GetOnFreeInt64 write SetOnFreeInt64;
  end;

  TFreePtrEvent = function (var APtr: Pointer): Pointer of object;

  IJclPtrOwner = interface(IInterface)
    ['{28340328-34AD-4632-9BAC-A7387A822200}']
    function GetOnFreePointer: TFreePtrEvent;
    function FreePointer(var APtr: Pointer): Pointer;
    procedure SetOnFreePointer(Value: TFreePtrEvent);
    property OnFreePointer: TFreePtrEvent read GetOnFreePointer write SetOnFreePointer;
  end;

  TFreeObjectEvent = function (var AObject: TObject): TObject of object;

  IJclObjectOwner = interface(IInterface)
    ['{5157EA13-924E-4A56-995D-36956441025C}']
    function GetOnFreeObject: TFreeObjectEvent;
    function FreeObject(var AObject: TObject): TObject;
    procedure SetOnFreeObject(Value: TFreeObjectEvent);
    property OnFreeObject: TFreeObjectEvent read GetOnFreeObject write SetOnFreeObject;
    function GetOwnsObjects: Boolean;
    property OwnsObjects: Boolean read GetOwnsObjects;
  end;



  IJclKeyOwner = interface
    ['{8BE209E6-2F85-44FD-B0CD-A8363C95349A}']
    function FreeKey(var Key: TObject): TObject;
    function GetOwnsKeys: Boolean;
    property OwnsKeys: Boolean read GetOwnsKeys;
  end;

  IJclValueOwner = interface
    ['{3BCD98CE-7056-416A-A9E7-AE3AB2A62E54}']
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read GetOwnsValues;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN

  TFreeItemEvent<T> = function (var AItem: T): T of object;

  IJclItemOwner<T> = interface(IInterface)
    ['{0CC220C1-E705-4B21-9F53-4AD340952165}']
    function GetOnFreeItem: TFreeItemEvent<T>;
    function FreeItem(var AItem: T): T;
    procedure SetOnFreeItem(Value: TFreeItemEvent<T>);
    property OnFreeItem: TFreeItemEvent<T> read GetOnFreeItem write SetOnFreeItem;

    function GetOwnsItems: Boolean;
    property OwnsItems: Boolean read GetOwnsItems;
  end;

  IJclPairOwner<TKey, TValue> = interface
    ['{321C1FF7-AA2E-4229-966A-7EC6417EA16D}']
    function FreeKey(var Key: TKey): TKey;
    function FreeValue(var Value: TValue): TValue;
    function GetOwnsKeys: Boolean;
    function GetOwnsValues: Boolean;
    property OwnsKeys: Boolean read GetOwnsKeys;
    property OwnsValues: Boolean read GetOwnsValues;
  end;

  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfIterator = interface(IJclAbstractIterator)
    ['{E121A98A-7C43-4587-806B-9189E8B2F106}']
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

  IJclAnsiStrIterator = interface(IJclAbstractIterator)
    ['{D5D4B681-F902-49C7-B9E1-73007C9D64F0}']
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

  IJclWideStrIterator = interface(IJclAbstractIterator)
    ['{F03BC7D4-CCDA-4C4A-AF3A-E51FDCDE8ADE}']
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
  IJclUnicodeStrIterator = interface(IJclAbstractIterator)
    ['{B913FFDC-792A-48FB-B58E-763EFDEBA15C}']
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
  IJclStrIterator = IJclAnsiStrIterator;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrIterator = IJclWideStrIterator;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrIterator = IJclUnicodeStrIterator;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleIterator = interface(IJclAbstractIterator)
    ['{FD1124F8-CB2B-4AD7-B12D-C05702F4204B}']
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

  IJclDoubleIterator = interface(IJclAbstractIterator)
    ['{004C154A-281C-4DA7-BF64-F3EE80ACF640}']
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

  IJclExtendedIterator = interface(IJclAbstractIterator)
    ['{B89877A5-DED4-4CD9-AB90-C7D062111DE0}']
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
  IJclFloatIterator = IJclSingleIterator;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatIterator = IJclDoubleIterator;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatIterator = IJclExtendedIterator;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerIterator = interface(IJclAbstractIterator)
    ['{1406A991-4574-48A1-83FE-2EDCA03908BE}']
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

  IJclCardinalIterator = interface(IJclAbstractIterator)
    ['{72847A34-C8C4-4592-9447-CEB8161E33AD}']
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

  IJclInt64Iterator = interface(IJclAbstractIterator)
    ['{573E5A51-BF76-43D7-9F93-46305BED20A8}']
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

  IJclPtrIterator = interface(IJclAbstractIterator)
    ['{62B5501C-07AA-4D00-A85B-713B39912CDF}']
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

  IJclIterator = interface(IJclAbstractIterator)
    ['{997DF9B7-9AA2-4239-8B94-14DFFD26D790}']
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
  IJclIterator<T> = interface(IJclAbstractIterator)
    ['{6E8547A4-5B5D-4831-8AE3-9C6D04071B11}']
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
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfTreeIterator = interface(IJclIntfIterator)
    ['{C97379BF-C6A9-4A90-9D7A-152E9BAD314F}']
    function AddChild(const AInterface: IInterface): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): IInterface;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AInterface: IInterface): Integer;
    function InsertChild(Index: Integer; const AInterface: IInterface): Boolean;
    function Parent: IInterface;
    procedure SetChild(Index: Integer; const AInterface: IInterface);
    property Children[Index: Integer]: IInterface read GetChild write SetChild;
  end;

  IJclAnsiStrTreeIterator = interface(IJclAnsiStrIterator)
    ['{66BC5C76-758C-4E72-ABF1-EB02CF851C6D}']
    function AddChild(const AString: AnsiString): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): AnsiString;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AString: AnsiString): Integer;
    function InsertChild(Index: Integer; const AString: AnsiString): Boolean;
    function Parent: AnsiString;
    procedure SetChild(Index: Integer; const AString: AnsiString);
    property Children[Index: Integer]: AnsiString read GetChild write SetChild;
  end;

  IJclWideStrTreeIterator = interface(IJclWideStrIterator)
    ['{B3168A3B-5A90-4ABF-855F-3D2B3AB6EE7F}']
    function AddChild(const AString: WideString): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): WideString;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AString: WideString): Integer;
    function InsertChild(Index: Integer; const AString: WideString): Boolean;
    function Parent: WideString;
    procedure SetChild(Index: Integer; const AString: WideString);
    property Children[Index: Integer]: WideString read GetChild write SetChild;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrTreeIterator = interface(IJclUnicodeStrIterator)
    ['{0B0A60DE-0403-4EE1-B1F0-10D849924CF8}']
    function AddChild(const AString: UnicodeString): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): UnicodeString;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AString: UnicodeString): Integer;
    function InsertChild(Index: Integer; const AString: UnicodeString): Boolean;
    function Parent: UnicodeString;
    procedure SetChild(Index: Integer; const AString: UnicodeString);
    property Children[Index: Integer]: UnicodeString read GetChild write SetChild;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrTreeIterator = IJclAnsiStrTreeIterator;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrTreeIterator = IJclWideStrTreeIterator;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrTreeIterator = IJclUnicodeStrTreeIterator;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleTreeIterator = interface(IJclSingleIterator)
    ['{17BFDE9D-DBF7-4DC8-AC74-919C717B4726}']
    function AddChild(const AValue: Single): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): Single;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AValue: Single): Integer;
    function InsertChild(Index: Integer; const AValue: Single): Boolean;
    function Parent: Single;
    procedure SetChild(Index: Integer; const AValue: Single);
    property Children[Index: Integer]: Single read GetChild write SetChild;
  end;

  IJclDoubleTreeIterator = interface(IJclDoubleIterator)
    ['{EB39B84E-D3C5-496E-A521-B8BF24579252}']
    function AddChild(const AValue: Double): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): Double;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AValue: Double): Integer;
    function InsertChild(Index: Integer; const AValue: Double): Boolean;
    function Parent: Double;
    procedure SetChild(Index: Integer; const AValue: Double);
    property Children[Index: Integer]: Double read GetChild write SetChild;
  end;

  IJclExtendedTreeIterator = interface(IJclExtendedIterator)
    ['{1B40A544-FC5D-454C-8E42-CE17B015E65C}']
    function AddChild(const AValue: Extended): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): Extended;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AValue: Extended): Integer;
    function InsertChild(Index: Integer; const AValue: Extended): Boolean;
    function Parent: Extended;
    procedure SetChild(Index: Integer; const AValue: Extended);
    property Children[Index: Integer]: Extended read GetChild write SetChild;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatTreeIterator = IJclSingleTreeIterator;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatTreeIterator = IJclDoubleTreeIterator;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatTreeIterator = IJclExtendedTreeIterator;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerTreeIterator = interface(IJclIntegerIterator)
    ['{88EDC5C5-CA41-41AF-9838-AA19D07E69F5}']
    function AddChild(AValue: Integer): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): Integer;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(AValue: Integer): Integer;
    function InsertChild(Index: Integer; AValue: Integer): Boolean;
    function Parent: Integer;
    procedure SetChild(Index: Integer; AValue: Integer);
    property Children[Index: Integer]: Integer read GetChild write SetChild;
  end;

  IJclCardinalTreeIterator = interface(IJclCardinalIterator)
    ['{FDBF493F-F79D-46EB-A59D-7193B6E6A860}']
    function AddChild(AValue: Cardinal): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): Cardinal;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(AValue: Cardinal): Integer;
    function InsertChild(Index: Integer; AValue: Cardinal): Boolean;
    function Parent: Cardinal;
    procedure SetChild(Index: Integer; AValue: Cardinal);
    property Children[Index: Integer]: Cardinal read GetChild write SetChild;
  end;

  IJclInt64TreeIterator = interface(IJclInt64Iterator)
    ['{C5A5E504-E19B-43AC-90B9-E4B8984BFA23}']
    function AddChild(const AValue: Int64): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): Int64;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AValue: Int64): Integer;
    function InsertChild(Index: Integer; const AValue: Int64): Boolean;
    function Parent: Int64;
    procedure SetChild(Index: Integer; const AValue: Int64);
    property Children[Index: Integer]: Int64 read GetChild write SetChild;
  end;

  IJclPtrTreeIterator = interface(IJclPtrIterator)
    ['{ED4C08E6-60FC-4ED3-BD19-E6605B9BD943}']
    function AddChild(APtr: Pointer): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): Pointer;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(APtr: Pointer): Integer;
    function InsertChild(Index: Integer; APtr: Pointer): Boolean;
    function Parent: Pointer;
    procedure SetChild(Index: Integer; APtr: Pointer);
    property Children[Index: Integer]: Pointer read GetChild write SetChild;
  end;

  IJclTreeIterator = interface(IJclIterator)
    ['{8B4863B0-B6B9-426E-B5B8-7AF71D264237}']
    function AddChild(AObject: TObject): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): TObject;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(AObject: TObject): Integer;
    function InsertChild(Index: Integer; AObject: TObject): Boolean;
    function Parent: TObject;
    procedure SetChild(Index: Integer; AObject: TObject);
    property Children[Index: Integer]: TObject read GetChild write SetChild;
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IJclTreeIterator<T> = interface(IJclIterator<T>)
    ['{29A06DA4-D93A-40A5-8581-0FE85BC8384B}']
    function AddChild(const AItem: T): Boolean;
    function ChildrenCount: Integer;
    procedure DeleteChild(Index: Integer);
    procedure DeleteChildren;
    procedure ExtractChild(Index: Integer);
    procedure ExtractChildren;
    function GetChild(Index: Integer): T;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AItem: T): Integer;
    function InsertChild(Index: Integer; const AItem: T): Boolean;
    function Parent: T;
    procedure SetChild(Index: Integer; const AItem: T);
    property Children[Index: Integer]: T read GetChild write SetChild;
  end;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfBinaryTreeIterator = interface(IJclIntfTreeIterator)
    ['{8BE874B2-0075-4EE0-8F49-665FC894D923}']
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: IInterface;
    function Right: IInterface;
  end;

  IJclAnsiStrBinaryTreeIterator = interface(IJclAnsiStrTreeIterator)
    ['{34A4A300-042C-43A9-AC23-8FC1B76BFB25}']
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: AnsiString;
    function Right: AnsiString;
  end;

  IJclWideStrBinaryTreeIterator = interface(IJclWideStrTreeIterator)
    ['{17C08EB9-6880-469E-878A-8F5EBFE905B1}']
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: WideString;
    function Right: WideString;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrBinaryTreeIterator = interface(IJclUnicodeStrTreeIterator)
    ['{CA32B126-AD4B-4C33-BC47-52B09FE093BE}']
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: UnicodeString;
    function Right: UnicodeString;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrBinaryTreeIterator = IJclAnsiStrBinaryTreeIterator;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrBinaryTreeIterator = IJclWideStrBinaryTreeIterator;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrBinaryTreeIterator = IJclUnicodeStrBinaryTreeIterator;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleBinaryTreeIterator = interface(IJclSingleTreeIterator)
    ['{BC6FFB13-FA1C-4077-8273-F25A3119168B}']
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Single;
    function Right: Single;
  end;

  IJclDoubleBinaryTreeIterator = interface(IJclDoubleTreeIterator)
    ['{CE48083C-D60C-4315-BC14-8CE77AC3269E}']
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Double;
    function Right: Double;
  end;

  IJclExtendedBinaryTreeIterator = interface(IJclExtendedTreeIterator)
    ['{8A9FAE2A-5EF5-4165-8E8D-51F2102A4580}']
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Extended;
    function Right: Extended;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatBinaryTreeIterator = IJclSingleBinaryTreeIterator;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatBinaryTreeIterator = IJclDoubleBinaryTreeIterator;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatBinaryTreeIterator = IJclExtendedBinaryTreeIterator;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerBinaryTreeIterator = interface(IJclIntegerTreeIterator)
    ['{FE2BF57D-D10D-4B0C-903D-BB61700FBA0A}']
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Integer;
    function Right: Integer;
  end;

  IJclCardinalBinaryTreeIterator = interface(IJclCardinalTreeIterator)
    ['{AAA358F5-95A1-480F-8E2A-09028BA6C397}']
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Cardinal;
    function Right: Cardinal;
  end;

  IJclInt64BinaryTreeIterator = interface(IJclInt64TreeIterator)
    ['{5605E164-5CDD-40B1-9323-DE1CB584E289}']
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Int64;
    function Right: Int64;
  end;

  IJclPtrBinaryTreeIterator = interface(IJclPtrTreeIterator)
    ['{75D3DF0D-C491-43F7-B078-E658197E8051}']
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Pointer;
    function Right: Pointer;
  end;

  IJclBinaryTreeIterator = interface(IJclTreeIterator)
    ['{821DE28D-631C-4F23-A0B2-CC0F35B4C64D}']
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: TObject;
    function Right: TObject;
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IJclBinaryTreeIterator<T> = interface(IJclTreeIterator<T>)
    ['{0CF5B0FC-C644-458C-BF48-2E093DAFEC26}']
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: T;
    function Right: T;
  end;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfCollection = interface(IJclIntfFlatContainer)
    ['{8E178463-4575-487A-B4D5-DC2AED3C7ACA}']
    function Add(const AInterface: IInterface): Boolean;
    function AddAll(const ACollection: IJclIntfCollection): Boolean;
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function ContainsAll(const ACollection: IJclIntfCollection): Boolean;
    function CollectionEquals(const ACollection: IJclIntfCollection): Boolean;
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
  end;

  IJclAnsiStrCollection = interface(IJclAnsiStrFlatContainer)
    ['{3E3CFC19-E8AF-4DD7-91FA-2DF2895FC7B9}']
    function Add(const AString: AnsiString): Boolean;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
    procedure Clear;
    function Contains(const AString: AnsiString): Boolean;
    function ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
    function CollectionEquals(const ACollection: IJclAnsiStrCollection): Boolean;
    function Extract(const AString: AnsiString): Boolean;
    function ExtractAll(const ACollection: IJclAnsiStrCollection): Boolean;
    function First: IJclAnsiStrIterator;
    function IsEmpty: Boolean;
    function Last: IJclAnsiStrIterator;
    function Remove(const AString: AnsiString): Boolean;
    function RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
    function RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclAnsiStrIterator;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  IJclWideStrCollection = interface(IJclWideStrFlatContainer)
    ['{CDCC0F94-4DD0-4F25-B441-6AE55D5C7466}']
    function Add(const AString: WideString): Boolean;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean;
    procedure Clear;
    function Contains(const AString: WideString): Boolean;
    function ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
    function CollectionEquals(const ACollection: IJclWideStrCollection): Boolean;
    function Extract(const AString: WideString): Boolean;
    function ExtractAll(const ACollection: IJclWideStrCollection): Boolean;
    function First: IJclWideStrIterator;
    function IsEmpty: Boolean;
    function Last: IJclWideStrIterator;
    function Remove(const AString: WideString): Boolean;
    function RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
    function RetainAll(const ACollection: IJclWideStrCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclWideStrIterator;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrCollection = interface(IJclUnicodeStrFlatContainer)
    ['{82EA7DDE-4EBF-4E0D-A380-CAF8A24C1A0D}']
    function Add(const AString: UnicodeString): Boolean;
    function AddAll(const ACollection: IJclUnicodeStrCollection): Boolean;
    procedure Clear;
    function Contains(const AString: UnicodeString): Boolean;
    function ContainsAll(const ACollection: IJclUnicodeStrCollection): Boolean;
    function CollectionEquals(const ACollection: IJclUnicodeStrCollection): Boolean;
    function Extract(const AString: UnicodeString): Boolean;
    function ExtractAll(const ACollection: IJclUnicodeStrCollection): Boolean;
    function First: IJclUnicodeStrIterator;
    function IsEmpty: Boolean;
    function Last: IJclUnicodeStrIterator;
    function Remove(const AString: UnicodeString): Boolean;
    function RemoveAll(const ACollection: IJclUnicodeStrCollection): Boolean;
    function RetainAll(const ACollection: IJclUnicodeStrCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclUnicodeStrIterator;
    {$ENDIF SUPPORTS_FOR_IN}
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrCollection = IJclAnsiStrCollection;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrCollection = IJclWideStrCollection;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrCollection = IJclUnicodeStrCollection;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleCollection = interface(IJclSingleFlatContainer)
    ['{1D34D474-6588-441E-B2B3-8C021A37ED89}']
    function Add(const AValue: Single): Boolean;
    function AddAll(const ACollection: IJclSingleCollection): Boolean;
    procedure Clear;
    function Contains(const AValue: Single): Boolean;
    function ContainsAll(const ACollection: IJclSingleCollection): Boolean;
    function CollectionEquals(const ACollection: IJclSingleCollection): Boolean;
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
  end;

  IJclDoubleCollection = interface(IJclDoubleFlatContainer)
    ['{E54C7717-C33A-4F1B-860C-4F60F303EAD3}']
    function Add(const AValue: Double): Boolean;
    function AddAll(const ACollection: IJclDoubleCollection): Boolean;
    procedure Clear;
    function Contains(const AValue: Double): Boolean;
    function ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
    function CollectionEquals(const ACollection: IJclDoubleCollection): Boolean;
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
  end;

  IJclExtendedCollection = interface(IJclExtendedFlatContainer)
    ['{2A1341CB-B997-4E3B-B1CA-6D60AE853C55}']
    function Add(const AValue: Extended): Boolean;
    function AddAll(const ACollection: IJclExtendedCollection): Boolean;
    procedure Clear;
    function Contains(const AValue: Extended): Boolean;
    function ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
    function CollectionEquals(const ACollection: IJclExtendedCollection): Boolean;
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
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatCollection = IJclSingleCollection;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatCollection = IJclDoubleCollection;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatCollection = IJclExtendedCollection;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerCollection = interface(IJclIntegerFlatContainer)
    ['{AF69890D-22D1-4D89-8FFD-5FAD7E0638BA}']
    function Add(AValue: Integer): Boolean;
    function AddAll(const ACollection: IJclIntegerCollection): Boolean;
    procedure Clear;
    function Contains(AValue: Integer): Boolean;
    function ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
    function CollectionEquals(const ACollection: IJclIntegerCollection): Boolean;
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
  end;

  IJclCardinalCollection = interface(IJclCardinalFlatContainer)
    ['{CFBD0344-58C8-4FA2-B4D7-D21D77DFBF80}']
    function Add(AValue: Cardinal): Boolean;
    function AddAll(const ACollection: IJclCardinalCollection): Boolean;
    procedure Clear;
    function Contains(AValue: Cardinal): Boolean;
    function ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
    function CollectionEquals(const ACollection: IJclCardinalCollection): Boolean;
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
  end;

  IJclInt64Collection = interface(IJclInt64FlatContainer)
    ['{93A45BDE-3C4C-48D6-9874-5322914DFDDA}']
    function Add(const AValue: Int64): Boolean;
    function AddAll(const ACollection: IJclInt64Collection): Boolean;
    procedure Clear;
    function Contains(const AValue: Int64): Boolean;
    function ContainsAll(const ACollection: IJclInt64Collection): Boolean;
    function CollectionEquals(const ACollection: IJclInt64Collection): Boolean;
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
  end;

  IJclPtrCollection = interface(IJclPtrFlatContainer)
    ['{02E909A7-5B1D-40D4-82EA-A0CD97D5C811}']
    function Add(APtr: Pointer): Boolean;
    function AddAll(const ACollection: IJclPtrCollection): Boolean;
    procedure Clear;
    function Contains(APtr: Pointer): Boolean;
    function ContainsAll(const ACollection: IJclPtrCollection): Boolean;
    function CollectionEquals(const ACollection: IJclPtrCollection): Boolean;
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
  end;

  IJclCollection = interface(IJclFlatContainer)
    ['{58947EF1-CD21-4DD1-AE3D-225C3AAD7EE5}']
    function Add(AObject: TObject): Boolean;
    function AddAll(const ACollection: IJclCollection): Boolean;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(const ACollection: IJclCollection): Boolean;
    function CollectionEquals(const ACollection: IJclCollection): Boolean;
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
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IJclCollection<T> = interface(IJclBaseContainer)
    ['{67EE8AF3-19B0-4DCA-A730-3C9B261B8EC5}']
    function Add(const AItem: T): Boolean;
    function AddAll(const ACollection: IJclCollection<T>): Boolean;
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function ContainsAll(const ACollection: IJclCollection<T>): Boolean;
    function CollectionEquals(const ACollection: IJclCollection<T>): Boolean;
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
  end;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfList = interface(IJclIntfCollection)
    ['{E14EDA4B-1DAA-4013-9E6C-CDCB365C7CF9}']
    function Delete(Index: Integer): IInterface;
    function ExtractIndex(Index: Integer): IInterface;
    function GetObject(Index: Integer): IInterface;
    function IndexOf(const AInterface: IInterface): Integer;
    function Insert(Index: Integer; const AInterface: IInterface): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
    function LastIndexOf(const AInterface: IInterface): Integer;
    procedure SetObject(Index: Integer; const AInterface: IInterface);
    function SubList(First, Count: Integer): IJclIntfList;
    property Objects[Key: Integer]: IInterface read GetObject write SetObject; default;
  end;

  IJclAnsiStrList = interface(IJclAnsiStrCollection)
    ['{07DD7644-EAC6-4059-99FC-BEB7FBB73186}']
    function Delete(Index: Integer): AnsiString;
    function ExtractIndex(Index: Integer): AnsiString;
    function GetString(Index: Integer): AnsiString;
    function IndexOf(const AString: AnsiString): Integer;
    function Insert(Index: Integer; const AString: AnsiString): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclAnsiStrCollection): Boolean;
    function LastIndexOf(const AString: AnsiString): Integer;
    procedure SetString(Index: Integer; const AString: AnsiString);
    function SubList(First, Count: Integer): IJclAnsiStrList;
    property Strings[Key: Integer]: AnsiString read GetString write SetString; default;
  end;

  IJclWideStrList = interface(IJclWideStrCollection)
    ['{C9955874-6AC0-4CE0-8CC0-606A3F1702C6}']
    function Delete(Index: Integer): WideString;
    function ExtractIndex(Index: Integer): WideString;
    function GetString(Index: Integer): WideString;
    function IndexOf(const AString: WideString): Integer;
    function Insert(Index: Integer; const AString: WideString): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclWideStrCollection): Boolean;
    function LastIndexOf(const AString: WideString): Integer;
    procedure SetString(Index: Integer; const AString: WideString);
    function SubList(First, Count: Integer): IJclWideStrList;
    property Strings[Key: Integer]: WideString read GetString write SetString; default;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrList = interface(IJclUnicodeStrCollection)
    ['{F4307EB4-D66E-4656-AC56-50883D0F2C83}']
    function Delete(Index: Integer): UnicodeString;
    function ExtractIndex(Index: Integer): UnicodeString;
    function GetString(Index: Integer): UnicodeString;
    function IndexOf(const AString: UnicodeString): Integer;
    function Insert(Index: Integer; const AString: UnicodeString): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclUnicodeStrCollection): Boolean;
    function LastIndexOf(const AString: UnicodeString): Integer;
    procedure SetString(Index: Integer; const AString: UnicodeString);
    function SubList(First, Count: Integer): IJclUnicodeStrList;
    property Strings[Key: Integer]: UnicodeString read GetString write SetString; default;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrList = IJclAnsiStrList;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrList = IJclWideStrList;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrList = IJclUnicodeStrList;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleList = interface(IJclSingleCollection)
    ['{D081324C-70A4-4AAC-BA42-7557F0262826}']
    function Delete(Index: Integer): Single;
    function ExtractIndex(Index: Integer): Single;
    function GetValue(Index: Integer): Single;
    function IndexOf(const AValue: Single): Integer;
    function Insert(Index: Integer; const AValue: Single): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclSingleCollection): Boolean;
    function LastIndexOf(const AValue: Single): Integer;
    procedure SetValue(Index: Integer; const AValue: Single);
    function SubList(First, Count: Integer): IJclSingleList;
    property Values[Key: Integer]: Single read GetValue write SetValue; default;
  end;

  IJclDoubleList = interface(IJclDoubleCollection)
    ['{ECA58515-3903-4312-9486-3214E03F35AB}']
    function Delete(Index: Integer): Double;
    function ExtractIndex(Index: Integer): Double;
    function GetValue(Index: Integer): Double;
    function IndexOf(const AValue: Double): Integer;
    function Insert(Index: Integer; const AValue: Double): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclDoubleCollection): Boolean;
    function LastIndexOf(const AValue: Double): Integer;
    procedure SetValue(Index: Integer; const AValue: Double);
    function SubList(First, Count: Integer): IJclDoubleList;
    property Values[Key: Integer]: Double read GetValue write SetValue; default;
  end;

  IJclExtendedList = interface(IJclExtendedCollection)
    ['{7463F954-F8DF-4B02-A284-FCB98746248E}']
    function Delete(Index: Integer): Extended;
    function ExtractIndex(Index: Integer): Extended;
    function GetValue(Index: Integer): Extended;
    function IndexOf(const AValue: Extended): Integer;
    function Insert(Index: Integer; const AValue: Extended): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclExtendedCollection): Boolean;
    function LastIndexOf(const AValue: Extended): Integer;
    procedure SetValue(Index: Integer; const AValue: Extended);
    function SubList(First, Count: Integer): IJclExtendedList;
    property Values[Key: Integer]: Extended read GetValue write SetValue; default;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatList = IJclSingleList;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatList = IJclDoubleList;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatList = IJclExtendedList;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerList = interface(IJclIntegerCollection)
    ['{339BE91B-557D-4CE0-A854-1CBD4FE31725}']
    function Delete(Index: Integer): Integer;
    function ExtractIndex(Index: Integer): Integer;
    function GetValue(Index: Integer): Integer;
    function IndexOf(AValue: Integer): Integer;
    function Insert(Index: Integer; AValue: Integer): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclIntegerCollection): Boolean;
    function LastIndexOf(AValue: Integer): Integer;
    procedure SetValue(Index: Integer; AValue: Integer);
    function SubList(First, Count: Integer): IJclIntegerList;
    property Values[Key: Integer]: Integer read GetValue write SetValue; default;
  end;

  IJclCardinalList = interface(IJclCardinalCollection)
    ['{02B09EA8-DE6F-4A18-AA57-C3533E6AC4E3}']
    function Delete(Index: Integer): Cardinal;
    function ExtractIndex(Index: Integer): Cardinal;
    function GetValue(Index: Integer): Cardinal;
    function IndexOf(AValue: Cardinal): Integer;
    function Insert(Index: Integer; AValue: Cardinal): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclCardinalCollection): Boolean;
    function LastIndexOf(AValue: Cardinal): Integer;
    procedure SetValue(Index: Integer; AValue: Cardinal);
    function SubList(First, Count: Integer): IJclCardinalList;
    property Values[Key: Integer]: Cardinal read GetValue write SetValue; default;
  end;

  IJclInt64List = interface(IJclInt64Collection)
    ['{E8D49200-91D3-4BD0-A59B-B93EC7E2074B}']
    function Delete(Index: Integer): Int64;
    function ExtractIndex(Index: Integer): Int64;
    function GetValue(Index: Integer): Int64;
    function IndexOf(const AValue: Int64): Integer;
    function Insert(Index: Integer; const AValue: Int64): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclInt64Collection): Boolean;
    function LastIndexOf(const AValue: Int64): Integer;
    procedure SetValue(Index: Integer; const AValue: Int64);
    function SubList(First, Count: Integer): IJclInt64List;
    property Values[Key: Integer]: Int64 read GetValue write SetValue; default;
  end;

  IJclPtrList = interface(IJclPtrCollection)
    ['{2CF5CF1F-C012-480C-A4CE-38BDAFB15D05}']
    function Delete(Index: Integer): Pointer;
    function ExtractIndex(Index: Integer): Pointer;
    function GetPointer(Index: Integer): Pointer;
    function IndexOf(APtr: Pointer): Integer;
    function Insert(Index: Integer; APtr: Pointer): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclPtrCollection): Boolean;
    function LastIndexOf(APtr: Pointer): Integer;
    procedure SetPointer(Index: Integer; APtr: Pointer);
    function SubList(First, Count: Integer): IJclPtrList;
    property Pointers[Key: Integer]: Pointer read GetPointer write SetPointer; default;
  end;

  IJclList = interface(IJclCollection)
    ['{8ABC70AC-5C06-43EA-AFE0-D066379BCC28}']
    function Delete(Index: Integer): TObject;
    function ExtractIndex(Index: Integer): TObject;
    function GetObject(Index: Integer): TObject;
    function IndexOf(AObject: TObject): Integer;
    function Insert(Index: Integer; AObject: TObject): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean;
    function LastIndexOf(AObject: TObject): Integer;
    procedure SetObject(Index: Integer; AObject: TObject);
    function SubList(First, Count: Integer): IJclList;
    property Objects[Key: Integer]: TObject read GetObject write SetObject; default;
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IJclList<T> = interface(IJclCollection<T>)
    ['{3B4BE3D7-8FF7-4163-91DF-3F73AE6935E7}']
    function Delete(Index: Integer): T;
    function ExtractIndex(Index: Integer): T;
    function GetItem(Index: Integer): T;
    function IndexOf(const AItem: T): Integer;
    function Insert(Index: Integer; const AItem: T): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
    function LastIndexOf(const AItem: T): Integer;
    procedure SetItem(Index: Integer; const AItem: T);
    function SubList(First, Count: Integer): IJclList<T>;
    property Items[Key: Integer]: T read GetItem write SetItem; default;
  end;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  // Pointer functions for sort algorithms
  TIntfSortProc = procedure(const AList: IJclIntfList; L, R: Integer; AComparator: TIntfCompare);
  TAnsiStrSortProc = procedure(const AList: IJclAnsiStrList; L, R: Integer; AComparator: TAnsiStrCompare);
  TWideStrSortProc = procedure(const AList: IJclWideStrList; L, R: Integer; AComparator: TWideStrCompare);
  {$IFDEF SUPPORTS_UNICODE_STRING}
  TUnicodeStrSortProc = procedure(const AList: IJclUnicodeStrList; L, R: Integer; AComparator: TUnicodeStrCompare);
  {$ENDIF SUPPORTS_UNICODE_STRING}
  {$IFDEF CONTAINER_ANSISTR}
  TStrSortProc = TAnsiStrSortProc;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TStrSortProc = TWideStrSortProc;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TStrSortProc = TUnicodeStrSortProc;
  {$ENDIF CONTAINER_UNICODESTR}
  TSingleSortProc = procedure(const AList: IJclSingleList; L, R: Integer; AComparator: TSingleCompare);
  TDoubleSortProc = procedure(const AList: IJclDoubleList; L, R: Integer; AComparator: TDoubleCompare);
  TExtendedSortProc = procedure(const AList: IJclExtendedList; L, R: Integer; AComparator: TExtendedCompare);
  {$IFDEF MATH_SINGLE_PRECISION}
  TFloatSortProc = TSingleSortProc;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TFloatSortProc = TDoubleSortProc;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TFloatSortProc = TExtendedSortProc;
  {$ENDIF MATH_EXTENDED_PRECISION}
  TIntegerSortProc = procedure(const AList: IJclIntegerList; L, R: Integer; AComparator: TIntegerCompare);
  TCardinalSortProc = procedure(const AList: IJclCardinalList; L, R: Integer; AComparator: TCardinalCompare);
  TInt64SortProc = procedure(const AList: IJclInt64List; L, R: Integer; AComparator: TInt64Compare);
  TPtrSortProc = procedure(const AList: IJclPtrList; L, R: Integer; AComparator: TPtrCompare);
  TSortProc = procedure(const AList: IJclList; L, R: Integer; AComparator: TCompare);

  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  TSortProc<T> = procedure(const AList: IJclList<T>; L, R: Integer; AComparator: TCompare<T>);
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfArray = interface(IJclIntfList)
    ['{B055B427-7817-43FC-97D4-AD1845643D63}']
    function GetObject(Index: Integer): IInterface;
    procedure SetObject(Index: Integer; const AInterface: IInterface);
    property Objects[Index: Integer]: IInterface read GetObject write SetObject; default;
  end;

  IJclAnsiStrArray = interface(IJclAnsiStrList)
    ['{4953EA83-9288-4537-9D10-544D1C992B62}']
    function GetString(Index: Integer): AnsiString;
    procedure SetString(Index: Integer; const AString: AnsiString);
    property Strings[Index: Integer]: AnsiString read GetString write SetString; default;
  end;

  IJclWideStrArray = interface(IJclWideStrList)
    ['{3CE09F9A-5CB4-4867-80D5-C2313D278D69}']
    function GetString(Index: Integer): WideString;
    procedure SetString(Index: Integer; const AString: WideString);
    property Strings[Index: Integer]: WideString read GetString write SetString; default;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrArray = interface(IJclUnicodeStrList)
    ['{24312E5B-B61D-485C-9E57-AC36C93D8159}']
    function GetString(Index: Integer): UnicodeString;
    procedure SetString(Index: Integer; const AString: UnicodeString);
    property Strings[Index: Integer]: UnicodeString read GetString write SetString; default;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrArray = IJclAnsiStrArray;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrArray = IJclWideStrArray;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrArray = IJclUnicodeStrArray;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleArray = interface(IJclSingleList)
    ['{B96E2A4D-D750-4B65-B975-C619A05A29F6}']
    function GetValue(Index: Integer): Single;
    procedure SetValue(Index: Integer; const AValue: Single);
    property Values[Index: Integer]: Single read GetValue write SetValue; default;
  end;

  IJclDoubleArray = interface(IJclDoubleList)
    ['{67E66324-9757-4E85-8ECD-53396910FB39}']
    function GetValue(Index: Integer): Double;
    procedure SetValue(Index: Integer; const AValue: Double);
    property Values[Index: Integer]: Double read GetValue write SetValue; default;
  end;

  IJclExtendedArray = interface(IJclExtendedList)
    ['{D43E8D18-26B3-41A2-8D52-ED7EA2FE1AB7}']
    function GetValue(Index: Integer): Extended;
    procedure SetValue(Index: Integer; const AValue: Extended);
    property Values[Index: Integer]: Extended read GetValue write SetValue; default;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatArray = IJclSingleArray;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatArray = IJclDoubleArray;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatArray = IJclExtendedArray;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerArray = interface(IJclIntegerList)
    ['{2B7C8B33-C0BD-4EC3-9764-63866E174781}']
    function GetValue(Index: Integer): Integer;
    procedure SetValue(Index: Integer; AValue: Integer);
    property Values[Index: Integer]: Integer read GetValue write SetValue; default;
  end;

  IJclCardinalArray = interface(IJclCardinalList)
    ['{C451F2F8-65C6-4C29-99A0-CC9C15356418}']
    function GetValue(Index: Integer): Cardinal;
    procedure SetValue(Index: Integer; AValue: Cardinal);
    property Values[Index: Integer]: Cardinal read GetValue write SetValue; default;
  end;

  IJclInt64Array = interface(IJclInt64List)
    ['{D947C43D-2D04-442A-A707-39EDE7D96FC9}']
    function GetValue(Index: Integer): Int64;
    procedure SetValue(Index: Integer; const AValue: Int64);
    property Values[Index: Integer]: Int64 read GetValue write SetValue; default;
  end;

  IJclPtrArray = interface(IJclPtrList)
    ['{D43E8D18-26B3-41A2-8D52-ED7EA2FE1AB7}']
    function GetPointer(Index: Integer): Pointer;
    procedure SetPointer(Index: Integer; APtr: Pointer);
    property Pointers[Index: Integer]: Pointer read GetPointer write SetPointer; default;
  end;

  IJclArray = interface(IJclList)
    ['{A69F6D35-54B2-4361-852E-097ED75E648A}']
    function GetObject(Index: Integer): TObject;
    procedure SetObject(Index: Integer; AObject: TObject);
    property Objects[Index: Integer]: TObject read GetObject write SetObject; default;
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IJclArray<T> = interface(IJclList<T>)
    ['{38810C13-E35E-428A-B84F-D25FB994BE8E}']
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const AItem: T);
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfSet = interface(IJclIntfCollection)
    ['{E2D28852-9774-49B7-A739-5DBA2B705924}']
    procedure Intersect(const ACollection: IJclIntfCollection);
    procedure Subtract(const ACollection: IJclIntfCollection);
    procedure Union(const ACollection: IJclIntfCollection);
  end;

  IJclAnsiStrSet = interface(IJclAnsiStrCollection)
    ['{72204D85-2B68-4914-B9F2-09E5180C12E9}']
    procedure Intersect(const ACollection: IJclAnsiStrCollection);
    procedure Subtract(const ACollection: IJclAnsiStrCollection);
    procedure Union(const ACollection: IJclAnsiStrCollection);
  end;

  IJclWideStrSet = interface(IJclWideStrCollection)
    ['{08009E0A-ABDD-46AB-8CEE-407D4723E17C}']
    procedure Intersect(const ACollection: IJclWideStrCollection);
    procedure Subtract(const ACollection: IJclWideStrCollection);
    procedure Union(const ACollection: IJclWideStrCollection);
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrSet = interface(IJclUnicodeStrCollection)
    ['{440E9BCB-341F-40B6-8AED-479B2E98C92A}']
    procedure Intersect(const ACollection: IJclUnicodeStrCollection);
    procedure Subtract(const ACollection: IJclUnicodeStrCollection);
    procedure Union(const ACollection: IJclUnicodeStrCollection);
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrSet = IJclAnsiStrSet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrSet = IJclWideStrSet;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrSet = IJclUnicodeStrSet;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleSet = interface(IJclSingleCollection)
    ['{36E34A78-6A29-4503-97D5-4BF53538CEC0}']
    procedure Intersect(const ACollection: IJclSingleCollection);
    procedure Subtract(const ACollection: IJclSingleCollection);
    procedure Union(const ACollection: IJclSingleCollection);
  end;

  IJclDoubleSet = interface(IJclDoubleCollection)
    ['{4E1E4847-E934-4811-A26C-5FC8E772A623}']
    procedure Intersect(const ACollection: IJclDoubleCollection);
    procedure Subtract(const ACollection: IJclDoubleCollection);
    procedure Union(const ACollection: IJclDoubleCollection);
  end;

  IJclExtendedSet = interface(IJclExtendedCollection)
    ['{3B9CF52D-1C49-4388-A7B3-9BEE1821FFD4}']
    procedure Intersect(const ACollection: IJclExtendedCollection);
    procedure Subtract(const ACollection: IJclExtendedCollection);
    procedure Union(const ACollection: IJclExtendedCollection);
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatSet = IJclSingleSet;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatSet = IJclDoubleSet;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatSet = IJclExtendedSet;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerSet = interface(IJclIntegerCollection)
    ['{5E4D29AF-F508-465B-9008-D11FF82F25FE}']
    procedure Intersect(const ACollection: IJclIntegerCollection);
    procedure Subtract(const ACollection: IJclIntegerCollection);
    procedure Union(const ACollection: IJclIntegerCollection);
  end;

  IJclCardinalSet = interface(IJclCardinalCollection)
    ['{09858637-CE8F-42E6-97E0-2786CD68387B}']
    procedure Intersect(const ACollection: IJclCardinalCollection);
    procedure Subtract(const ACollection: IJclCardinalCollection);
    procedure Union(const ACollection: IJclCardinalCollection);
  end;

  IJclInt64Set = interface(IJclInt64Collection)
    ['{ACB3127A-48EE-4F9F-B988-6AE9057780E9}']
    procedure Intersect(const ACollection: IJclInt64Collection);
    procedure Subtract(const ACollection: IJclInt64Collection);
    procedure Union(const ACollection: IJclInt64Collection);
  end;

  IJclPtrSet = interface(IJclPtrCollection)
    ['{26717C68-4F83-4CCB-973A-7324FBD09632}']
    procedure Intersect(const ACollection: IJclPtrCollection);
    procedure Subtract(const ACollection: IJclPtrCollection);
    procedure Union(const ACollection: IJclPtrCollection);
  end;

  IJclSet = interface(IJclCollection)
    ['{0B7CDB90-8588-4260-A54C-D87101C669EA}']
    procedure Intersect(const ACollection: IJclCollection);
    procedure Subtract(const ACollection: IJclCollection);
    procedure Union(const ACollection: IJclCollection);
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IJclSet<T> = interface(IJclCollection<T>)
    ['{0B7CDB90-8588-4260-A54C-D87101C669EA}']
    procedure Intersect(const ACollection: IJclCollection<T>);
    procedure Subtract(const ACollection: IJclCollection<T>);
    procedure Union(const ACollection: IJclCollection<T>);
  end;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  TJclTraverseOrder = (toPreOrder, toOrder, toPostOrder);

  IJclIntfTree = interface(IJclIntfCollection)
    ['{5A21688F-113D-41B4-A17C-54BDB0BD6559}']
    function GetRoot: IJclIntfTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclIntfTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  IJclAnsiStrTree = interface(IJclAnsiStrCollection)
    ['{1E1896C0-0497-47DF-83AF-A9422084636C}']
    function GetRoot: IJclAnsiStrTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclAnsiStrTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  IJclWideStrTree = interface(IJclWideStrCollection)
    ['{E325615A-7A20-4788-87FA-9051002CCD91}']
    function GetRoot: IJclWideStrTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclWideStrTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrTree = interface(IJclUnicodeStrCollection)
    ['{A378BC36-1FB1-4330-A335-037DD370E81B}']
    function GetRoot: IJclUnicodeStrTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclUnicodeStrTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrTree = IJclAnsiStrTree;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrTree = IJclWideStrTree;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrTree = IJclUnicodeStrTree;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleTree = interface(IJclSingleCollection)
    ['{A90A51BC-EBD7-40D3-B0A0-C9987E7A83D0}']
    function GetRoot: IJclSingleTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclSingleTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  IJclDoubleTree = interface(IJclDoubleCollection)
    ['{69DA85B1-A0DD-407B-B5CF-5EB7C6D4B82D}']
    function GetRoot: IJclDoubleTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclDoubleTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  IJclExtendedTree = interface(IJclExtendedCollection)
    ['{9ACCCAFD-B617-43DC-AAF9-916BE324A17E}']
    function GetRoot: IJclExtendedTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclExtendedTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatTree = IJclSingleTree;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatTree = IJclDoubleTree;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatTree = IJclExtendedTree;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerTree = interface(IJclIntegerCollection)
    ['{40A6F934-E5F3-4C74-AC02-227035C8C3C6}']
    function GetRoot: IJclIntegerTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclIntegerTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  IJclCardinalTree = interface(IJclCardinalCollection)
    ['{6C76C668-50C8-42A2-B72B-79BF102E270D}']
    function GetRoot: IJclCardinalTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclCardinalTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  IJclInt64Tree = interface(IJclInt64Collection)
    ['{1925B973-8B75-4A79-A993-DF2598FF19BE}']
    function GetRoot: IJclInt64TreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclInt64TreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  IJclPtrTree = interface(IJclPtrCollection)
    ['{2C1ACA3E-3F23-4E3C-984D-151CF9776E14}']
    function GetRoot: IJclPtrTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclPtrTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  IJclTree = interface(IJclCollection)
    ['{B0C658CC-FEF5-4178-A4C5-442C0DEDE207}']
    function GetRoot: IJclTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IJclTree<T> = interface(IJclCollection<T>)
    ['{3F963AB5-5A75-41F9-A21B-7E7FB541A459}']
    function GetRoot: IJclTreeIterator<T>;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property Root: IJclTreeIterator<T> read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfIntfMap = interface(IJclBaseContainer)
    ['{01D05399-4A05-4F3E-92F4-0C236BE77019}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: IInterface): IInterface;
    function GetValue(const Key: IInterface): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): IInterface;
    function KeySet: IJclIntfSet;
    function MapEquals(const AMap: IJclIntfIntfMap): Boolean;
    procedure PutAll(const AMap: IJclIntfIntfMap);
    procedure PutValue(const Key: IInterface; const Value: IInterface);
    function Remove(const Key: IInterface): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    property Items[const Key: IInterface]: IInterface read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclAnsiStrIntfMap = interface(IJclAnsiStrContainer)
    ['{A4788A96-281A-4924-AA24-03776DDAAD8A}']
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: AnsiString): IInterface;
    function GetValue(const Key: AnsiString): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): AnsiString;
    function KeySet: IJclAnsiStrSet;
    function MapEquals(const AMap: IJclAnsiStrIntfMap): Boolean;
    procedure PutAll(const AMap: IJclAnsiStrIntfMap);
    procedure PutValue(const Key: AnsiString; const Value: IInterface);
    function Remove(const Key: AnsiString): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    property Items[const Key: AnsiString]: IInterface read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclIntfAnsiStrMap = interface(IJclAnsiStrContainer)
    ['{B10E324A-1D98-42FF-B9B4-7F99044591B2}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: AnsiString): Boolean;
    function Extract(const Key: IInterface): AnsiString;
    function GetValue(const Key: IInterface): AnsiString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: AnsiString): IInterface;
    function KeySet: IJclIntfSet;
    function MapEquals(const AMap: IJclIntfAnsiStrMap): Boolean;
    procedure PutAll(const AMap: IJclIntfAnsiStrMap);
    procedure PutValue(const Key: IInterface; const Value: AnsiString);
    function Remove(const Key: IInterface): AnsiString;
    function Size: Integer;
    function Values: IJclAnsiStrCollection;
    property Items[const Key: IInterface]: AnsiString read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclAnsiStrAnsiStrMap = interface(IJclAnsiStrContainer)
    ['{A4788A96-281A-4924-AA24-03776DDAAD8A}']
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(const Value: AnsiString): Boolean;
    function Extract(const Key: AnsiString): AnsiString;
    function GetValue(const Key: AnsiString): AnsiString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: AnsiString): AnsiString;
    function KeySet: IJclAnsiStrSet;
    function MapEquals(const AMap: IJclAnsiStrAnsiStrMap): Boolean;
    procedure PutAll(const AMap: IJclAnsiStrAnsiStrMap);
    procedure PutValue(const Key: AnsiString; const Value: AnsiString);
    function Remove(const Key: AnsiString): AnsiString;
    function Size: Integer;
    function Values: IJclAnsiStrCollection;
    property Items[const Key: AnsiString]: AnsiString read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclWideStrIntfMap = interface(IJclWideStrContainer)
    ['{C959AB76-9CF0-4C2C-A2C6-8A1846563FAF}']
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: WideString): IInterface;
    function GetValue(const Key: WideString): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): WideString;
    function KeySet: IJclWideStrSet;
    function MapEquals(const AMap: IJclWideStrIntfMap): Boolean;
    procedure PutAll(const AMap: IJclWideStrIntfMap);
    procedure PutValue(const Key: WideString; const Value: IInterface);
    function Remove(const Key: WideString): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    property Items[const Key: WideString]: IInterface read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclIntfWideStrMap = interface(IJclWideStrContainer)
    ['{D9FD7887-B840-4636-8A8F-E586663E332C}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: WideString): Boolean;
    function Extract(const Key: IInterface): WideString;
    function GetValue(const Key: IInterface): WideString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: WideString): IInterface;
    function KeySet: IJclIntfSet;
    function MapEquals(const AMap: IJclIntfWideStrMap): Boolean;
    procedure PutAll(const AMap: IJclIntfWideStrMap);
    procedure PutValue(const Key: IInterface; const Value: WideString);
    function Remove(const Key: IInterface): WideString;
    function Size: Integer;
    function Values: IJclWideStrCollection;
    property Items[const Key: IInterface]: WideString read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclWideStrWideStrMap = interface(IJclWideStrContainer)
    ['{8E8D2735-C4FB-4F00-8802-B2102BCE3644}']
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(const Value: WideString): Boolean;
    function Extract(const Key: WideString): WideString;
    function GetValue(const Key: WideString): WideString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: WideString): WideString;
    function KeySet: IJclWideStrSet;
    function MapEquals(const AMap: IJclWideStrWideStrMap): Boolean;
    procedure PutAll(const AMap: IJclWideStrWideStrMap);
    procedure PutValue(const Key: WideString; const Value: WideString);
    function Remove(const Key: WideString): WideString;
    function Size: Integer;
    function Values: IJclWideStrCollection;
    property Items[const Key: WideString]: WideString read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrIntfMap = interface(IJclUnicodeStrContainer)
    ['{C83D4F5E-8E66-41E9-83F6-338B44F24BE6}']
    procedure Clear;
    function ContainsKey(const Key: UnicodeString): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: UnicodeString): IInterface;
    function GetValue(const Key: UnicodeString): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): UnicodeString;
    function KeySet: IJclUnicodeStrSet;
    function MapEquals(const AMap: IJclUnicodeStrIntfMap): Boolean;
    procedure PutAll(const AMap: IJclUnicodeStrIntfMap);
    procedure PutValue(const Key: UnicodeString; const Value: IInterface);
    function Remove(const Key: UnicodeString): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    property Items[const Key: UnicodeString]: IInterface read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclIntfUnicodeStrMap = interface(IJclUnicodeStrContainer)
    ['{40F8B873-B763-4A3C-8EC4-31DB3404BF73}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: UnicodeString): Boolean;
    function Extract(const Key: IInterface): UnicodeString;
    function GetValue(const Key: IInterface): UnicodeString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: UnicodeString): IInterface;
    function KeySet: IJclIntfSet;
    function MapEquals(const AMap: IJclIntfUnicodeStrMap): Boolean;
    procedure PutAll(const AMap: IJclIntfUnicodeStrMap);
    procedure PutValue(const Key: IInterface; const Value: UnicodeString);
    function Remove(const Key: IInterface): UnicodeString;
    function Size: Integer;
    function Values: IJclUnicodeStrCollection;
    property Items[const Key: IInterface]: UnicodeString read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrUnicodeStrMap = interface(IJclUnicodeStrContainer)
    ['{557E1CBD-06AC-41C2-BAED-253709CBD0AE}']
    procedure Clear;
    function ContainsKey(const Key: UnicodeString): Boolean;
    function ContainsValue(const Value: UnicodeString): Boolean;
    function Extract(const Key: UnicodeString): UnicodeString;
    function GetValue(const Key: UnicodeString): UnicodeString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: UnicodeString): UnicodeString;
    function KeySet: IJclUnicodeStrSet;
    function MapEquals(const AMap: IJclUnicodeStrUnicodeStrMap): Boolean;
    procedure PutAll(const AMap: IJclUnicodeStrUnicodeStrMap);
    procedure PutValue(const Key: UnicodeString; const Value: UnicodeString);
    function Remove(const Key: UnicodeString): UnicodeString;
    function Size: Integer;
    function Values: IJclUnicodeStrCollection;
    property Items[const Key: UnicodeString]: UnicodeString read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrIntfMap = IJclAnsiStrIntfMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrIntfMap = IJclWideStrIntfMap;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrIntfMap = IJclUnicodeStrIntfMap;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  IJclIntfStrMap = IJclIntfAnsiStrMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclIntfStrMap = IJclIntfWideStrMap;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclIntfStrMap = IJclIntfUnicodeStrMap;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrStrMap = IJclAnsiStrAnsiStrMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrStrMap = IJclWideStrWideStrMap;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrStrMap = IJclUnicodeStrUnicodeStrMap;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleIntfMap = interface(IJclSingleContainer)
    ['{5F5E9E8B-E648-450B-B6C0-0EC65CC2D0BA}']
    procedure Clear;
    function ContainsKey(const Key: Single): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: Single): IInterface;
    function GetValue(const Key: Single): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Single;
    function KeySet: IJclSingleSet;
    function MapEquals(const AMap: IJclSingleIntfMap): Boolean;
    procedure PutAll(const AMap: IJclSingleIntfMap);
    procedure PutValue(const Key: Single; const Value: IInterface);
    function Remove(const Key: Single): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    property Items[const Key: Single]: IInterface read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclIntfSingleMap = interface(IJclSingleContainer)
    ['{234D1618-FB0E-46F5-A70D-5106163A90F7}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Single): Boolean;
    function Extract(const Key: IInterface): Single;
    function GetValue(const Key: IInterface): Single;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Single): IInterface;
    function KeySet: IJclIntfSet;
    function MapEquals(const AMap: IJclIntfSingleMap): Boolean;
    procedure PutAll(const AMap: IJclIntfSingleMap);
    procedure PutValue(const Key: IInterface; const Value: Single);
    function Remove(const Key: IInterface): Single;
    function Size: Integer;
    function Values: IJclSingleCollection;
    property Items[const Key: IInterface]: Single read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclSingleSingleMap = interface(IJclSingleContainer)
    ['{AEB0008F-F3CF-4055-A7F3-A330D312F03F}']
    procedure Clear;
    function ContainsKey(const Key: Single): Boolean;
    function ContainsValue(const Value: Single): Boolean;
    function Extract(const Key: Single): Single;
    function GetValue(const Key: Single): Single;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Single): Single;
    function KeySet: IJclSingleSet;
    function MapEquals(const AMap: IJclSingleSingleMap): Boolean;
    procedure PutAll(const AMap: IJclSingleSingleMap);
    procedure PutValue(const Key: Single; const Value: Single);
    function Remove(const Key: Single): Single;
    function Size: Integer;
    function Values: IJclSingleCollection;
    property Items[const Key: Single]: Single read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclDoubleIntfMap = interface(IJclDoubleContainer)
    ['{08968FFB-36C6-4FBA-BC09-3DCA2B5D7A50}']
    procedure Clear;
    function ContainsKey(const Key: Double): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: Double): IInterface;
    function GetValue(const Key: Double): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Double;
    function KeySet: IJclDoubleSet;
    function MapEquals(const AMap: IJclDoubleIntfMap): Boolean;
    procedure PutAll(const AMap: IJclDoubleIntfMap);
    procedure PutValue(const Key: Double; const Value: IInterface);
    function Remove(const Key: Double): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    property Items[const Key: Double]: IInterface read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclIntfDoubleMap = interface(IJclDoubleContainer)
    ['{B23DAF6A-6DC5-4DDD-835C-CD4633DDA010}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Double): Boolean;
    function Extract(const Key: IInterface): Double;
    function GetValue(const Key: IInterface): Double;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Double): IInterface;
    function KeySet: IJclIntfSet;
    function MapEquals(const AMap: IJclIntfDoubleMap): Boolean;
    procedure PutAll(const AMap: IJclIntfDoubleMap);
    procedure PutValue(const Key: IInterface; const Value: Double);
    function Remove(const Key: IInterface): Double;
    function Size: Integer;
    function Values: IJclDoubleCollection;
    property Items[const Key: IInterface]: Double read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclDoubleDoubleMap = interface(IJclDoubleContainer)
    ['{329A03B8-0B6B-4FE3-87C5-4B63447A5FFD}']
    procedure Clear;
    function ContainsKey(const Key: Double): Boolean;
    function ContainsValue(const Value: Double): Boolean;
    function Extract(const Key: Double): Double;
    function GetValue(const Key: Double): Double;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Double): Double;
    function KeySet: IJclDoubleSet;
    function MapEquals(const AMap: IJclDoubleDoubleMap): Boolean;
    procedure PutAll(const AMap: IJclDoubleDoubleMap);
    procedure PutValue(const Key: Double; const Value: Double);
    function Remove(const Key: Double): Double;
    function Size: Integer;
    function Values: IJclDoubleCollection;
    property Items[const Key: Double]: Double read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclExtendedIntfMap = interface(IJclExtendedContainer)
    ['{7C0731E0-C9AB-4378-B1B0-8CE3DD60AD41}']
    procedure Clear;
    function ContainsKey(const Key: Extended): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: Extended): IInterface;
    function GetValue(const Key: Extended): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Extended;
    function KeySet: IJclExtendedSet;
    function MapEquals(const AMap: IJclExtendedIntfMap): Boolean;
    procedure PutAll(const AMap: IJclExtendedIntfMap);
    procedure PutValue(const Key: Extended; const Value: IInterface);
    function Remove(const Key: Extended): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    property Items[const Key: Extended]: IInterface read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclIntfExtendedMap = interface(IJclExtendedContainer)
    ['{479FCE5A-2D8A-44EE-96BC-E8DA3187DBD8}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Extended): Boolean;
    function Extract(const Key: IInterface): Extended;
    function GetValue(const Key: IInterface): Extended;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Extended): IInterface;
    function KeySet: IJclIntfSet;
    function MapEquals(const AMap: IJclIntfExtendedMap): Boolean;
    procedure PutAll(const AMap: IJclIntfExtendedMap);
    procedure PutValue(const Key: IInterface; const Value: Extended);
    function Remove(const Key: IInterface): Extended;
    function Size: Integer;
    function Values: IJclExtendedCollection;
    property Items[const Key: IInterface]: Extended read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclExtendedExtendedMap = interface(IJclExtendedContainer)
    ['{962C2B09-8CF5-44E8-A21A-4A7DAFB72A11}']
    procedure Clear;
    function ContainsKey(const Key: Extended): Boolean;
    function ContainsValue(const Value: Extended): Boolean;
    function Extract(const Key: Extended): Extended;
    function GetValue(const Key: Extended): Extended;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Extended): Extended;
    function KeySet: IJclExtendedSet;
    function MapEquals(const AMap: IJclExtendedExtendedMap): Boolean;
    procedure PutAll(const AMap: IJclExtendedExtendedMap);
    procedure PutValue(const Key: Extended; const Value: Extended);
    function Remove(const Key: Extended): Extended;
    function Size: Integer;
    function Values: IJclExtendedCollection;
    property Items[const Key: Extended]: Extended read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatIntfMap = IJclSingleIntfMap;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatIntfMap = IJclDoubleIntfMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatIntfMap = IJclExtendedIntfMap;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclIntfFloatMap = IJclIntfSingleMap;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclIntfFloatMap = IJclIntfDoubleMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclIntfFloatMap = IJclIntfExtendedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatFloatMap = IJclSingleSingleMap;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatFloatMap = IJclDoubleDoubleMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatFloatMap = IJclExtendedExtendedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerIntfMap = interface(IJclBaseContainer)
    ['{E535FE65-AC88-49D3-BEF2-FB30D92C2FA6}']
    procedure Clear;
    function ContainsKey(Key: Integer): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(Key: Integer): IInterface;
    function GetValue(Key: Integer): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Integer;
    function KeySet: IJclIntegerSet;
    function MapEquals(const AMap: IJclIntegerIntfMap): Boolean;
    procedure PutAll(const AMap: IJclIntegerIntfMap);
    procedure PutValue(Key: Integer; const Value: IInterface);
    function Remove(Key: Integer): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    property Items[Key: Integer]: IInterface read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclIntfIntegerMap = interface(IJclBaseContainer)
    ['{E01DA012-BEE0-4259-8E30-0A7A1A87BED0}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: Integer): Boolean;
    function Extract(const Key: IInterface): Integer;
    function GetValue(const Key: IInterface): Integer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Integer): IInterface;
    function KeySet: IJclIntfSet;
    function MapEquals(const AMap: IJclIntfIntegerMap): Boolean;
    procedure PutAll(const AMap: IJclIntfIntegerMap);
    procedure PutValue(const Key: IInterface; Value: Integer);
    function Remove(const Key: IInterface): Integer;
    function Size: Integer;
    function Values: IJclIntegerCollection;
    property Items[const Key: IInterface]: Integer read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclIntegerIntegerMap = interface(IJclBaseContainer)
    ['{23A46BC0-DF8D-4BD2-89D2-4DACF1EC73A1}']
    procedure Clear;
    function ContainsKey(Key: Integer): Boolean;
    function ContainsValue(Value: Integer): Boolean;
    function Extract(Key: Integer): Integer;
    function GetValue(Key: Integer): Integer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Integer): Integer;
    function KeySet: IJclIntegerSet;
    function MapEquals(const AMap: IJclIntegerIntegerMap): Boolean;
    procedure PutAll(const AMap: IJclIntegerIntegerMap);
    procedure PutValue(Key: Integer; Value: Integer);
    function Remove(Key: Integer): Integer;
    function Size: Integer;
    function Values: IJclIntegerCollection;
    property Items[Key: Integer]: Integer read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclCardinalIntfMap = interface(IJclBaseContainer)
    ['{80D39FB1-0D10-49CE-8AF3-1CD98A1D4F6C}']
    procedure Clear;
    function ContainsKey(Key: Cardinal): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(Key: Cardinal): IInterface;
    function GetValue(Key: Cardinal): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Cardinal;
    function KeySet: IJclCardinalSet;
    function MapEquals(const AMap: IJclCardinalIntfMap): Boolean;
    procedure PutAll(const AMap: IJclCardinalIntfMap);
    procedure PutValue(Key: Cardinal; const Value: IInterface);
    function Remove(Key: Cardinal): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    property Items[Key: Cardinal]: IInterface read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclIntfCardinalMap = interface(IJclBaseContainer)
    ['{E1A724AB-6BDA-45F0-AE21-5E7E789A751B}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: Cardinal): Boolean;
    function Extract(const Key: IInterface): Cardinal;
    function GetValue(const Key: IInterface): Cardinal;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Cardinal): IInterface;
    function KeySet: IJclIntfSet;
    function MapEquals(const AMap: IJclIntfCardinalMap): Boolean;
    procedure PutAll(const AMap: IJclIntfCardinalMap);
    procedure PutValue(const Key: IInterface; Value: Cardinal);
    function Remove(const Key: IInterface): Cardinal;
    function Size: Integer;
    function Values: IJclCardinalCollection;
    property Items[const Key: IInterface]: Cardinal read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclCardinalCardinalMap = interface(IJclBaseContainer)
    ['{1CD3F54C-F92F-4AF4-82B2-0829C08AA83B}']
    procedure Clear;
    function ContainsKey(Key: Cardinal): Boolean;
    function ContainsValue(Value: Cardinal): Boolean;
    function Extract(Key: Cardinal): Cardinal;
    function GetValue(Key: Cardinal): Cardinal;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Cardinal): Cardinal;
    function KeySet: IJclCardinalSet;
    function MapEquals(const AMap: IJclCardinalCardinalMap): Boolean;
    procedure PutAll(const AMap: IJclCardinalCardinalMap);
    procedure PutValue(Key: Cardinal; Value: Cardinal);
    function Remove(Key: Cardinal): Cardinal;
    function Size: Integer;
    function Values: IJclCardinalCollection;
    property Items[Key: Cardinal]: Cardinal read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclInt64IntfMap = interface(IJclBaseContainer)
    ['{B64FB2D1-8D45-4367-B950-98D3D05AC6A0}']
    procedure Clear;
    function ContainsKey(const Key: Int64): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(const Key: Int64): IInterface;
    function GetValue(const Key: Int64): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Int64;
    function KeySet: IJclInt64Set;
    function MapEquals(const AMap: IJclInt64IntfMap): Boolean;
    procedure PutAll(const AMap: IJclInt64IntfMap);
    procedure PutValue(const Key: Int64; const Value: IInterface);
    function Remove(const Key: Int64): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    property Items[const Key: Int64]: IInterface read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclIntfInt64Map = interface(IJclBaseContainer)
    ['{9886BEE3-D15B-45D2-A3FB-4D3A0ADEC8AC}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Int64): Boolean;
    function Extract(const Key: IInterface): Int64;
    function GetValue(const Key: IInterface): Int64;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Int64): IInterface;
    function KeySet: IJclIntfSet;
    function MapEquals(const AMap: IJclIntfInt64Map): Boolean;
    procedure PutAll(const AMap: IJclIntfInt64Map);
    procedure PutValue(const Key: IInterface; const Value: Int64);
    function Remove(const Key: IInterface): Int64;
    function Size: Integer;
    function Values: IJclInt64Collection;
    property Items[const Key: IInterface]: Int64 read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclInt64Int64Map = interface(IJclBaseContainer)
    ['{EF2A2726-408A-4984-9971-DDC1B6EFC9F5}']
    procedure Clear;
    function ContainsKey(const Key: Int64): Boolean;
    function ContainsValue(const Value: Int64): Boolean;
    function Extract(const Key: Int64): Int64;
    function GetValue(const Key: Int64): Int64;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Int64): Int64;
    function KeySet: IJclInt64Set;
    function MapEquals(const AMap: IJclInt64Int64Map): Boolean;
    procedure PutAll(const AMap: IJclInt64Int64Map);
    procedure PutValue(const Key: Int64; const Value: Int64);
    function Remove(const Key: Int64): Int64;
    function Size: Integer;
    function Values: IJclInt64Collection;
    property Items[const Key: Int64]: Int64 read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclPtrIntfMap = interface(IJclBaseContainer)
    ['{B7C48542-39A0-453F-8F03-8C8CFAB0DCCF}']
    procedure Clear;
    function ContainsKey(Key: Pointer): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Extract(Key: Pointer): IInterface;
    function GetValue(Key: Pointer): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Pointer;
    function KeySet: IJclPtrSet;
    function MapEquals(const AMap: IJclPtrIntfMap): Boolean;
    procedure PutAll(const AMap: IJclPtrIntfMap);
    procedure PutValue(Key: Pointer; const Value: IInterface);
    function Remove(Key: Pointer): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    property Items[Key: Pointer]: IInterface read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclIntfPtrMap = interface(IJclBaseContainer)
    ['{DA51D823-58DB-4D7C-9B8E-07E0FD560B57}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: Pointer): Boolean;
    function Extract(const Key: IInterface): Pointer;
    function GetValue(const Key: IInterface): Pointer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Pointer): IInterface;
    function KeySet: IJclIntfSet;
    function MapEquals(const AMap: IJclIntfPtrMap): Boolean;
    procedure PutAll(const AMap: IJclIntfPtrMap);
    procedure PutValue(const Key: IInterface; Value: Pointer);
    function Remove(const Key: IInterface): Pointer;
    function Size: Integer;
    function Values: IJclPtrCollection;
    property Items[const Key: IInterface]: Pointer read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclPtrPtrMap = interface(IJclBaseContainer)
    ['{1200CB0F-A766-443F-9030-5A804C11B798}']
    procedure Clear;
    function ContainsKey(Key: Pointer): Boolean;
    function ContainsValue(Value: Pointer): Boolean;
    function Extract(Key: Pointer): Pointer;
    function GetValue(Key: Pointer): Pointer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Pointer): Pointer;
    function KeySet: IJclPtrSet;
    function MapEquals(const AMap: IJclPtrPtrMap): Boolean;
    procedure PutAll(const AMap: IJclPtrPtrMap);
    procedure PutValue(Key: Pointer; Value: Pointer);
    function Remove(Key: Pointer): Pointer;
    function Size: Integer;
    function Values: IJclPtrCollection;
    property Items[Key: Pointer]: Pointer read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclIntfMap = interface(IJclBaseContainer)
    ['{C70570C6-EDDB-47B4-9003-C637B486731D}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: IInterface): TObject;
    function GetValue(const Key: IInterface): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): IInterface;
    function KeySet: IJclIntfSet;
    function MapEquals(const AMap: IJclIntfMap): Boolean;
    procedure PutAll(const AMap: IJclIntfMap);
    procedure PutValue(const Key: IInterface; Value: TObject);
    function Remove(const Key: IInterface): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    property Items[const Key: IInterface]: TObject read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclAnsiStrMap = interface(IJclAnsiStrContainer)
    ['{A7D0A882-6952-496D-A258-23D47DDCCBC4}']
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: AnsiString): TObject;
    function GetValue(const Key: AnsiString): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): AnsiString;
    function KeySet: IJclAnsiStrSet;
    function MapEquals(const AMap: IJclAnsiStrMap): Boolean;
    procedure PutAll(const AMap: IJclAnsiStrMap);
    procedure PutValue(const Key: AnsiString; Value: TObject);
    function Remove(const Key: AnsiString): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    property Items[const Key: AnsiString]: TObject read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclWideStrMap = interface(IJclWideStrContainer)
    ['{ACE8E6B4-5A56-4753-A2C6-BAE195A56B63}']
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: WideString): TObject;
    function GetValue(const Key: WideString): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): WideString;
    function KeySet: IJclWideStrSet;
    function MapEquals(const AMap: IJclWideStrMap): Boolean;
    procedure PutAll(const AMap: IJclWideStrMap);
    procedure PutValue(const Key: WideString; Value: TObject);
    function Remove(const Key: WideString): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    property Items[const Key: WideString]: TObject read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrMap = interface(IJclUnicodeStrContainer)
    ['{4328E033-9B92-40C6-873D-A6982CFC2B95}']
    procedure Clear;
    function ContainsKey(const Key: UnicodeString): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: UnicodeString): TObject;
    function GetValue(const Key: UnicodeString): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): UnicodeString;
    function KeySet: IJclUnicodeStrSet;
    function MapEquals(const AMap: IJclUnicodeStrMap): Boolean;
    procedure PutAll(const AMap: IJclUnicodeStrMap);
    procedure PutValue(const Key: UnicodeString; Value: TObject);
    function Remove(const Key: UnicodeString): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    property Items[const Key: UnicodeString]: TObject read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrMap = IJclAnsiStrMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrMap = IJclWideStrMap;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrMap = IJclUnicodeStrMap;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleMap = interface(IJclSingleContainer)
    ['{C501920A-F252-4F94-B142-1F05AE06C3D2}']
    procedure Clear;
    function ContainsKey(const Key: Single): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: Single): TObject;
    function GetValue(const Key: Single): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Single;
    function KeySet: IJclSingleSet;
    function MapEquals(const AMap: IJclSingleMap): Boolean;
    procedure PutAll(const AMap: IJclSingleMap);
    procedure PutValue(const Key: Single; Value: TObject);
    function Remove(const Key: Single): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    property Items[const Key: Single]: TObject read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclDoubleMap = interface(IJclDoubleContainer)
    ['{B1B994AC-49C9-418B-814B-43BAD706F355}']
    procedure Clear;
    function ContainsKey(const Key: Double): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: Double): TObject;
    function GetValue(const Key: Double): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Double;
    function KeySet: IJclDoubleSet;
    function MapEquals(const AMap: IJclDoubleMap): Boolean;
    procedure PutAll(const AMap: IJclDoubleMap);
    procedure PutValue(const Key: Double; Value: TObject);
    function Remove(const Key: Double): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    property Items[const Key: Double]: TObject read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclExtendedMap = interface(IJclExtendedContainer)
    ['{3BCC8C87-A186-45E8-9B37-0B8E85120434}']
    procedure Clear;
    function ContainsKey(const Key: Extended): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: Extended): TObject;
    function GetValue(const Key: Extended): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Extended;
    function KeySet: IJclExtendedSet;
    function MapEquals(const AMap: IJclExtendedMap): Boolean;
    procedure PutAll(const AMap: IJclExtendedMap);
    procedure PutValue(const Key: Extended; Value: TObject);
    function Remove(const Key: Extended): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    property Items[const Key: Extended]: TObject read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatMap = IJclSingleMap;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatMap = IJclDoubleMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatMap = IJclExtendedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerMap = interface(IJclBaseContainer)
    ['{D6FA5D64-A4AF-4419-9981-56BA79BF8770}']
    procedure Clear;
    function ContainsKey(Key: Integer): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(Key: Integer): TObject;
    function GetValue(Key: Integer): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Integer;
    function KeySet: IJclIntegerSet;
    function MapEquals(const AMap: IJclIntegerMap): Boolean;
    procedure PutAll(const AMap: IJclIntegerMap);
    procedure PutValue(Key: Integer; Value: TObject);
    function Remove(Key: Integer): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    property Items[Key: Integer]: TObject read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclCardinalMap = interface(IJclBaseContainer)
    ['{A2F92F4F-11CB-4DB2-932F-F10A14237126}']
    procedure Clear;
    function ContainsKey(Key: Cardinal): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(Key: Cardinal): TObject;
    function GetValue(Key: Cardinal): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Cardinal;
    function KeySet: IJclCardinalSet;
    function MapEquals(const AMap: IJclCardinalMap): Boolean;
    procedure PutAll(const AMap: IJclCardinalMap);
    procedure PutValue(Key: Cardinal; Value: TObject);
    function Remove(Key: Cardinal): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    property Items[Key: Cardinal]: TObject read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclInt64Map = interface(IJclBaseContainer)
    ['{4C720CE0-7A7C-41D5-BFC1-8D58A47E648F}']
    procedure Clear;
    function ContainsKey(const Key: Int64): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(const Key: Int64): TObject;
    function GetValue(const Key: Int64): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Int64;
    function KeySet: IJclInt64Set;
    function MapEquals(const AMap: IJclInt64Map): Boolean;
    procedure PutAll(const AMap: IJclInt64Map);
    procedure PutValue(const Key: Int64; Value: TObject);
    function Remove(const Key: Int64): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    property Items[const Key: Int64]: TObject read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclPtrMap = interface(IJclBaseContainer)
    ['{2FE029A9-026C-487D-8204-AD3A28BD2FA2}']
    procedure Clear;
    function ContainsKey(Key: Pointer): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(Key: Pointer): TObject;
    function GetValue(Key: Pointer): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Pointer;
    function KeySet: IJclPtrSet;
    function MapEquals(const AMap: IJclPtrMap): Boolean;
    procedure PutAll(const AMap: IJclPtrMap);
    procedure PutValue(Key: Pointer; Value: TObject);
    function Remove(Key: Pointer): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    property Items[Key: Pointer]: TObject read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  IJclMap = interface(IJclBaseContainer)
    ['{A7D0A882-6952-496D-A258-23D47DDCCBC4}']
    procedure Clear;
    function ContainsKey(Key: TObject): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Extract(Key: TObject): TObject;
    function GetValue(Key: TObject): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): TObject;
    function KeySet: IJclSet;
    function MapEquals(const AMap: IJclMap): Boolean;
    procedure PutAll(const AMap: IJclMap);
    procedure PutValue(Key: TObject; Value: TObject);
    function Remove(Key: TObject): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    property Items[Key: TObject]: TObject read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;



  (*IJclMultiIntfIntfMap = interface(IJclIntfIntfMap)
    ['{497775A5-D3F1-49FC-A641-15CC9E77F3D0}']
    function GetValues(const Key: IInterface): IJclIntfIterator;
    function Count(const Key: IInterface): Integer;
  end;*)

  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IHashable = interface
    function GetHashCode: Integer;
  end;

  IJclMap<TKey,TValue> = interface(IJclBaseContainer)
    ['{22624C43-4828-4A1E-BDD4-4A7FE59AE135}']
    procedure Clear;
    function ContainsKey(const Key: TKey): Boolean;
    function ContainsValue(const Value: TValue): Boolean;
    function Extract(const Key: TKey): TValue;
    function GetValue(const Key: TKey): TValue;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: TValue): TKey;
    function KeySet: IJclSet<TKey>;
    function MapEquals(const AMap: IJclMap<TKey,TValue>): Boolean;
    procedure PutAll(const AMap: IJclMap<TKey,TValue>);
    procedure PutValue(const Key: TKey; const Value: TValue);
    function Remove(const Key: TKey): TValue;
    function Size: Integer;
    function Values: IJclCollection<TValue>;
    property Items[const Key: TKey]: TValue read GetValue write PutValue;
      {$IFNDEF BUGGY_DEFAULT_INDEXED_PROP} default; {$ENDIF ~BUGGY_DEFAULT_INDEXED_PROP}
  end;

  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfQueue = interface(IJclIntfContainer)
    ['{B88756FE-5553-4106-957E-3E33120BFA99}']
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function Dequeue: IInterface;
    function Empty: Boolean;
    function Enqueue(const AInterface: IInterface): Boolean;
    function Peek: IInterface;
    function Size: Integer;
  end;

  IJclAnsiStrQueue = interface(IJclAnsiStrContainer)
    ['{5BA0ED9A-5AF3-4F79-9D80-34FA7FF15D1F}']
    procedure Clear;
    function Contains(const AString: AnsiString): Boolean;
    function Dequeue: AnsiString;
    function Empty: Boolean;
    function Enqueue(const AString: AnsiString): Boolean;
    function Peek: AnsiString;
    function Size: Integer;
  end;

  IJclWideStrQueue = interface(IJclWideStrContainer)
    ['{058BBFB7-E9B9-44B5-B676-D5B5B9A79BEF}']
    procedure Clear;
    function Contains(const AString: WideString): Boolean;
    function Dequeue: WideString;
    function Empty: Boolean;
    function Enqueue(const AString: WideString): Boolean;
    function Peek: WideString;
    function Size: Integer;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrQueue = interface(IJclUnicodeStrContainer)
    ['{94A09E52-424A-486E-846B-9C2C52DC3A8F}']
    procedure Clear;
    function Contains(const AString: UnicodeString): Boolean;
    function Dequeue: UnicodeString;
    function Empty: Boolean;
    function Enqueue(const AString: UnicodeString): Boolean;
    function Peek: UnicodeString;
    function Size: Integer;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrQueue = IJclAnsiStrQueue;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrQueue = IJclWideStrQueue;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrQueue = IJclUnicodeStrQueue;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleQueue = interface(IJclSingleContainer)
    ['{67D74314-9967-4C99-8A48-6E0ADD73EC29}']
    procedure Clear;
    function Contains(const AValue: Single): Boolean;
    function Dequeue: Single;
    function Empty: Boolean;
    function Enqueue(const AValue: Single): Boolean;
    function Peek: Single;
    function Size: Integer;
  end;

  IJclDoubleQueue = interface(IJclDoubleContainer)
    ['{FA1B6D25-3456-4963-87DC-5A2E53B2963F}']
    procedure Clear;
    function Contains(const AValue: Double): Boolean;
    function Dequeue: Double;
    function Empty: Boolean;
    function Enqueue(const AValue: Double): Boolean;
    function Peek: Double;
    function Size: Integer;
  end;

  IJclExtendedQueue = interface(IJclExtendedContainer)
    ['{76F349C0-7681-4BE8-9E94-280C962780D8}']
    procedure Clear;
    function Contains(const AValue: Extended): Boolean;
    function Dequeue: Extended;
    function Empty: Boolean;
    function Enqueue(const AValue: Extended): Boolean;
    function Peek: Extended;
    function Size: Integer;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatQueue = IJclSingleQueue;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatQueue = IJclDoubleQueue;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatQueue = IJclExtendedQueue;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerQueue = interface(IJclIntegerContainer)
    ['{4C4E174E-5D19-44CE-A248-B5589A9B68DF}']
    procedure Clear;
    function Contains(AValue: Integer): Boolean;
    function Dequeue: Integer;
    function Empty: Boolean;
    function Enqueue(AValue: Integer): Boolean;
    function Peek: Integer;
    function Size: Integer;
  end;

  IJclCardinalQueue = interface(IJclCardinalContainer)
    ['{CC1D4358-E259-4FB0-BA83-5180A0F8A6C0}']
    procedure Clear;
    function Contains(AValue: Cardinal): Boolean;
    function Dequeue: Cardinal;
    function Empty: Boolean;
    function Enqueue(AValue: Cardinal): Boolean;
    function Peek: Cardinal;
    function Size: Integer;
  end;

  IJclInt64Queue = interface(IJclInt64Container)
    ['{96B620BB-9A90-43D5-82A7-2D818A11C8E1}']
    procedure Clear;
    function Contains(const AValue: Int64): Boolean;
    function Dequeue: Int64;
    function Empty: Boolean;
    function Enqueue(const AValue: Int64): Boolean;
    function Peek: Int64;
    function Size: Integer;
  end;

  IJclPtrQueue = interface(IJclPtrContainer)
    ['{1052DD37-3035-4C44-A793-54AC4B9C0B29}']
    procedure Clear;
    function Contains(APtr: Pointer): Boolean;
    function Dequeue: Pointer;
    function Empty: Boolean;
    function Enqueue(APtr: Pointer): Boolean;
    function Peek: Pointer;
    function Size: Integer;
  end;

  IJclQueue = interface(IJclContainer)
    ['{7D0F9DE4-71EA-46EF-B879-88BCFD5D9610}']
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function Dequeue: TObject;
    function Empty: Boolean;
    function Enqueue(AObject: TObject): Boolean;
    function Peek: TObject;
    function Size: Integer;
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IJclQueue<T> = interface(IJclBaseContainer)
    ['{16AB909F-2194-46CF-BD89-B4207AC0CAB8}']
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function Dequeue: T;
    function Empty: Boolean;
    function Enqueue(const AItem: T): Boolean;
    function Peek: T;
    function Size: Integer;
  end;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfIntfSortedMap = interface(IJclIntfIntfMap)
    ['{265A6EB2-4BB3-459F-8813-360FD32A4971}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfIntfSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfIntfSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfIntfSortedMap;
  end;

  IJclAnsiStrIntfSortedMap = interface(IJclAnsiStrIntfMap)
    ['{706D1C91-5416-4FDC-B6B1-F4C1E8CFCD38}']
    function FirstKey: AnsiString;
    function HeadMap(const ToKey: AnsiString): IJclAnsiStrIntfSortedMap;
    function LastKey: AnsiString;
    function SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrIntfSortedMap;
    function TailMap(const FromKey: AnsiString): IJclAnsiStrIntfSortedMap;
  end;

  IJclIntfAnsiStrSortedMap = interface(IJclIntfAnsiStrMap)
    ['{96E6AC5E-8C40-4795-9C8A-CFD098B58680}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfAnsiStrSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfAnsiStrSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfAnsiStrSortedMap;
  end;

  IJclAnsiStrAnsiStrSortedMap = interface(IJclAnsiStrAnsiStrMap)
    ['{4F457799-5D03-413D-A46C-067DC4200CC3}']
    function FirstKey: AnsiString;
    function HeadMap(const ToKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
    function LastKey: AnsiString;
    function SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
    function TailMap(const FromKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
  end;

  IJclWideStrIntfSortedMap = interface(IJclWideStrIntfMap)
    ['{299FDCFD-2DB7-4D64-BF18-EE3668316430}']
    function FirstKey: WideString;
    function HeadMap(const ToKey: WideString): IJclWideStrIntfSortedMap;
    function LastKey: WideString;
    function SubMap(const FromKey, ToKey: WideString): IJclWideStrIntfSortedMap;
    function TailMap(const FromKey: WideString): IJclWideStrIntfSortedMap;
  end;

  IJclIntfWideStrSortedMap = interface(IJclIntfWideStrMap)
    ['{FBE3AD2E-2781-4DC0-9E80-027027380E21}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfWideStrSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfWideStrSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfWideStrSortedMap;
  end;

  IJclWideStrWideStrSortedMap = interface(IJclWideStrWideStrMap)
    ['{3B0757B2-2290-4AFA-880D-F9BA600E501E}']
    function FirstKey: WideString;
    function HeadMap(const ToKey: WideString): IJclWideStrWideStrSortedMap;
    function LastKey: WideString;
    function SubMap(const FromKey, ToKey: WideString): IJclWideStrWideStrSortedMap;
    function TailMap(const FromKey: WideString): IJclWideStrWideStrSortedMap;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrIntfSortedMap = interface(IJclUnicodeStrIntfMap)
    ['{25FDE916-730D-449A-BA29-852D8A0470B6}']
    function FirstKey: UnicodeString;
    function HeadMap(const ToKey: UnicodeString): IJclUnicodeStrIntfSortedMap;
    function LastKey: UnicodeString;
    function SubMap(const FromKey, ToKey: UnicodeString): IJclUnicodeStrIntfSortedMap;
    function TailMap(const FromKey: UnicodeString): IJclUnicodeStrIntfSortedMap;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclIntfUnicodeStrSortedMap = interface(IJclIntfUnicodeStrMap)
    ['{B0B0CB9B-268B-40D2-94A8-0B8B5BE2E1AC}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfUnicodeStrSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfUnicodeStrSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfUnicodeStrSortedMap;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrUnicodeStrSortedMap = interface(IJclUnicodeStrUnicodeStrMap)
    ['{D8EACC5D-B31E-47A8-9CC9-32B15A79CACA}']
    function FirstKey: UnicodeString;
    function HeadMap(const ToKey: UnicodeString): IJclUnicodeStrUnicodeStrSortedMap;
    function LastKey: UnicodeString;
    function SubMap(const FromKey, ToKey: UnicodeString): IJclUnicodeStrUnicodeStrSortedMap;
    function TailMap(const FromKey: UnicodeString): IJclUnicodeStrUnicodeStrSortedMap;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrIntfSortedMap = IJclAnsiStrIntfSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrIntfSortedMap = IJclWideStrIntfSortedMap;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrIntfSortedMap = IJclUnicodeStrIntfSortedMap;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  IJclIntfStrSortedMap = IJclIntfAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclIntfStrSortedMap = IJclIntfWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclIntfStrSortedMap = IJclIntfUnicodeStrSortedMap;
  {$ENDIF CONTAINER_UNICODESTR}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrStrSortedMap = IJclAnsiStrAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrStrSortedMap = IJclWideStrWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrStrSortedMap = IJclUnicodeStrUnicodeStrSortedMap;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleIntfSortedMap = interface(IJclSingleIntfMap)
    ['{83D57068-7B8E-453E-B35B-2AB4B594A7A9}']
    function FirstKey: Single;
    function HeadMap(const ToKey: Single): IJclSingleIntfSortedMap;
    function LastKey: Single;
    function SubMap(const FromKey, ToKey: Single): IJclSingleIntfSortedMap;
    function TailMap(const FromKey: Single): IJclSingleIntfSortedMap;
  end;

  IJclIntfSingleSortedMap = interface(IJclIntfSingleMap)
    ['{B07FA192-3466-4F2A-BBF0-2DC0100B08A8}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfSingleSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfSingleSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfSingleSortedMap;
  end;

  IJclSingleSingleSortedMap = interface(IJclSingleSingleMap)
    ['{7C6EA0B4-959D-44D5-915F-99DFC1753B00}']
    function FirstKey: Single;
    function HeadMap(const ToKey: Single): IJclSingleSingleSortedMap;
    function LastKey: Single;
    function SubMap(const FromKey, ToKey: Single): IJclSingleSingleSortedMap;
    function TailMap(const FromKey: Single): IJclSingleSingleSortedMap;
  end;

  IJclDoubleIntfSortedMap = interface(IJclDoubleIntfMap)
    ['{F36C5F4F-4F8C-4943-AA35-41623D3C21E9}']
    function FirstKey: Double;
    function HeadMap(const ToKey: Double): IJclDoubleIntfSortedMap;
    function LastKey: Double;
    function SubMap(const FromKey, ToKey: Double): IJclDoubleIntfSortedMap;
    function TailMap(const FromKey: Double): IJclDoubleIntfSortedMap;
  end;

  IJclIntfDoubleSortedMap = interface(IJclIntfDoubleMap)
    ['{0F16ADAE-F499-4857-B5EA-6F3CC9009DBA}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfDoubleSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfDoubleSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfDoubleSortedMap;
  end;

  IJclDoubleDoubleSortedMap = interface(IJclDoubleDoubleMap)
    ['{855C858B-74CF-4338-872B-AF88A02DB537}']
    function FirstKey: Double;
    function HeadMap(const ToKey: Double): IJclDoubleDoubleSortedMap;
    function LastKey: Double;
    function SubMap(const FromKey, ToKey: Double): IJclDoubleDoubleSortedMap;
    function TailMap(const FromKey: Double): IJclDoubleDoubleSortedMap;
  end;

  IJclExtendedIntfSortedMap = interface(IJclExtendedIntfMap)
    ['{A30B8835-A319-4776-9A11-D1EEF60B9C26}']
    function FirstKey: Extended;
    function HeadMap(const ToKey: Extended): IJclExtendedIntfSortedMap;
    function LastKey: Extended;
    function SubMap(const FromKey, ToKey: Extended): IJclExtendedIntfSortedMap;
    function TailMap(const FromKey: Extended): IJclExtendedIntfSortedMap;
  end;

  IJclIntfExtendedSortedMap = interface(IJclIntfExtendedMap)
    ['{3493D6C4-3075-48B6-8E99-CB0000D3978C}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfExtendedSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfExtendedSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfExtendedSortedMap;
  end;

  IJclExtendedExtendedSortedMap = interface(IJclExtendedExtendedMap)
    ['{8CAA505C-D9BB-47E7-92EC-6043DC4AF42C}']
    function FirstKey: Extended;
    function HeadMap(const ToKey: Extended): IJclExtendedExtendedSortedMap;
    function LastKey: Extended;
    function SubMap(const FromKey, ToKey: Extended): IJclExtendedExtendedSortedMap;
    function TailMap(const FromKey: Extended): IJclExtendedExtendedSortedMap;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatIntfSortedMap = IJclSingleIntfSortedMap;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatIntfSortedMap = IJclDoubleIntfSortedMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatIntfSortedMap = IJclExtendedIntfSortedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclIntfFloatSortedMap = IJclIntfSingleSortedMap;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclIntfFloatSortedMap = IJclIntfDoubleSortedMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclIntfFloatSortedMap = IJclIntfExtendedSortedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatFloatSortedMap = IJclSingleSingleSortedMap;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatFloatSortedMap = IJclDoubleDoubleSortedMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatFloatSortedMap = IJclExtendedExtendedSortedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerIntfSortedMap = interface(IJclIntegerIntfMap)
    ['{8B22802C-61F2-4DA5-B1E9-DBB7840E7996}']
    function FirstKey: Integer;
    function HeadMap(ToKey: Integer): IJclIntegerIntfSortedMap;
    function LastKey: Integer;
    function SubMap(FromKey, ToKey: Integer): IJclIntegerIntfSortedMap;
    function TailMap(FromKey: Integer): IJclIntegerIntfSortedMap;
  end;

  IJclIntfIntegerSortedMap = interface(IJclIntfIntegerMap)
    ['{8D3C9B7E-772D-409B-A58C-0CABFAFDEFF0}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfIntegerSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfIntegerSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfIntegerSortedMap;
  end;

  IJclIntegerIntegerSortedMap = interface(IJclIntegerIntegerMap)
    ['{8A8BA17A-F468-469C-AF99-77D64C802F7A}']
    function FirstKey: Integer;
    function HeadMap(ToKey: Integer): IJclIntegerIntegerSortedMap;
    function LastKey: Integer;
    function SubMap(FromKey, ToKey: Integer): IJclIntegerIntegerSortedMap;
    function TailMap(FromKey: Integer): IJclIntegerIntegerSortedMap;
  end;

  IJclCardinalIntfSortedMap = interface(IJclCardinalIntfMap)
    ['{BAE97425-4F2E-461B-88DD-F83D27657AFA}']
    function FirstKey: Cardinal;
    function HeadMap(ToKey: Cardinal): IJclCardinalIntfSortedMap;
    function LastKey: Cardinal;
    function SubMap(FromKey, ToKey: Cardinal): IJclCardinalIntfSortedMap;
    function TailMap(FromKey: Cardinal): IJclCardinalIntfSortedMap;
  end;

  IJclIntfCardinalSortedMap = interface(IJclIntfCardinalMap)
    ['{BC66BACF-23AE-48C4-9573-EDC3B5110BE7}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfCardinalSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfCardinalSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfCardinalSortedMap;
  end;

  IJclCardinalCardinalSortedMap = interface(IJclCardinalCardinalMap)
    ['{182ACDA4-7D74-4D29-BB5C-4C8189DA774E}']
    function FirstKey: Cardinal;
    function HeadMap(ToKey: Cardinal): IJclCardinalCardinalSortedMap;
    function LastKey: Cardinal;
    function SubMap(FromKey, ToKey: Cardinal): IJclCardinalCardinalSortedMap;
    function TailMap(FromKey: Cardinal): IJclCardinalCardinalSortedMap;
  end;

  IJclInt64IntfSortedMap = interface(IJclInt64IntfMap)
    ['{24391756-FB02-4901-81E3-A37738B73DAD}']
    function FirstKey: Int64;
    function HeadMap(const ToKey: Int64): IJclInt64IntfSortedMap;
    function LastKey: Int64;
    function SubMap(const FromKey, ToKey: Int64): IJclInt64IntfSortedMap;
    function TailMap(const FromKey: Int64): IJclInt64IntfSortedMap;
  end;

  IJclIntfInt64SortedMap = interface(IJclIntfInt64Map)
    ['{6E2AB647-59CC-4609-82E8-6AE75AED80CA}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfInt64SortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfInt64SortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfInt64SortedMap;
  end;

  IJclInt64Int64SortedMap = interface(IJclInt64Int64Map)
    ['{168581D2-9DD3-46D0-934E-EA0CCE5E3C0C}']
    function FirstKey: Int64;
    function HeadMap(const ToKey: Int64): IJclInt64Int64SortedMap;
    function LastKey: Int64;
    function SubMap(const FromKey, ToKey: Int64): IJclInt64Int64SortedMap;
    function TailMap(const FromKey: Int64): IJclInt64Int64SortedMap;
  end;

  IJclPtrIntfSortedMap = interface(IJclPtrIntfMap)
    ['{6D7B8042-3CBC-4C8F-98B5-69AFAA104532}']
    function FirstKey: Pointer;
    function HeadMap(ToKey: Pointer): IJclPtrIntfSortedMap;
    function LastKey: Pointer;
    function SubMap(FromKey, ToKey: Pointer): IJclPtrIntfSortedMap;
    function TailMap(FromKey: Pointer): IJclPtrIntfSortedMap;
  end;

  IJclIntfPtrSortedMap = interface(IJclIntfPtrMap)
    ['{B054BDA2-536F-4C16-B6BB-BB64FA0818B3}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfPtrSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfPtrSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfPtrSortedMap;
  end;

  IJclPtrPtrSortedMap = interface(IJclPtrPtrMap)
    ['{F1FAE922-0212-41D0-BB4E-76A8AB2CAB86}']
    function FirstKey: Pointer;
    function HeadMap(ToKey: Pointer): IJclPtrPtrSortedMap;
    function LastKey: Pointer;
    function SubMap(FromKey, ToKey: Pointer): IJclPtrPtrSortedMap;
    function TailMap(FromKey: Pointer): IJclPtrPtrSortedMap;
  end;

  IJclIntfSortedMap = interface(IJclIntfMap)
    ['{3CED1477-B958-4109-9BDA-7C84B9E063B2}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfSortedMap;
  end;

  IJclAnsiStrSortedMap = interface(IJclAnsiStrMap)
    ['{573F98E3-EBCD-4F28-8F35-96A7366CBF47}']
    function FirstKey: AnsiString;
    function HeadMap(const ToKey: AnsiString): IJclAnsiStrSortedMap;
    function LastKey: AnsiString;
    function SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrSortedMap;
    function TailMap(const FromKey: AnsiString): IJclAnsiStrSortedMap;
  end;

  IJclWideStrSortedMap = interface(IJclWideStrMap)
    ['{B3021EFC-DE25-4B4B-A896-ACE823CD5C01}']
    function FirstKey: WideString;
    function HeadMap(const ToKey: WideString): IJclWideStrSortedMap;
    function LastKey: WideString;
    function SubMap(const FromKey, ToKey: WideString): IJclWideStrSortedMap;
    function TailMap(const FromKey: WideString): IJclWideStrSortedMap;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrSortedMap = interface(IJclUnicodeStrMap)
    ['{5510B8FC-3439-4211-8D1F-5EDD9A56D3E3}']
    function FirstKey: UnicodeString;
    function HeadMap(const ToKey: UnicodeString): IJclUnicodeStrSortedMap;
    function LastKey: UnicodeString;
    function SubMap(const FromKey, ToKey: UnicodeString): IJclUnicodeStrSortedMap;
    function TailMap(const FromKey: UnicodeString): IJclUnicodeStrSortedMap;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrSortedMap = IJclAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrSortedMap = IJclWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrSortedMap = IJclUnicodeStrSortedMap;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleSortedMap = interface(IJclSingleMap)
    ['{8C1A12BE-A7F2-4351-90B7-25DB0AAF5F94}']
    function FirstKey: Single;
    function HeadMap(const ToKey: Single): IJclSingleSortedMap;
    function LastKey: Single;
    function SubMap(const FromKey, ToKey: Single): IJclSingleSortedMap;
    function TailMap(const FromKey: Single): IJclSingleSortedMap;
  end;

  IJclDoubleSortedMap = interface(IJclDoubleMap)
    ['{8018D66B-AA54-4016-84FC-3E780FFCC38B}']
    function FirstKey: Double;
    function HeadMap(const ToKey: Double): IJclDoubleSortedMap;
    function LastKey: Double;
    function SubMap(const FromKey, ToKey: Double): IJclDoubleSortedMap;
    function TailMap(const FromKey: Double): IJclDoubleSortedMap;
  end;

  IJclExtendedSortedMap = interface(IJclExtendedMap)
    ['{2B82C65A-B3EF-477D-BEC0-3D8620A226B1}']
    function FirstKey: Extended;
    function HeadMap(const ToKey: Extended): IJclExtendedSortedMap;
    function LastKey: Extended;
    function SubMap(const FromKey, ToKey: Extended): IJclExtendedSortedMap;
    function TailMap(const FromKey: Extended): IJclExtendedSortedMap;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatSortedMap = IJclSingleSortedMap;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatSortedMap = IJclDoubleSortedMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatSortedMap = IJclExtendedSortedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerSortedMap = interface(IJclIntegerMap)
    ['{DD7B4C5E-6D51-44CC-9328-B38396A7E1C9}']
    function FirstKey: Integer;
    function HeadMap(ToKey: Integer): IJclIntegerSortedMap;
    function LastKey: Integer;
    function SubMap(FromKey, ToKey: Integer): IJclIntegerSortedMap;
    function TailMap(FromKey: Integer): IJclIntegerSortedMap;
  end;

  IJclCardinalSortedMap = interface(IJclCardinalMap)
    ['{4AEAF81F-D72E-4499-B10E-3D017F39915E}']
    function FirstKey: Cardinal;
    function HeadMap(ToKey: Cardinal): IJclCardinalSortedMap;
    function LastKey: Cardinal;
    function SubMap(FromKey, ToKey: Cardinal): IJclCardinalSortedMap;
    function TailMap(FromKey: Cardinal): IJclCardinalSortedMap;
  end;

  IJclInt64SortedMap = interface(IJclInt64Map)
    ['{06C03F90-7DE9-4043-AA56-AAE071D8BD50}']
    function FirstKey: Int64;
    function HeadMap(const ToKey: Int64): IJclInt64SortedMap;
    function LastKey: Int64;
    function SubMap(const FromKey, ToKey: Int64): IJclInt64SortedMap;
    function TailMap(const FromKey: Int64): IJclInt64SortedMap;
  end;

  IJclPtrSortedMap = interface(IJclPtrMap)
    ['{578918DB-6A4A-4A9D-B44E-AE3E8FF70818}']
    function FirstKey: Pointer;
    function HeadMap(ToKey: Pointer): IJclPtrSortedMap;
    function LastKey: Pointer;
    function SubMap(FromKey, ToKey: Pointer): IJclPtrSortedMap;
    function TailMap(FromKey: Pointer): IJclPtrSortedMap;
  end;

  IJclSortedMap = interface(IJclMap)
    ['{F317A70F-7851-49C2-9DCF-092D8F4D4F98}']
    function FirstKey: TObject;
    function HeadMap(ToKey: TObject): IJclSortedMap;
    function LastKey: TObject;
    function SubMap(FromKey, ToKey: TObject): IJclSortedMap;
    function TailMap(FromKey: TObject): IJclSortedMap;
  end;



  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IJclSortedMap<TKey,TValue> = interface(IJclMap<TKey,TValue>)
    ['{C62B75C4-891B-442E-A5D6-9954E75A5C0C}']
    function FirstKey: TKey;
    function HeadMap(const ToKey: TKey): IJclSortedMap<TKey,TValue>;
    function LastKey: TKey;
    function SubMap(const FromKey, ToKey: TKey): IJclSortedMap<TKey,TValue>;
    function TailMap(const FromKey: TKey): IJclSortedMap<TKey,TValue>;
  end;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfSortedSet = interface(IJclIntfSet)
    ['{159BE5A7-7349-42FF-BE55-9CA1B9DBA991}']
    function HeadSet(const Finish: IInterface): IJclIntfSortedSet;
    function SubSet(const Start, Finish: IInterface): IJclIntfSortedSet;
    function TailSet(const Start: IInterface): IJclIntfSortedSet;
  end;

  IJclAnsiStrSortedSet = interface(IJclAnsiStrSet)
    ['{03198146-F967-4310-868B-7AD3D52D5CBE}']
    function HeadSet(const Finish: AnsiString): IJclAnsiStrSortedSet;
    function SubSet(const Start, Finish: AnsiString): IJclAnsiStrSortedSet;
    function TailSet(const Start: AnsiString): IJclAnsiStrSortedSet;
  end;

  IJclWideStrSortedSet = interface(IJclWideStrSet)
    ['{ED9567E2-C1D3-4C00-A1D4-90D5C7E27C2D}']
    function HeadSet(const Finish: WideString): IJclWideStrSortedSet;
    function SubSet(const Start, Finish: WideString): IJclWideStrSortedSet;
    function TailSet(const Start: WideString): IJclWideStrSortedSet;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrSortedSet = interface(IJclUnicodeStrSet)
    ['{172BCD6F-D23C-4014-9C8C-A77A27D6E881}']
    function HeadSet(const Finish: UnicodeString): IJclUnicodeStrSortedSet;
    function SubSet(const Start, Finish: UnicodeString): IJclUnicodeStrSortedSet;
    function TailSet(const Start: UnicodeString): IJclUnicodeStrSortedSet;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrSortedSet = IJclAnsiStrSortedSet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrSortedSet = IJclWideStrSortedSet;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrSortedSet = IJclUnicodeStrSortedSet;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleSortedSet = interface(IJclSingleSet)
    ['{65EDA801-9E04-4119-BF9E-D7DD4AF82144}']
    function HeadSet(const Finish: Single): IJclSingleSortedSet;
    function SubSet(const Start, Finish: Single): IJclSingleSortedSet;
    function TailSet(const Start: Single): IJclSingleSortedSet;
  end;

  IJclDoubleSortedSet = interface(IJclDoubleSet)
    ['{DA0E689F-BAFE-4BCE-85E4-C38E780BC84C}']
    function HeadSet(const Finish: Double): IJclDoubleSortedSet;
    function SubSet(const Start, Finish: Double): IJclDoubleSortedSet;
    function TailSet(const Start: Double): IJclDoubleSortedSet;
  end;

  IJclExtendedSortedSet = interface(IJclExtendedSet)
    ['{A9875ED3-81A4-43A3-86BB-3429F51B278B}']
    function HeadSet(const Finish: Extended): IJclExtendedSortedSet;
    function SubSet(const Start, Finish: Extended): IJclExtendedSortedSet;
    function TailSet(const Start: Extended): IJclExtendedSortedSet;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatSortedSet = IJclSingleSortedSet;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatSortedSet = IJclDoubleSortedSet;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatSortedSet = IJclExtendedSortedSet;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerSortedSet = interface(IJclIntegerSet)
    ['{E086C54B-4FA3-426D-AC4E-FF8E8CA3D663}']
    function HeadSet(Finish: Integer): IJclIntegerSortedSet;
    function SubSet(Start, Finish: Integer): IJclIntegerSortedSet;
    function TailSet(Start: Integer): IJclIntegerSortedSet;
  end;

  IJclCardinalSortedSet = interface(IJclCardinalSet)
    ['{2D7995C6-A784-48B6-87E9-55D394A72362}']
    function HeadSet(Finish: Cardinal): IJclCardinalSortedSet;
    function SubSet(Start, Finish: Cardinal): IJclCardinalSortedSet;
    function TailSet(Start: Cardinal): IJclCardinalSortedSet;
  end;

  IJclInt64SortedSet = interface(IJclInt64Set)
    ['{4C1C3FCA-6169-4A2F-B044-91AC2AA2E954}']
    function HeadSet(const Finish: Int64): IJclInt64SortedSet;
    function SubSet(const Start, Finish: Int64): IJclInt64SortedSet;
    function TailSet(const Start: Int64): IJclInt64SortedSet;
  end;

  IJclPtrSortedSet = interface(IJclPtrSet)
    ['{F3A3183C-0820-425C-9446-E0838F0ADAD8}']
    function HeadSet(Finish: Pointer): IJclPtrSortedSet;
    function SubSet(Start, Finish: Pointer): IJclPtrSortedSet;
    function TailSet(Start: Pointer): IJclPtrSortedSet;
  end;

  IJclSortedSet = interface(IJclSet)
    ['{A3D23E76-ADE9-446C-9B97-F49FCE895D9F}']
    function HeadSet(Finish: TObject): IJclSortedSet;
    function SubSet(Start, Finish: TObject): IJclSortedSet;
    function TailSet(Start: TObject): IJclSortedSet;
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IJclSortedSet<T> = interface(IJclSet<T>)
    ['{30F836E3-2FB1-427E-A499-DFAE201633C8}']
    function HeadSet(const Finish: T): IJclSortedSet<T>;
    function SubSet(const Start, Finish: T): IJclSortedSet<T>;
    function TailSet(const Start: T): IJclSortedSet<T>;
  end;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfStack = interface(IJclIntfContainer)
    ['{CA1DC7A1-8D8F-4A5D-81D1-0FE32E9A4E84}']
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function Empty: Boolean;
    function Peek: IInterface;
    function Pop: IInterface;
    function Push(const AInterface: IInterface): Boolean;
    function Size: Integer;
  end;

  IJclAnsiStrStack = interface(IJclAnsiStrContainer)
    ['{649BB74C-D7BE-40D9-9F4E-32DDC3F13F3B}']
    procedure Clear;
    function Contains(const AString: AnsiString): Boolean;
    function Empty: Boolean;
    function Peek: AnsiString;
    function Pop: AnsiString;
    function Push(const AString: AnsiString): Boolean;
    function Size: Integer;
  end;

  IJclWideStrStack = interface(IJclWideStrContainer)
    ['{B2C3B165-33F1-4B7D-A2EC-0B19D12CE33C}']
    procedure Clear;
    function Contains(const AString: WideString): Boolean;
    function Empty: Boolean;
    function Peek: WideString;
    function Pop: WideString;
    function Push(const AString: WideString): Boolean;
    function Size: Integer;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  IJclUnicodeStrStack = interface(IJclUnicodeStrContainer)
    ['{BC046C3D-E3D2-42BA-A96D-054834A70404}']
    procedure Clear;
    function Contains(const AString: UnicodeString): Boolean;
    function Empty: Boolean;
    function Peek: UnicodeString;
    function Pop: UnicodeString;
    function Push(const AString: UnicodeString): Boolean;
    function Size: Integer;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrStack = IJclAnsiStrStack;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrStack = IJclWideStrStack;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  IJclStrStack = IJclUnicodeStrStack;
  {$ENDIF CONTAINER_UNICODESTR}

  IJclSingleStack = interface(IJclSingleContainer)
    ['{8DCE45C8-B5B3-43AB-BA08-DAD531CEB9CF}']
    procedure Clear;
    function Contains(const AValue: Single): Boolean;
    function Empty: Boolean;
    function Peek: Single;
    function Pop: Single;
    function Push(const AValue: Single): Boolean;
    function Size: Integer;
  end;

  IJclDoubleStack = interface(IJclDoubleContainer)
    ['{46DF2701-16F0-453C-B938-F04E9C1CEBF8}']
    procedure Clear;
    function Contains(const AValue: Double): Boolean;
    function Empty: Boolean;
    function Peek: Double;
    function Pop: Double;
    function Push(const AValue: Double): Boolean;
    function Size: Integer;
  end;

  IJclExtendedStack = interface(IJclExtendedContainer)
    ['{A2A30585-F561-4757-ABE1-CA511AE72CC5}']
    procedure Clear;
    function Contains(const AValue: Extended): Boolean;
    function Empty: Boolean;
    function Peek: Extended;
    function Pop: Extended;
    function Push(const AValue: Extended): Boolean;
    function Size: Integer;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatStack = IJclSingleStack;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatStack = IJclDoubleStack;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatStack = IJclExtendedStack;
  {$ENDIF MATH_EXTENDED_PRECISION}

  IJclIntegerStack = interface(IJclIntegerContainer)
    ['{9190BF0E-5B0C-4D6C-A107-20A933C9B56A}']
    procedure Clear;
    function Contains(AValue: Integer): Boolean;
    function Empty: Boolean;
    function Peek: Integer;
    function Pop: Integer;
    function Push(AValue: Integer): Boolean;
    function Size: Integer;
  end;

  IJclCardinalStack = interface(IJclCardinalContainer)
    ['{94F9EDB3-602B-49CE-9990-0AFDAC556F83}']
    procedure Clear;
    function Contains(AValue: Cardinal): Boolean;
    function Empty: Boolean;
    function Peek: Cardinal;
    function Pop: Cardinal;
    function Push(AValue: Cardinal): Boolean;
    function Size: Integer;
  end;

  IJclInt64Stack = interface(IJclInt64Container)
    ['{D689EB8F-2746-40E9-AD1B-7E656475FC64}']
    procedure Clear;
    function Contains(const AValue: Int64): Boolean;
    function Empty: Boolean;
    function Peek: Int64;
    function Pop: Int64;
    function Push(const AValue: Int64): Boolean;
    function Size: Integer;
  end;

  IJclPtrStack = interface(IJclPtrContainer)
    ['{AD11D06C-E0E1-4EDE-AA2F-BC8BDD972B73}']
    procedure Clear;
    function Contains(APtr: Pointer): Boolean;
    function Empty: Boolean;
    function Peek: Pointer;
    function Pop: Pointer;
    function Push(APtr: Pointer): Boolean;
    function Size: Integer;
  end;

  IJclStack = interface(IJclContainer)
    ['{E07E0BD8-A831-41B9-B9A0-7199BD4873B9}']
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function Empty: Boolean;
    function Peek: TObject;
    function Pop: TObject;
    function Push(AObject: TObject): Boolean;
    function Size: Integer;
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  IJclStack<T> = interface(IJclBaseContainer)
    ['{2F08EAC9-270D-496E-BE10-5E975918A5F2}']
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function Empty: Boolean;
    function Peek: T;
    function Pop: T;
    function Push(const AItem: T): Boolean;
    function Size: Integer;
  end;
  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

  // Exceptions
  EJclContainerError = class(EJclError);

  EJclOutOfBoundsError = class(EJclContainerError)
  public
    // RsEOutOfBounds
    constructor Create;
  end;

  EJclNoSuchElementError = class(EJclContainerError)
  public
    // RsEValueNotFound
    constructor Create(const Value: string);
  end;

  EJclDuplicateElementError = class(EJclContainerError)
  public
    // RsEDuplicateElement
    constructor Create;
  end;

  EJclIllegalArgumentError = class(EJclContainerError)
  end;

  EJclNoCollectionError = class(EJclIllegalArgumentError)
  public
    // RsENoCollection
    constructor Create;
  end;

  EJclIllegalQueueCapacityError = class(EJclIllegalArgumentError)
  public
    // RsEIllegalQueueCapacity
    constructor Create;
  end;

  EJclOperationNotSupportedError = class(EJclContainerError)
  public
    // RsEOperationNotSupported
    constructor Create;
  end;

  EJclNoEqualityComparerError = class(EJclContainerError)
  public
    // RsENoEqualityComparer
    constructor Create;
  end;

  EJclNoComparerError = class(EJclContainerError)
  public
    // RsENoComparer
    constructor Create;
  end;

  EJclNoHashConverterError = class(EJclContainerError)
  public
    // RsENoHashConverter
    constructor Create;
  end;

  EJclIllegalStateOperationError = class(EJclContainerError)
  public
    // RsEIllegalStateOperation
    constructor Create;
  end;

  EJclAssignError = class(EJclContainerError)
  public
    // RsEAssignError
    constructor Create;
  end;

  EJclReadOnlyError = class(EJclContainerError)
  public
    // RsEReadOnlyError
    constructor Create;
  end;

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

//=== { EJclOutOfBoundsError } ===============================================

constructor EJclOutOfBoundsError.Create;
begin
  inherited CreateRes(@RsEOutOfBounds);
end;

//=== { EJclNoSuchElementError } =============================================

constructor EJclNoSuchElementError.Create(const Value: string);
begin
  inherited CreateResFmt(@RsEValueNotFound, [Value]);
end;

//=== { EJclDuplicateElementError } ==========================================

constructor EJclDuplicateElementError.Create;
begin
  inherited CreateRes(@RsEDuplicateElement);
end;

//=== { EJclIllegalQueueCapacityError } ======================================

constructor EJclIllegalQueueCapacityError.Create;
begin
  inherited CreateRes(@RsEIllegalQueueCapacity);
end;

//=== { EJclNoCollectionError } ==============================================

constructor EJclNoCollectionError.Create;
begin
  inherited CreateRes(@RsENoCollection);
end;

//=== { EJclOperationNotSupportedError } =====================================

constructor EJclOperationNotSupportedError.Create;
begin
  inherited CreateRes(@RsEOperationNotSupported);
end;

//=== { EJclIllegalStateOperationError } =====================================

constructor EJclIllegalStateOperationError.Create;
begin
  inherited CreateRes(@RsEIllegalStateOperation);
end;

//=== { EJclNoComparerError } ================================================

constructor EJclNoComparerError.Create;
begin
  inherited CreateRes(@RsENoComparer);
end;

//=== { EJclNoEqualityComparerError } ========================================

constructor EJclNoEqualityComparerError.Create;
begin
  inherited CreateRes(@RsENoEqualityComparer);
end;

//=== { EJclNoHashConverterError } ===========================================

constructor EJclNoHashConverterError.Create;
begin
  inherited CreateRes(@RsENoHashConverter);
end;

//=== { EJclAssignError } ====================================================

constructor EJclAssignError.Create;
begin
  inherited CreateRes(@RsEAssignError);
end;

//=== { EJclReadOnlyError } ==================================================

constructor EJclReadOnlyError.Create;
begin
  inherited CreateRes(@RsEReadOnlyError);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

