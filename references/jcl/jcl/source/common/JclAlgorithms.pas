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
{ The Original Code is Algorithms.pas.                                                             }
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

unit JclAlgorithms;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase, JclContainerIntf;

// Compare functions
function IntfSimpleCompare(const Obj1, Obj2: IInterface): Integer;
function AnsiStrSimpleCompare(const Obj1, Obj2: AnsiString): Integer;
function WideStrSimpleCompare(const Obj1, Obj2: WideString): Integer;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UnicodeStrSimpleCompare(const Obj1, Obj2: UnicodeString): Integer;
{$ENDIF SUPPORTS_UNICODE_STRING}
function StrSimpleCompare(const Obj1, Obj2: string): Integer;
function SingleSimpleCompare(const Obj1, Obj2: Single): Integer;
function DoubleSimpleCompare(const Obj1, Obj2: Double): Integer;
function ExtendedSimpleCompare(const Obj1, Obj2: Extended): Integer;
function FloatSimpleCompare(const Obj1, Obj2: Float): Integer;
function IntegerSimpleCompare(Obj1, Obj2: Integer): Integer;
function CardinalSimpleCompare(Obj1, Obj2: Cardinal): Integer;
function Int64SimpleCompare(const Obj1, Obj2: Int64): Integer;
function PtrSimpleCompare(Obj1, Obj2: Pointer): Integer;
function SimpleCompare(Obj1, Obj2: TObject): Integer;

function IntegerCompare(Obj1, Obj2: TObject): Integer;

function AnsiStrSimpleCompareI(const Obj1, Obj2: AnsiString): Integer;
function WideStrSimpleCompareI(const Obj1, Obj2: WideString): Integer;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UnicodeStrSimpleCompareI(const Obj1, Obj2: UnicodeString): Integer;
{$ENDIF SUPPORTS_UNICODE_STRING}
function StrSimpleCompareI(const Obj1, Obj2: string): Integer;

// Compare functions for equality
function IntfSimpleEqualityCompare(const Obj1, Obj2: IInterface): Boolean;
function AnsiStrSimpleEqualityCompare(const Obj1, Obj2: AnsiString): Boolean;
function WideStrSimpleEqualityCompare(const Obj1, Obj2: WideString): Boolean;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UnicodeStrSimpleEqualityCompare(const Obj1, Obj2: UnicodeString): Boolean;
{$ENDIF SUPPORTS_UNICODE_STRING}
function StrSimpleEqualityCompare(const Obj1, Obj2: string): Boolean;
function SingleSimpleEqualityCompare(const Obj1, Obj2: Single): Boolean;
function DoubleSimpleEqualityCompare(const Obj1, Obj2: Double): Boolean;
function ExtendedSimpleEqualityCompare(const Obj1, Obj2: Extended): Boolean;
function FloatSimpleEqualityCompare(const Obj1, Obj2: Float): Boolean;
function IntegerSimpleEqualityCompare(Obj1, Obj2: Integer): Boolean;
function CardinalSimpleEqualityCompare(Obj1, Obj2: Cardinal): Boolean;
function Int64SimpleEqualityCompare(const Obj1, Obj2: Int64): Boolean;
function PtrSimpleEqualityCompare(Obj1, Obj2: Pointer): Boolean;
function SimpleEqualityCompare(Obj1, Obj2: TObject): Boolean;

function AnsiStrSimpleEqualityCompareI(const Obj1, Obj2: AnsiString): Boolean;
function WideStrSimpleEqualityCompareI(const Obj1, Obj2: WideString): Boolean;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UnicodeStrSimpleEqualityCompareI(const Obj1, Obj2: UnicodeString): Boolean;
{$ENDIF SUPPORTS_UNICODE_STRING}
function StrSimpleEqualityCompareI(const Obj1, Obj2: string): Boolean;

// Hash conversion functions
function IntfSimpleHashConvert(const AInterface: IInterface): Integer;
function AnsiStrSimpleHashConvert(const AString: AnsiString): Integer;
function WideStrSimpleHashConvert(const AString: WideString): Integer;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UnicodeStrSimpleHashConvert(const AString: UnicodeString): Integer;
{$ENDIF SUPPORTS_UNICODE_STRING}
function StrSimpleHashConvert(const AString: string): Integer;
function SingleSimpleHashConvert(const AValue: Single): Integer;
function DoubleSimpleHashConvert(const AValue: Double): Integer;
function ExtendedSimpleHashConvert(const AValue: Extended): Integer;
function FloatSimpleHashConvert(const AValue: Float): Integer;
function IntegerSimpleHashConvert(AValue: Integer): Integer;
function CardinalSimpleHashConvert(AValue: Cardinal): Integer;
function Int64SimpleHashConvert(const AValue: Int64): Integer;
function PtrSimpleHashConvert(APtr: Pointer): Integer;
function SimpleHashConvert(AObject: TObject): Integer;

function AnsiStrSimpleHashConvertI(const AString: AnsiString): Integer;
function AnsiStrSimpleHashConvertU(const AString: AnsiString): Integer;
function AnsiStrSimpleHashConvertUI(const AString: AnsiString): Integer;
function WideStrSimpleHashConvertI(const AString: WideString): Integer;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UnicodeStrSimpleHashConvertI(const AString: UnicodeString): Integer;
{$ENDIF SUPPORTS_UNICODE_STRING}
function StrSimpleHashConvertI(const AString: string): Integer;

type
  // Hash Function
  // Result must be in 0..Range-1
  TJclHashToRangeFunction = function(Key, Range: Integer): Integer;

function JclSimpleHashToRange(Key, Range: Integer): Integer;

// move array algorithms
procedure FinalizeArrayBeforeMove(var List: TDynIInterfaceArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure InitializeArray(var List: TDynIInterfaceArray; FromIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure InitializeArrayAfterMove(var List: TDynIInterfaceArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

procedure MoveArray(var List: TDynIInterfaceArray; FromIndex, ToIndex, Count: SizeInt); overload;
procedure FinalizeArrayBeforeMove(var List: TDynAnsiStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure InitializeArray(var List: TDynAnsiStringArray; FromIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure InitializeArrayAfterMove(var List: TDynAnsiStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

procedure MoveArray(var List: TDynAnsiStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
procedure FinalizeArrayBeforeMove(var List: TDynWideStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure InitializeArray(var List: TDynWideStringArray; FromIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure InitializeArrayAfterMove(var List: TDynWideStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

procedure MoveArray(var List: TDynWideStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
{$IFDEF SUPPORTS_UNICODE_STRING}

procedure FinalizeArrayBeforeMove(var List: TDynUnicodeStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure InitializeArray(var List: TDynUnicodeStringArray; FromIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure InitializeArrayAfterMove(var List: TDynUnicodeStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

procedure MoveArray(var List: TDynUnicodeStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
procedure InitializeArrayAfterMove(var List: TDynSingleArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure MoveArray(var List: TDynSingleArray; FromIndex, ToIndex, Count: SizeInt); overload;
procedure InitializeArrayAfterMove(var List: TDynDoubleArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure MoveArray(var List: TDynDoubleArray; FromIndex, ToIndex, Count: SizeInt); overload;
procedure InitializeArrayAfterMove(var List: TDynExtendedArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure MoveArray(var List: TDynExtendedArray; FromIndex, ToIndex, Count: SizeInt); overload;
procedure InitializeArrayAfterMove(var List: TDynIntegerArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure MoveArray(var List: TDynIntegerArray; FromIndex, ToIndex, Count: SizeInt); overload;
procedure InitializeArrayAfterMove(var List: TDynCardinalArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure MoveArray(var List: TDynCardinalArray; FromIndex, ToIndex, Count: SizeInt); overload;
procedure InitializeArrayAfterMove(var List: TDynInt64Array; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure MoveArray(var List: TDynInt64Array; FromIndex, ToIndex, Count: SizeInt); overload;
procedure InitializeArrayAfterMove(var List: TDynPointerArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure MoveArray(var List: TDynPointerArray; FromIndex, ToIndex, Count: SizeInt); overload;
procedure InitializeArrayAfterMove(var List: TDynObjectArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure MoveArray(var List: TDynObjectArray; FromIndex, ToIndex, Count: SizeInt); overload;

{$IFDEF GENERIC}
procedure MoveArray(var List: TDynSizeIntArray; FromIndex, ToIndex, Count: SizeInt); overload;
{$ELSE ~GENERIC}
{$IFDEF REFCOUNTED}
procedure FinalizeArrayBeforeMove(var List: TDynSizeIntArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure InitializeArray(var List: TDynSizeIntArray; FromIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure InitializeArrayAfterMove(var List: TDynSizeIntArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ELSE ~REFCOUNTED}
{$IFDEF ZEROINIT}
procedure InitializeArrayAfterMove(var List: TDynSizeIntArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ENDIF ZEROINIT}
{$ENDIF ~REFCOUNTED}
procedure MoveArray(var List: TDynSizeIntArray; FromIndex, ToIndex, Count: SizeInt); overload;{$ENDIF ~GENERIC}
{$IFNDEF FPC}
{$IFDEF GENERIC}
procedure MoveArray(var List: TDynStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
{$ELSE ~GENERIC}
{$IFDEF REFCOUNTED}
procedure FinalizeArrayBeforeMove(var List: TDynStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure InitializeArray(var List: TDynStringArray; FromIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure InitializeArrayAfterMove(var List: TDynStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ELSE ~REFCOUNTED}
{$IFDEF ZEROINIT}
procedure InitializeArrayAfterMove(var List: TDynStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ENDIF ZEROINIT}
{$ENDIF ~REFCOUNTED}
procedure MoveArray(var List: TDynStringArray; FromIndex, ToIndex, Count: SizeInt); overload;{$ENDIF ~GENERIC}
{$IFDEF GENERIC}
procedure MoveArray(var List: TDynFloatArray; FromIndex, ToIndex, Count: SizeInt); overload;
{$ELSE ~GENERIC}
{$IFDEF REFCOUNTED}
procedure FinalizeArrayBeforeMove(var List: TDynFloatArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure InitializeArray(var List: TDynFloatArray; FromIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure InitializeArrayAfterMove(var List: TDynFloatArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ELSE ~REFCOUNTED}
{$IFDEF ZEROINIT}
procedure InitializeArrayAfterMove(var List: TDynFloatArray; FromIndex, ToIndex, Count: SizeInt); overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ENDIF ZEROINIT}
{$ENDIF ~REFCOUNTED}
procedure MoveArray(var List: TDynFloatArray; FromIndex, ToIndex, Count: SizeInt); overload;{$ENDIF ~GENERIC}
{$ENDIF ~FPC}

// Iterate algorithms
procedure Iterate(const First: IJclIntfIterator; Count: Integer; F: TIntfIterateProcedure); overload;
procedure Iterate(const First: IJclAnsiStrIterator; Count: Integer; F: TAnsiStrIterateProcedure); overload;
procedure Iterate(const First: IJclWideStrIterator; Count: Integer; F: TWideStrIterateProcedure); overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
procedure Iterate(const First: IJclUnicodeStrIterator; Count: Integer; F: TUnicodeStrIterateProcedure); overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
procedure Iterate(const First: IJclSingleIterator; Count: Integer; F: TSingleIterateProcedure); overload;
procedure Iterate(const First: IJclDoubleIterator; Count: Integer; F: TDoubleIterateProcedure); overload;
procedure Iterate(const First: IJclExtendedIterator; Count: Integer; F: TExtendedIterateProcedure); overload;
procedure Iterate(const First: IJclIntegerIterator; Count: Integer; F: TIntegerIterateProcedure); overload;
procedure Iterate(const First: IJclCardinalIterator; Count: Integer; F: TCardinalIterateProcedure); overload;
procedure Iterate(const First: IJclInt64Iterator; Count: Integer; F: TInt64IterateProcedure); overload;
procedure Iterate(const First: IJclPtrIterator; Count: Integer; F: TPtrIterateProcedure); overload;
procedure Iterate(const First: IJclIterator; Count: Integer; F: TIterateProcedure); overload;


// Apply algorithms
procedure Apply(const First: IJclIntfIterator; Count: Integer; F: TIntfApplyFunction); overload;
procedure Apply(const First: IJclAnsiStrIterator; Count: Integer; F: TAnsiStrApplyFunction); overload;
procedure Apply(const First: IJclWideStrIterator; Count: Integer; F: TWideStrApplyFunction); overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
procedure Apply(const First: IJclUnicodeStrIterator; Count: Integer; F: TUnicodeStrApplyFunction); overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
procedure Apply(const First: IJclSingleIterator; Count: Integer; F: TSingleApplyFunction); overload;
procedure Apply(const First: IJclDoubleIterator; Count: Integer; F: TDoubleApplyFunction); overload;
procedure Apply(const First: IJclExtendedIterator; Count: Integer; F: TExtendedApplyFunction); overload;
procedure Apply(const First: IJclIntegerIterator; Count: Integer; F: TIntegerApplyFunction); overload;
procedure Apply(const First: IJclCardinalIterator; Count: Integer; F: TCardinalApplyFunction); overload;
procedure Apply(const First: IJclInt64Iterator; Count: Integer; F: TInt64ApplyFunction); overload;
procedure Apply(const First: IJclPtrIterator; Count: Integer; F: TPtrApplyFunction); overload;
procedure Apply(const First: IJclIterator; Count: Integer; F: TApplyFunction); overload;


// Find algorithms
function Find(const First: IJclIntfIterator; Count: Integer; const AInterface: IInterface;
  AComparator: TIntfCompare): IJclIntfIterator; overload;
function Find(const First: IJclIntfIterator; Count: Integer; const AInterface: IInterface;
  AEqualityComparator: TIntfEqualityCompare): IJclIntfIterator; overload;
function Find(const First: IJclAnsiStrIterator; Count: Integer; const AString: AnsiString;
  AComparator: TAnsiStrCompare): IJclAnsiStrIterator; overload;
function Find(const First: IJclAnsiStrIterator; Count: Integer; const AString: AnsiString;
  AEqualityComparator: TAnsiStrEqualityCompare): IJclAnsiStrIterator; overload;
function Find(const First: IJclWideStrIterator; Count: Integer; const AString: WideString;
  AComparator: TWideStrCompare): IJclWideStrIterator; overload;
function Find(const First: IJclWideStrIterator; Count: Integer; const AString: WideString;
  AEqualityComparator: TWideStrEqualityCompare): IJclWideStrIterator; overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
function Find(const First: IJclUnicodeStrIterator; Count: Integer; const AString: UnicodeString;
  AComparator: TUnicodeStrCompare): IJclUnicodeStrIterator; overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
{$IFDEF SUPPORTS_UNICODE_STRING}
function Find(const First: IJclUnicodeStrIterator; Count: Integer; const AString: UnicodeString;
  AEqualityComparator: TUnicodeStrEqualityCompare): IJclUnicodeStrIterator; overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
function Find(const First: IJclSingleIterator; Count: Integer; const AValue: Single;
  AComparator: TSingleCompare): IJclSingleIterator; overload;
function Find(const First: IJclSingleIterator; Count: Integer; const AValue: Single;
  AEqualityComparator: TSingleEqualityCompare): IJclSingleIterator; overload;
function Find(const First: IJclDoubleIterator; Count: Integer; const AValue: Double;
  AComparator: TDoubleCompare): IJclDoubleIterator; overload;
function Find(const First: IJclDoubleIterator; Count: Integer; const AValue: Double;
  AEqualityComparator: TDoubleEqualityCompare): IJclDoubleIterator; overload;
function Find(const First: IJclExtendedIterator; Count: Integer; const AValue: Extended;
  AComparator: TExtendedCompare): IJclExtendedIterator; overload;
function Find(const First: IJclExtendedIterator; Count: Integer; const AValue: Extended;
  AEqualityComparator: TExtendedEqualityCompare): IJclExtendedIterator; overload;
function Find(const First: IJclIntegerIterator; Count: Integer; AValue: Integer;
  AComparator: TIntegerCompare): IJclIntegerIterator; overload;
function Find(const First: IJclIntegerIterator; Count: Integer; AValue: Integer;
  AEqualityComparator: TIntegerEqualityCompare): IJclIntegerIterator; overload;
function Find(const First: IJclCardinalIterator; Count: Integer; AValue: Cardinal;
  AComparator: TCardinalCompare): IJclCardinalIterator; overload;
function Find(const First: IJclCardinalIterator; Count: Integer; AValue: Cardinal;
  AEqualityComparator: TCardinalEqualityCompare): IJclCardinalIterator; overload;
function Find(const First: IJclInt64Iterator; Count: Integer; const AValue: Int64;
  AComparator: TInt64Compare): IJclInt64Iterator; overload;
function Find(const First: IJclInt64Iterator; Count: Integer; const AValue: Int64;
  AEqualityComparator: TInt64EqualityCompare): IJclInt64Iterator; overload;
function Find(const First: IJclPtrIterator; Count: Integer; APtr: Pointer;
  AComparator: TPtrCompare): IJclPtrIterator; overload;
function Find(const First: IJclPtrIterator; Count: Integer; APtr: Pointer;
  AEqualityComparator: TPtrEqualityCompare): IJclPtrIterator; overload;
function Find(const First: IJclIterator; Count: Integer; AObject: TObject;
  AComparator: TCompare): IJclIterator; overload;
function Find(const First: IJclIterator; Count: Integer; AObject: TObject;
  AEqualityComparator: TEqualityCompare): IJclIterator; overload;


// CountObject algorithms
function CountObject(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AComparator: TIntfCompare): Integer; overload;
function CountObject(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AEqualityComparator: TIntfEqualityCompare): Integer; overload;
function CountObject(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString; AComparator: TAnsiStrCompare): Integer; overload;
function CountObject(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString; AEqualityComparator: TAnsiStrEqualityCompare): Integer; overload;
function CountObject(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString; AComparator: TWideStrCompare): Integer; overload;
function CountObject(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString; AEqualityComparator: TWideStrEqualityCompare): Integer; overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
function CountObject(const First: IJclUnicodeStrIterator; Count: Integer;
  const AString: UnicodeString; AComparator: TUnicodeStrCompare): Integer; overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
{$IFDEF SUPPORTS_UNICODE_STRING}
function CountObject(const First: IJclUnicodeStrIterator; Count: Integer;
  const AString: UnicodeString; AEqualityComparator: TUnicodeStrEqualityCompare): Integer; overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
function CountObject(const First: IJclSingleIterator; Count: Integer;
  const AValue: Single; AComparator: TSingleCompare): Integer; overload;
function CountObject(const First: IJclSingleIterator; Count: Integer;
  const AValue: Single; AEqualityComparator: TSingleEqualityCompare): Integer; overload;
function CountObject(const First: IJclDoubleIterator; Count: Integer;
  const AValue: Double; AComparator: TDoubleCompare): Integer; overload;
function CountObject(const First: IJclDoubleIterator; Count: Integer;
  const AValue: Double; AEqualityComparator: TDoubleEqualityCompare): Integer; overload;
function CountObject(const First: IJclExtendedIterator; Count: Integer;
  const AValue: Extended; AComparator: TExtendedCompare): Integer; overload;
function CountObject(const First: IJclExtendedIterator; Count: Integer;
  const AValue: Extended; AEqualityComparator: TExtendedEqualityCompare): Integer; overload;
function CountObject(const First: IJclIntegerIterator; Count: Integer;
  AValue: Integer; AComparator: TIntegerCompare): Integer; overload;
function CountObject(const First: IJclIntegerIterator; Count: Integer;
  AValue: Integer; AEqualityComparator: TIntegerEqualityCompare): Integer; overload;
function CountObject(const First: IJclCardinalIterator; Count: Integer;
  AValue: Cardinal; AComparator: TCardinalCompare): Integer; overload;
function CountObject(const First: IJclCardinalIterator; Count: Integer;
  AValue: Cardinal; AEqualityComparator: TCardinalEqualityCompare): Integer; overload;
function CountObject(const First: IJclInt64Iterator; Count: Integer;
  const AValue: Int64; AComparator: TInt64Compare): Integer; overload;
function CountObject(const First: IJclInt64Iterator; Count: Integer;
  const AValue: Int64; AEqualityComparator: TInt64EqualityCompare): Integer; overload;
function CountObject(const First: IJclPtrIterator; Count: Integer;
  APtr: Pointer; AComparator: TPtrCompare): Integer; overload;
function CountObject(const First: IJclPtrIterator; Count: Integer;
  APtr: Pointer; AEqualityComparator: TPtrEqualityCompare): Integer; overload;
function CountObject(const First: IJclIterator; Count: Integer;
  AObject: TObject; AComparator: TCompare): Integer; overload;
function CountObject(const First: IJclIterator; Count: Integer;
  AObject: TObject; AEqualityComparator: TEqualityCompare): Integer; overload;


// Copy algorithms
procedure Copy(const First: IJclIntfIterator; Count: Integer;
  const Output: IJclIntfIterator); overload;
procedure Copy(const First: IJclAnsiStrIterator; Count: Integer;
  const Output: IJclAnsiStrIterator); overload;
procedure Copy(const First: IJclWideStrIterator; Count: Integer;
  const Output: IJclWideStrIterator); overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
procedure Copy(const First: IJclUnicodeStrIterator; Count: Integer;
  const Output: IJclUnicodeStrIterator); overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
procedure Copy(const First: IJclSingleIterator; Count: Integer;
  const Output: IJclSingleIterator); overload;
procedure Copy(const First: IJclDoubleIterator; Count: Integer;
  const Output: IJclDoubleIterator); overload;
procedure Copy(const First: IJclExtendedIterator; Count: Integer;
  const Output: IJclExtendedIterator); overload;
procedure Copy(const First: IJclIntegerIterator; Count: Integer;
  const Output: IJclIntegerIterator); overload;
procedure Copy(const First: IJclCardinalIterator; Count: Integer;
  const Output: IJclCardinalIterator); overload;
procedure Copy(const First: IJclInt64Iterator; Count: Integer;
  const Output: IJclInt64Iterator); overload;
procedure Copy(const First: IJclPtrIterator; Count: Integer;
  const Output: IJclPtrIterator); overload;
procedure Copy(const First: IJclIterator; Count: Integer;
  const Output: IJclIterator); overload;


// Generate algorithms
procedure Generate(const List: IJclIntfList; Count: Integer; const AInterface: IInterface); overload;
procedure Generate(const List: IJclAnsiStrList; Count: Integer; const AString: AnsiString); overload;
procedure Generate(const List: IJclWideStrList; Count: Integer; const AString: WideString); overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
procedure Generate(const List: IJclUnicodeStrList; Count: Integer; const AString: UnicodeString); overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
procedure Generate(const List: IJclSingleList; Count: Integer; const AValue: Single); overload;
procedure Generate(const List: IJclDoubleList; Count: Integer; const AValue: Double); overload;
procedure Generate(const List: IJclExtendedList; Count: Integer; const AValue: Extended); overload;
procedure Generate(const List: IJclIntegerList; Count: Integer; AValue: Integer); overload;
procedure Generate(const List: IJclCardinalList; Count: Integer; AValue: Cardinal); overload;
procedure Generate(const List: IJclInt64List; Count: Integer; const AValue: Int64); overload;
procedure Generate(const List: IJclPtrList; Count: Integer; APtr: Pointer); overload;
procedure Generate(const List: IJclList; Count: Integer; AObject: TObject); overload;


// Fill algorithms
procedure Fill(const First: IJclIntfIterator; Count: Integer; const AInterface: IInterface); overload;
procedure Fill(const First: IJclAnsiStrIterator; Count: Integer; const AString: AnsiString); overload;
procedure Fill(const First: IJclWideStrIterator; Count: Integer; const AString: WideString); overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
procedure Fill(const First: IJclUnicodeStrIterator; Count: Integer; const AString: UnicodeString); overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
procedure Fill(const First: IJclSingleIterator; Count: Integer; const AValue: Single); overload;
procedure Fill(const First: IJclDoubleIterator; Count: Integer; const AValue: Double); overload;
procedure Fill(const First: IJclExtendedIterator; Count: Integer; const AValue: Extended); overload;
procedure Fill(const First: IJclIntegerIterator; Count: Integer; AValue: Integer); overload;
procedure Fill(const First: IJclCardinalIterator; Count: Integer; AValue: Cardinal); overload;
procedure Fill(const First: IJclInt64Iterator; Count: Integer; const AValue: Int64); overload;
procedure Fill(const First: IJclPtrIterator; Count: Integer; APtr: Pointer); overload;
procedure Fill(const First: IJclIterator; Count: Integer; AObject: TObject); overload;


// Reverse algorithms
procedure Reverse(const First, Last: IJclIntfIterator); overload;
procedure Reverse(const First, Last: IJclAnsiStrIterator); overload;
procedure Reverse(const First, Last: IJclWideStrIterator); overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
procedure Reverse(const First, Last: IJclUnicodeStrIterator); overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
procedure Reverse(const First, Last: IJclSingleIterator); overload;
procedure Reverse(const First, Last: IJclDoubleIterator); overload;
procedure Reverse(const First, Last: IJclExtendedIterator); overload;
procedure Reverse(const First, Last: IJclIntegerIterator); overload;
procedure Reverse(const First, Last: IJclCardinalIterator); overload;
procedure Reverse(const First, Last: IJclInt64Iterator); overload;
procedure Reverse(const First, Last: IJclPtrIterator); overload;
procedure Reverse(const First, Last: IJclIterator); overload;


procedure QuickSort(const AList: IJclIntfList; L, R: Integer; AComparator: TIntfCompare); overload;
procedure QuickSort(const AList: IJclAnsiStrList; L, R: Integer; AComparator: TAnsiStrCompare); overload;
procedure QuickSort(const AList: IJclWideStrList; L, R: Integer; AComparator: TWideStrCompare); overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
procedure QuickSort(const AList: IJclUnicodeStrList; L, R: Integer; AComparator: TUnicodeStrCompare); overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
procedure QuickSort(const AList: IJclSingleList; L, R: Integer; AComparator: TSingleCompare); overload;
procedure QuickSort(const AList: IJclDoubleList; L, R: Integer; AComparator: TDoubleCompare); overload;
procedure QuickSort(const AList: IJclExtendedList; L, R: Integer; AComparator: TExtendedCompare); overload;
procedure QuickSort(const AList: IJclIntegerList; L, R: Integer; AComparator: TIntegerCompare); overload;
procedure QuickSort(const AList: IJclCardinalList; L, R: Integer; AComparator: TCardinalCompare); overload;
procedure QuickSort(const AList: IJclInt64List; L, R: Integer; AComparator: TInt64Compare); overload;
procedure QuickSort(const AList: IJclPtrList; L, R: Integer; AComparator: TPtrCompare); overload;
procedure QuickSort(const AList: IJclList; L, R: Integer; AComparator: TCompare); overload;


var
  IntfSortProc: TIntfSortProc = QuickSort;
  AnsiStrSortProc: TAnsiStrSortProc = QuickSort;
  WideStrSortProc: TWideStrSortProc = QuickSort;
  {$IFDEF SUPPORTS_UNICODE_STRING}
  UnicodeStrSortProc: TUnicodeStrSortProc = QuickSort;
  {$ENDIF SUPPORTS_UNICODE_STRING}
  SingleSortProc: TSingleSortProc = QuickSort;
  DoubleSortProc: TDoubleSortProc = QuickSort;
  ExtendedSortProc: TExtendedSortProc = QuickSort;
  IntegerSortProc: TIntegerSortProc = QuickSort;
  CardinalSortProc: TCardinalSortProc = QuickSort;
  Int64SortProc: TInt64SortProc = QuickSort;
  PtrSortProc: TPtrSortProc = QuickSort;
  SortProc: TSortProc = QuickSort;

// Sort algorithms
procedure Sort(const AList: IJclIntfList; First, Last: Integer; AComparator: TIntfCompare); overload;
procedure Sort(const AList: IJclAnsiStrList; First, Last: Integer; AComparator: TAnsiStrCompare); overload;
procedure Sort(const AList: IJclWideStrList; First, Last: Integer; AComparator: TWideStrCompare); overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
procedure Sort(const AList: IJclUnicodeStrList; First, Last: Integer; AComparator: TUnicodeStrCompare); overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
procedure Sort(const AList: IJclSingleList; First, Last: Integer; AComparator: TSingleCompare); overload;
procedure Sort(const AList: IJclDoubleList; First, Last: Integer; AComparator: TDoubleCompare); overload;
procedure Sort(const AList: IJclExtendedList; First, Last: Integer; AComparator: TExtendedCompare); overload;
procedure Sort(const AList: IJclIntegerList; First, Last: Integer; AComparator: TIntegerCompare); overload;
procedure Sort(const AList: IJclCardinalList; First, Last: Integer; AComparator: TCardinalCompare); overload;
procedure Sort(const AList: IJclInt64List; First, Last: Integer; AComparator: TInt64Compare); overload;
procedure Sort(const AList: IJclPtrList; First, Last: Integer; AComparator: TPtrCompare); overload;
procedure Sort(const AList: IJclList; First, Last: Integer; AComparator: TCompare); overload;


{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

type
  // cannot implement generic global functions
  TJclAlgorithms<T> = class
  private
    //FSortProc: TSortProc;
  public
    class procedure Iterate(const First: IJclIterator<T>; Count: Integer; F: TIterateProcedure<T>);
    class procedure Apply(const First: IJclIterator<T>; Count: Integer; F: TApplyFunction<T>);
    class function Find(const First: IJclIterator<T>; Count: Integer; const AItem: T;
      AComparator: TCompare<T>): IJclIterator<T>; overload;
    class function Find(const First: IJclIterator<T>; Count: Integer; const AItem: T;
      AEqualityComparator: TEqualityCompare<T>): IJclIterator<T>; overload;
    class function CountObject(const First: IJclIterator<T>; Count: Integer;
      const AItem: T; AComparator: TCompare<T>): Integer; overload;
    class function CountObject(const First: IJclIterator<T>; Count: Integer;
      const AItem: T; AEqualityComparator: TEqualityCompare<T>): Integer; overload;
    class procedure Copy(const First: IJclIterator<T>; Count: Integer;
      const Output: IJclIterator<T>);
    class procedure Generate(const List: IJclList<T>; Count: Integer; const AItem: T);
    class procedure Fill(const First: IJclIterator<T>; Count: Integer; const AItem: T);
    class procedure Reverse(const First, Last: IJclIterator<T>);
    class procedure QuickSort(const AList: IJclList<T>; L, R: Integer; AComparator: TCompare<T>);
    class procedure Sort(const AList: IJclList<T>; First, Last: Integer; AComparator: TCompare<T>);
    //class property SortProc: TSortProc<T> read FSortProc write FSortProc;
  end;

//DOM-IGNORE-END
{$ENDIF SUPPORTS_GENERICS}

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
  {$IFDEF COMPILER11_UP}
  Winapi.Windows,
  {$ENDIF COMPILER11_UP}
  {$IFDEF HAS_UNIT_ANSISTRINGS}
  System.AnsiStrings,
  {$ENDIF HAS_UNIT_ANSISTRINGS}
  {$IFDEF UNICODE_RTL_DATABASE}
  System.Character,
  {$ENDIF UNICODE_RTL_DATABASE}
  System.SysUtils,
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF COMPILER11_UP}
  Windows,
  {$ENDIF COMPILER11_UP}
  {$IFDEF HAS_UNIT_ANSISTRINGS}
  AnsiStrings,
  {$ENDIF HAS_UNIT_ANSISTRINGS}
  {$IFDEF UNICODE_RTL_DATABASE}
  Character,
  {$ENDIF UNICODE_RTL_DATABASE}
  SysUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  JclAnsiStrings, JclStringConversions, JclUnicode;

function IntfSimpleCompare(const Obj1, Obj2: IInterface): Integer;
begin
  if SizeInt(Obj1) < SizeInt(Obj2) then
    Result := -1
  else
  if SizeInt(Obj1) > SizeInt(Obj2) then
    Result := 1
  else
    Result := 0;
end;

// default is case-sensitive
function AnsiStrSimpleCompare(const Obj1, Obj2: AnsiString): Integer;
begin
  Result := CompareStr(Obj1, Obj2);
end;

// case-insensitive
function AnsiStrSimpleCompareI(const Obj1, Obj2: AnsiString): Integer;
begin
  Result := CompareText(Obj1, Obj2);
end;

// default is case-sensitive
function WideStrSimpleCompare(const Obj1, Obj2: WideString): Integer;
begin
  Result := WideCompareStr(Obj1, Obj2);
end;

// case-insensitive
function WideStrSimpleCompareI(const Obj1, Obj2: WideString): Integer;
begin
  Result := {$IFDEF HAS_UNITSCOPE}System.{$ENDIF}SysUtils.WideCompareText(Obj1, Obj2);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
// default is case-sensitive
function UnicodeStrSimpleCompare(const Obj1, Obj2: UnicodeString): Integer;
begin
  Result := CompareStr(Obj1, Obj2);
end;

// case-insensitive
function UnicodeStrSimpleCompareI(const Obj1, Obj2: UnicodeString): Integer;
begin
  Result := CompareText(Obj1, Obj2);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

function StrSimpleCompare(const Obj1, Obj2: string): Integer;
begin
  case SizeOf(Obj1[1]) of
    SizeOf(AnsiChar):
      Result := CompareStr(Obj1, Obj2);
    SizeOf(WideChar):
      {$IFDEF SUPPORTS_UNICODE}
      Result := CompareStr(Obj1, Obj2);
      {$ELSE ~SUPPORTS_UNICODE}
      Result := WideCompareStr(Obj1, Obj2);
      {$ENDIF ~SUPPORTS_UNICODE}
  else
    raise EJclOperationNotSupportedError.Create;
  end;
end;

function StrSimpleCompareI(const Obj1, Obj2: string): Integer;
begin
  case SizeOf(Obj1[1]) of
    SizeOf(AnsiChar):
      Result := CompareText(Obj1, Obj2);
    SizeOf(WideChar):
      {$IFDEF SUPPORTS_UNICODE}
      Result := CompareText(Obj1, Obj2);
      {$ELSE ~SUPPORTS_UNICODE}
      {$IFDEF HAS_UNITSCOPE}System.{$ENDIF}SysUtils.WideCompareText(Obj1, Obj2);
      {$ENDIF ~SUPPORTS_UNICODE}
  else
    raise EJclOperationNotSupportedError.Create;
  end;
end;

function SingleSimpleCompare(const Obj1, Obj2: Single): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function DoubleSimpleCompare(const Obj1, Obj2: Double): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function ExtendedSimpleCompare(const Obj1, Obj2: Extended): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function FloatSimpleCompare(const Obj1, Obj2: Float): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function IntegerSimpleCompare(Obj1, Obj2: Integer): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function CardinalSimpleCompare(Obj1, Obj2: Cardinal): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function Int64SimpleCompare(const Obj1, Obj2: Int64): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function PtrSimpleCompare(Obj1, Obj2: Pointer): Integer;
begin
  if SizeInt(Obj1) < SizeInt(Obj2) then
    Result := -1
  else
  if SizeInt(Obj1) > SizeInt(Obj2) then
    Result := 1
  else
    Result := 0;
end;

function SimpleCompare(Obj1, Obj2: TObject): Integer;
begin
  if SizeInt(Obj1) < SizeInt(Obj2) then
    Result := -1
  else
  if SizeInt(Obj1) > SizeInt(Obj2) then
    Result := 1
  else
    Result := 0;
end;

function IntegerCompare(Obj1, Obj2: TObject): Integer;
begin
  if SizeInt(Obj1) < SizeInt(Obj2) then
    Result := -1
  else
  if SizeInt(Obj1) > SizeInt(Obj2) then
    Result := 1
  else
    Result := 0;
end;

function IntfSimpleEqualityCompare(const Obj1, Obj2: IInterface): Boolean;
begin
  Result := SizeInt(Obj1) = SizeInt(Obj2);
end;

// default is case-sensitive
function AnsiStrSimpleEqualityCompare(const Obj1, Obj2: AnsiString): Boolean;
begin
  Result := CompareStr(Obj1, Obj2) = 0;
end;

// case-insensitive
function AnsiStrSimpleEqualityCompareI(const Obj1, Obj2: AnsiString): Boolean;
begin
  Result := CompareText(Obj1, Obj2) = 0;
end;

// default is case-sensitive
function WideStrSimpleEqualityCompare(const Obj1, Obj2: WideString): Boolean;
begin
  Result := WideCompareStr(Obj1, Obj2) = 0;
end;

// case-insensitive
function WideStrSimpleEqualityCompareI(const Obj1, Obj2: WideString): Boolean;
begin
  Result := {$IFDEF HAS_UNITSCOPE}System.{$ENDIF}SysUtils.WideCompareText(Obj1, Obj2) = 0;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
// default is case-sensitive
function UnicodeStrSimpleEqualityCompare(const Obj1, Obj2: UnicodeString): Boolean;
begin
  Result := CompareStr(Obj1, Obj2) = 0;
end;

// case-insensitive
function UnicodeStrSimpleEqualityCompareI(const Obj1, Obj2: UnicodeString): Boolean;
begin
  Result := CompareText(Obj1, Obj2) = 0;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

function StrSimpleEqualityCompare(const Obj1, Obj2: string): Boolean;
begin
  case SizeOf(Obj1[1]) of
    SizeOf(AnsiChar):
      Result := CompareStr(Obj1, Obj2) = 0;
    SizeOf(WideChar):
      Result := WideCompareStr(Obj1, Obj2) = 0;
  else
    raise EJclOperationNotSupportedError.Create;
  end;
end;

function StrSimpleEqualityCompareI(const Obj1, Obj2: string): Boolean;
begin
  case SizeOf(Obj1[1]) of
    SizeOf(AnsiChar):
      Result := CompareText(Obj1, Obj2) = 0;
    SizeOf(WideChar):
      Result := {$IFDEF HAS_UNITSCOPE}System.{$ENDIF}SysUtils.WideCompareText(Obj1, Obj2) = 0;
  else
    raise EJclOperationNotSupportedError.Create;
  end;
end;

function SingleSimpleEqualityCompare(const Obj1, Obj2: Single): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function DoubleSimpleEqualityCompare(const Obj1, Obj2: Double): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function ExtendedSimpleEqualityCompare(const Obj1, Obj2: Extended): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function FloatSimpleEqualityCompare(const Obj1, Obj2: Float): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function IntegerSimpleEqualityCompare(Obj1, Obj2: Integer): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function CardinalSimpleEqualityCompare(Obj1, Obj2: Cardinal): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function Int64SimpleEqualityCompare(const Obj1, Obj2: Int64): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function PtrSimpleEqualityCompare(Obj1, Obj2: Pointer): Boolean;
begin
  Result := SizeInt(Obj1) = SizeInt(Obj2);
end;

function SimpleEqualityCompare(Obj1, Obj2: TObject): Boolean;
begin
  Result := SizeInt(Obj1) = SizeInt(Obj2);
end;

function IntfSimpleHashConvert(const AInterface: IInterface): Integer;
begin
  {$IFDEF CPU32}
  Result := SizeInt(AInterface) and MaxInt;
  {$ELSE ~CPU32}
  Result := (SizeInt(AInterface) xor (SizeInt(AInterface) shr 32)) and MaxInt;
  {$ENDIF ~CPU32}
end;

// from "Fast Hashing of Variable-Length Text Strings", Peter K. Pearson, 1990
// http://portal.acm.org/citation.cfm?id=78978
type
  TIntegerHash = packed record
    case Byte of
      0: (H1, H2, H3, H4: Byte);
      1: (H: Integer);
      2: (C: UCS4);
  end;

// default is case-sensitive and ISO-encoded
function AnsiStrSimpleHashConvert(const AString: AnsiString): Integer;
var
  I: Integer;
  C: Byte;
  IntegerHash: TIntegerHash;
begin
  IntegerHash.H1 := 0;
  IntegerHash.H2 := 1;
  IntegerHash.H3 := 2;
  IntegerHash.H4 := 3;
  for I := 1 to Length(AString) do
  begin
    C := Ord(AString[I]);
    IntegerHash.H1 := BytePermTable[IntegerHash.H1 xor C];
    IntegerHash.H2 := BytePermTable[IntegerHash.H2 xor C];
    IntegerHash.H3 := BytePermTable[IntegerHash.H3 xor C];
    IntegerHash.H4 := BytePermTable[IntegerHash.H4 xor C];
  end;
  Result := IntegerHash.H and MaxInt;
end;

// case-insensitive and ISO-encoded
function AnsiStrSimpleHashConvertI(const AString: AnsiString): Integer;
var
  I: Integer;
  C: Byte;
  IntegerHash: TIntegerHash;
begin
  IntegerHash.H1 := 0;
  IntegerHash.H2 := 1;
  IntegerHash.H3 := 2;
  IntegerHash.H4 := 3;
  for I := 1 to Length(AString) - 1 do
  begin
    C := Ord(JclAnsiStrings.CharUpper(AString[I]));
    IntegerHash.H1 := BytePermTable[IntegerHash.H1 xor C];
    IntegerHash.H2 := BytePermTable[IntegerHash.H2 xor C];
    IntegerHash.H3 := BytePermTable[IntegerHash.H3 xor C];
    IntegerHash.H4 := BytePermTable[IntegerHash.H4 xor C];
  end;
  Result := IntegerHash.H and MaxInt;
end;

// case-sensitive and UTF8-encoded
function AnsiStrSimpleHashConvertU(const AString: AnsiString): Integer;
var
  I: SizeInt;
  C, IntegerHash: TIntegerHash;
begin
  IntegerHash.H1 := 0;
  IntegerHash.H2 := 1;
  IntegerHash.H3 := 2;
  IntegerHash.H4 := 3;
  I := 1;
  while I < Length(AString) do
  begin
    C.C := UTF8GetNextChar(AString, I);
    if I = -1 then
      raise EJclUnexpectedEOSequenceError.Create;
    IntegerHash.H1 := BytePermTable[IntegerHash.H1 xor C.H1];
    IntegerHash.H2 := BytePermTable[IntegerHash.H2 xor C.H2];
    IntegerHash.H3 := BytePermTable[IntegerHash.H3 xor C.H3];
    IntegerHash.H4 := BytePermTable[IntegerHash.H4 xor C.H4];
  end;
  Result := IntegerHash.H and MaxInt;
end;

// case-insensitive and UTF8-encoded
function AnsiStrSimpleHashConvertUI(const AString: AnsiString): Integer;
var
  I: SizeInt;
  J: Integer;
  C, IntegerHash: TIntegerHash;
  CA: TUCS4Array;
begin
  IntegerHash.H1 := 0;
  IntegerHash.H2 := 1;
  IntegerHash.H3 := 2;
  IntegerHash.H4 := 3;
  I := 1;
  {$IFDEF UNICODE_RTL_DATABASE}
  SetLength(CA, 1);
  {$ELSE ~UNICODE_RTL_DATABASE}
  SetLength(CA, 0);
  {$ENDIF ~UNICODE_RTL_DATABASE}
  while I < Length(AString) do
  begin
    C.C := UTF8GetNextChar(AString, I);
    {$IFDEF UNICODE_RTL_DATABASE}
    CA[0] := Ord(TCharacter.ToLower(Chr(C.C)));
    {$ELSE ~UNICODE_RTL_DATABASE}
    CA := UnicodeCaseFold(C.C);
    {$ENDIF ~UNICODE_RTL_DATABASE}
    for J := Low(CA) to High(CA) do
    begin
      C.C := CA[J];
      if I = -1 then
        raise EJclUnexpectedEOSequenceError.Create;
      IntegerHash.H1 := BytePermTable[IntegerHash.H1 xor C.H1];
      IntegerHash.H2 := BytePermTable[IntegerHash.H2 xor C.H2];
      IntegerHash.H3 := BytePermTable[IntegerHash.H3 xor C.H3];
      IntegerHash.H4 := BytePermTable[IntegerHash.H4 xor C.H4];
    end;
  end;
  Result := IntegerHash.H and MaxInt;
end;

// default is case-sensitive
function WideStrSimpleHashConvert(const AString: WideString): Integer;
var
  I: SizeInt;
  C, IntegerHash: TIntegerHash;
begin
  IntegerHash.H1 := 0;
  IntegerHash.H2 := 1;
  IntegerHash.H3 := 2;
  IntegerHash.H4 := 3;
  I := 1;
  while I < Length(AString) do
  begin
    C.C := UTF16GetNextChar(AString, I);
    if I = -1 then
      raise EJclUnexpectedEOSequenceError.Create;
    IntegerHash.H1 := BytePermTable[IntegerHash.H1 xor C.H1];
    IntegerHash.H2 := BytePermTable[IntegerHash.H2 xor C.H2];
    IntegerHash.H3 := BytePermTable[IntegerHash.H3 xor C.H3];
    IntegerHash.H4 := BytePermTable[IntegerHash.H4 xor C.H4];
  end;
  Result := IntegerHash.H and MaxInt;
end;

// case-insensitive
function WideStrSimpleHashConvertI(const AString: WideString): Integer;
var
  I: SizeInt;
  J: Integer;
  C, IntegerHash: TIntegerHash;
  CA: TUCS4Array;
begin
  IntegerHash.H1 := 0;
  IntegerHash.H2 := 1;
  IntegerHash.H3 := 2;
  IntegerHash.H4 := 3;
  {$IFDEF UNICODE_RTL_DATABASE}
  SetLength(CA, 1);
  {$ELSE ~UNICODE_RTL_DATABASE}
  SetLength(CA, 0);
  {$ENDIF ~UNICODE_RTL_DATABASE}
  I := 1;
  while I < Length(AString) do
  begin
    C.C := UTF16GetNextChar(AString, I);
    {$IFDEF UNICODE_RTL_DATABASE}
    CA[0] := Ord(TCharacter.ToLower(Chr(C.C)));
    {$ELSE ~UNICODE_RTL_DATABASE}
    CA := UnicodeCaseFold(C.C);
    {$ENDIF ~UNICODE_RTL_DATABASE}
    for J := Low(CA) to High(CA) do
    begin
      C.C := CA[J];
      if I = -1 then
        raise EJclUnexpectedEOSequenceError.Create;
      IntegerHash.H1 := BytePermTable[IntegerHash.H1 xor C.H1];
      IntegerHash.H2 := BytePermTable[IntegerHash.H2 xor C.H2];
      IntegerHash.H3 := BytePermTable[IntegerHash.H3 xor C.H3];
      IntegerHash.H4 := BytePermTable[IntegerHash.H4 xor C.H4];
    end;
  end;
  Result := IntegerHash.H and MaxInt;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
// default is case-sensitive
function UnicodeStrSimpleHashConvert(const AString: UnicodeString): Integer;
var
  I: SizeInt;
  C, IntegerHash: TIntegerHash;
begin
  IntegerHash.H1 := 0;
  IntegerHash.H2 := 1;
  IntegerHash.H3 := 2;
  IntegerHash.H4 := 3;
  I := 1;
  while I < Length(AString) do
  begin
    C.C := UTF16GetNextChar(AString, I);
    if I = -1 then
      raise EJclUnexpectedEOSequenceError.Create;
    IntegerHash.H1 := BytePermTable[IntegerHash.H1 xor C.H1];
    IntegerHash.H2 := BytePermTable[IntegerHash.H2 xor C.H2];
    IntegerHash.H3 := BytePermTable[IntegerHash.H3 xor C.H3];
    IntegerHash.H4 := BytePermTable[IntegerHash.H4 xor C.H4];
  end;
  Result := IntegerHash.H and MaxInt;
end;

// case-insensitive
function UnicodeStrSimpleHashConvertI(const AString: UnicodeString): Integer;
var
  I: SizeInt;
  J: Integer;
  C, IntegerHash: TIntegerHash;
  CA: TUCS4Array;
begin
  IntegerHash.H1 := 0;
  IntegerHash.H2 := 1;
  IntegerHash.H3 := 2;
  IntegerHash.H4 := 3;
  {$IFDEF UNICODE_RTL_DATABASE}
  SetLength(CA, 1);
  {$ELSE ~UNICODE_RTL_DATABASE}
  SetLength(CA, 0);
  {$ENDIF ~UNICODE_RTL_DATABASE}
  I := 1;
  while I < Length(AString) do
  begin
    C.C := UTF16GetNextChar(AString, I);
    {$IFDEF UNICODE_RTL_DATABASE}
    CA[0] := Ord(TCharacter.ToLower(Chr(C.C)));
    {$ELSE ~UNICODE_RTL_DATABASE}
    CA := UnicodeCaseFold(C.C);
    {$ENDIF ~UNICODE_RTL_DATABASE}
    for J := Low(CA) to High(CA) do
    begin
      C.C := CA[J];
      if I = -1 then
        raise EJclUnexpectedEOSequenceError.Create;
      IntegerHash.H1 := BytePermTable[IntegerHash.H1 xor C.H1];
      IntegerHash.H2 := BytePermTable[IntegerHash.H2 xor C.H2];
      IntegerHash.H3 := BytePermTable[IntegerHash.H3 xor C.H3];
      IntegerHash.H4 := BytePermTable[IntegerHash.H4 xor C.H4];
    end;
  end;
  Result := IntegerHash.H and MaxInt;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

function StrSimpleHashConvert(const AString: string): Integer;
begin
  {$IFDEF SUPPORTS_UNICODE}
  {$IFDEF SUPPORTS_UNICODE_STRING}
  Result := UnicodeStrSimpleHashConvert(AString);
  {$ELSE ~SUPPORTS_UNICODE_STRING}
  Result := WideStrSimpleHashConvert(AString);
  {$ENDIF ~SUPPORTS_UNICODE_STRING}
  {$ELSE ~SUPPORTS_UNICODE}
  Result := AnsiStrSimpleHashConvert(AString);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

function StrSimpleHashConvertI(const AString: string): Integer;
begin
  {$IFDEF SUPPORTS_UNICODE}
  {$IFDEF SUPPORTS_UNICODE_STRING}
  Result := UnicodeStrSimpleHashConvertI(AString);
  {$ELSE ~SUPPORTS_UNICODE_STRING}
  Result := WideStrSimpleHashConvertI(AString);
  {$ENDIF ~SUPPORTS_UNICODE_STRING}
  {$ELSE ~SUPPORTS_UNICODE}
  Result := AnsiStrSimpleHashConvertI(AString);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

function SingleSimpleHashConvert(const AValue: Single): Integer;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Round(MaxInt * Frac(AValue * A));
end;

function DoubleSimpleHashConvert(const AValue: Double): Integer;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Round(MaxInt * Frac(AValue * A));
end;

function ExtendedSimpleHashConvert(const AValue: Extended): Integer;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Round(MaxInt * Frac(AValue * A));
end;

function FloatSimpleHashConvert(const AValue: Float): Integer;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Round(MaxInt * Frac(AValue * A));
end;

function IntegerSimpleHashConvert(AValue: Integer): Integer;
begin
  Result := AValue and MaxInt;
end;

function CardinalSimpleHashConvert(AValue: Cardinal): Integer;
begin
  Result := AValue and MaxInt;
end;

function Int64SimpleHashConvert(const AValue: Int64): Integer;
begin
  Result := (AValue xor (AValue shr 32)) and MaxInt;
end;

function PtrSimpleHashConvert(APtr: Pointer): Integer;
begin
  Result := SizeInt(APtr) and MaxInt;
end;

function SimpleHashConvert(AObject: TObject): Integer;
begin
  Result := SizeInt(AObject) and MaxInt;
end;

function JclSimpleHashToRange(Key, Range: Integer): Integer;
// return a value between 0 and (Range-1) based on integer-hash Key
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(Range * (Frac(Abs(Key * A))));
end;

procedure FinalizeArrayBeforeMove(var List: TDynIInterfaceArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure InitializeArray(var List: TDynIInterfaceArray; FromIndex, Count: SizeInt); overload;
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

procedure InitializeArrayAfterMove(var List: TDynIInterfaceArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure MoveArray(var List: TDynIInterfaceArray; FromIndex, ToIndex, Count: SizeInt); overload;
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

procedure FinalizeArrayBeforeMove(var List: TDynAnsiStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure InitializeArray(var List: TDynAnsiStringArray; FromIndex, Count: SizeInt); overload;
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

procedure InitializeArrayAfterMove(var List: TDynAnsiStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure MoveArray(var List: TDynAnsiStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

procedure FinalizeArrayBeforeMove(var List: TDynWideStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure InitializeArray(var List: TDynWideStringArray; FromIndex, Count: SizeInt); overload;
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

procedure InitializeArrayAfterMove(var List: TDynWideStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure MoveArray(var List: TDynWideStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}

procedure FinalizeArrayBeforeMove(var List: TDynUnicodeStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure InitializeArray(var List: TDynUnicodeStringArray; FromIndex, Count: SizeInt); overload;
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

procedure InitializeArrayAfterMove(var List: TDynUnicodeStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure MoveArray(var List: TDynUnicodeStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

procedure InitializeArrayAfterMove(var List: TDynSingleArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure MoveArray(var List: TDynSingleArray; FromIndex, ToIndex, Count: SizeInt); overload;
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

procedure InitializeArrayAfterMove(var List: TDynDoubleArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure MoveArray(var List: TDynDoubleArray; FromIndex, ToIndex, Count: SizeInt); overload;
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

procedure InitializeArrayAfterMove(var List: TDynExtendedArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure MoveArray(var List: TDynExtendedArray; FromIndex, ToIndex, Count: SizeInt); overload;
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

procedure InitializeArrayAfterMove(var List: TDynIntegerArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure MoveArray(var List: TDynIntegerArray; FromIndex, ToIndex, Count: SizeInt); overload;
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

procedure InitializeArrayAfterMove(var List: TDynCardinalArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure MoveArray(var List: TDynCardinalArray; FromIndex, ToIndex, Count: SizeInt); overload;
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

procedure InitializeArrayAfterMove(var List: TDynInt64Array; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure MoveArray(var List: TDynInt64Array; FromIndex, ToIndex, Count: SizeInt); overload;
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

procedure InitializeArrayAfterMove(var List: TDynPointerArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure MoveArray(var List: TDynPointerArray; FromIndex, ToIndex, Count: SizeInt); overload;
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

procedure InitializeArrayAfterMove(var List: TDynObjectArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure MoveArray(var List: TDynObjectArray; FromIndex, ToIndex, Count: SizeInt); overload;
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;


procedure InitializeArrayAfterMove(var List: TDynSizeIntArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure MoveArray(var List: TDynSizeIntArray; FromIndex, ToIndex, Count: SizeInt); overload;
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

{$IFNDEF FPC}
procedure FinalizeArrayBeforeMove(var List: TDynStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure InitializeArray(var List: TDynStringArray; FromIndex, Count: SizeInt); overload;
begin

  Initialize(List[FromIndex], Count);
end;

procedure InitializeArrayAfterMove(var List: TDynStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure MoveArray(var List: TDynStringArray; FromIndex, ToIndex, Count: SizeInt); overload;
begin
  if Count > 0 then
  begin
    FinalizeArrayBeforeMove(List, FromIndex, ToIndex, Count);
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;

procedure InitializeArrayAfterMove(var List: TDynFloatArray; FromIndex, ToIndex, Count: SizeInt); overload;
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

procedure MoveArray(var List: TDynFloatArray; FromIndex, ToIndex, Count: SizeInt); overload;
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    InitializeArrayAfterMove(List, FromIndex, ToIndex, Count);
  end;
end;
{$ENDIF ~FPC}

procedure Iterate(const First: IJclIntfIterator; Count: Integer; F: TIntfIterateProcedure);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      F(First.Next)
    else
      Break;
end;

procedure Iterate(const First: IJclAnsiStrIterator; Count: Integer; F: TAnsiStrIterateProcedure);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      F(First.Next)
    else
      Break;
end;

procedure Iterate(const First: IJclWideStrIterator; Count: Integer; F: TWideStrIterateProcedure);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      F(First.Next)
    else
      Break;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
procedure Iterate(const First: IJclUnicodeStrIterator; Count: Integer; F: TUnicodeStrIterateProcedure);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      F(First.Next)
    else
      Break;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

procedure Iterate(const First: IJclSingleIterator; Count: Integer; F: TSingleIterateProcedure);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      F(First.Next)
    else
      Break;
end;

procedure Iterate(const First: IJclDoubleIterator; Count: Integer; F: TDoubleIterateProcedure);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      F(First.Next)
    else
      Break;
end;

procedure Iterate(const First: IJclExtendedIterator; Count: Integer; F: TExtendedIterateProcedure);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      F(First.Next)
    else
      Break;
end;

procedure Iterate(const First: IJclIntegerIterator; Count: Integer; F: TIntegerIterateProcedure);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      F(First.Next)
    else
      Break;
end;

procedure Iterate(const First: IJclCardinalIterator; Count: Integer; F: TCardinalIterateProcedure);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      F(First.Next)
    else
      Break;
end;

procedure Iterate(const First: IJclInt64Iterator; Count: Integer; F: TInt64IterateProcedure);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      F(First.Next)
    else
      Break;
end;

procedure Iterate(const First: IJclPtrIterator; Count: Integer; F: TPtrIterateProcedure);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      F(First.Next)
    else
      Break;
end;

procedure Iterate(const First: IJclIterator; Count: Integer; F: TIterateProcedure);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      F(First.Next)
    else
      Break;
end;


procedure Apply(const First: IJclIntfIterator; Count: Integer; F: TIntfApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetObject(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclAnsiStrIterator; Count: Integer; F: TAnsiStrApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetString(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclWideStrIterator; Count: Integer; F: TWideStrApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetString(F(First.Next))
    else
      Break;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
procedure Apply(const First: IJclUnicodeStrIterator; Count: Integer; F: TUnicodeStrApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetString(F(First.Next))
    else
      Break;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

procedure Apply(const First: IJclSingleIterator; Count: Integer; F: TSingleApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetValue(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclDoubleIterator; Count: Integer; F: TDoubleApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetValue(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclExtendedIterator; Count: Integer; F: TExtendedApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetValue(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclIntegerIterator; Count: Integer; F: TIntegerApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetValue(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclCardinalIterator; Count: Integer; F: TCardinalApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetValue(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclInt64Iterator; Count: Integer; F: TInt64ApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetValue(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclPtrIterator; Count: Integer; F: TPtrApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetPointer(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclIterator; Count: Integer; F: TApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetObject(F(First.Next))
    else
      Break;
end;


function Find(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AComparator: TIntfCompare): IJclIntfIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AInterface) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AEqualityComparator: TIntfEqualityCompare): IJclIntfIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AInterface) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString; AComparator: TAnsiStrCompare): IJclAnsiStrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AString) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString; AEqualityComparator: TAnsiStrEqualityCompare): IJclAnsiStrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AString) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString; AComparator: TWideStrCompare): IJclWideStrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AString) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString; AEqualityComparator: TWideStrEqualityCompare): IJclWideStrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AString) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
function Find(const First: IJclUnicodeStrIterator; Count: Integer;
  const AString: UnicodeString; AComparator: TUnicodeStrCompare): IJclUnicodeStrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AString) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF SUPPORTS_UNICODE_STRING}
function Find(const First: IJclUnicodeStrIterator; Count: Integer;
  const AString: UnicodeString; AEqualityComparator: TUnicodeStrEqualityCompare): IJclUnicodeStrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AString) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

function Find(const First: IJclSingleIterator; Count: Integer;
  const AValue: Single; AComparator: TSingleCompare): IJclSingleIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AValue) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclSingleIterator; Count: Integer;
  const AValue: Single; AEqualityComparator: TSingleEqualityCompare): IJclSingleIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AValue) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclDoubleIterator; Count: Integer;
  const AValue: Double; AComparator: TDoubleCompare): IJclDoubleIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AValue) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclDoubleIterator; Count: Integer;
  const AValue: Double; AEqualityComparator: TDoubleEqualityCompare): IJclDoubleIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AValue) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclExtendedIterator; Count: Integer;
  const AValue: Extended; AComparator: TExtendedCompare): IJclExtendedIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AValue) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclExtendedIterator; Count: Integer;
  const AValue: Extended; AEqualityComparator: TExtendedEqualityCompare): IJclExtendedIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AValue) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclIntegerIterator; Count: Integer;
  AValue: Integer; AComparator: TIntegerCompare): IJclIntegerIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AValue) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclIntegerIterator; Count: Integer;
  AValue: Integer; AEqualityComparator: TIntegerEqualityCompare): IJclIntegerIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AValue) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclCardinalIterator; Count: Integer;
  AValue: Cardinal; AComparator: TCardinalCompare): IJclCardinalIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AValue) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclCardinalIterator; Count: Integer;
  AValue: Cardinal; AEqualityComparator: TCardinalEqualityCompare): IJclCardinalIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AValue) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclInt64Iterator; Count: Integer;
  const AValue: Int64; AComparator: TInt64Compare): IJclInt64Iterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AValue) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclInt64Iterator; Count: Integer;
  const AValue: Int64; AEqualityComparator: TInt64EqualityCompare): IJclInt64Iterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AValue) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclPtrIterator; Count: Integer;
  APtr: Pointer; AComparator: TPtrCompare): IJclPtrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, APtr) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclPtrIterator; Count: Integer;
  APtr: Pointer; AEqualityComparator: TPtrEqualityCompare): IJclPtrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, APtr) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclIterator; Count: Integer;
  AObject: TObject; AComparator: TCompare): IJclIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AObject) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclIterator; Count: Integer;
  AObject: TObject; AEqualityComparator: TEqualityCompare): IJclIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AObject) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;


function CountObject(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AComparator: TIntfCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AInterface) = 0))
    else
      Break;
end;

function CountObject(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AEqualityComparator: TIntfEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AInterface)))
    else
      Break;
end;

function CountObject(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString; AComparator: TAnsiStrCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AString) = 0))
    else
      Break;
end;

function CountObject(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString; AEqualityComparator: TAnsiStrEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AString)))
    else
      Break;
end;

function CountObject(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString; AComparator: TWideStrCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AString) = 0))
    else
      Break;
end;

function CountObject(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString; AEqualityComparator: TWideStrEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AString)))
    else
      Break;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
function CountObject(const First: IJclUnicodeStrIterator; Count: Integer;
  const AString: UnicodeString; AComparator: TUnicodeStrCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AString) = 0))
    else
      Break;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF SUPPORTS_UNICODE_STRING}
function CountObject(const First: IJclUnicodeStrIterator; Count: Integer;
  const AString: UnicodeString; AEqualityComparator: TUnicodeStrEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AString)))
    else
      Break;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

function CountObject(const First: IJclSingleIterator; Count: Integer;
  const AValue: Single; AComparator: TSingleCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AValue) = 0))
    else
      Break;
end;

function CountObject(const First: IJclSingleIterator; Count: Integer;
  const AValue: Single; AEqualityComparator: TSingleEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AValue)))
    else
      Break;
end;

function CountObject(const First: IJclDoubleIterator; Count: Integer;
  const AValue: Double; AComparator: TDoubleCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AValue) = 0))
    else
      Break;
end;

function CountObject(const First: IJclDoubleIterator; Count: Integer;
  const AValue: Double; AEqualityComparator: TDoubleEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AValue)))
    else
      Break;
end;

function CountObject(const First: IJclExtendedIterator; Count: Integer;
  const AValue: Extended; AComparator: TExtendedCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AValue) = 0))
    else
      Break;
end;

function CountObject(const First: IJclExtendedIterator; Count: Integer;
  const AValue: Extended; AEqualityComparator: TExtendedEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AValue)))
    else
      Break;
end;

function CountObject(const First: IJclIntegerIterator; Count: Integer;
  AValue: Integer; AComparator: TIntegerCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AValue) = 0))
    else
      Break;
end;

function CountObject(const First: IJclIntegerIterator; Count: Integer;
  AValue: Integer; AEqualityComparator: TIntegerEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AValue)))
    else
      Break;
end;

function CountObject(const First: IJclCardinalIterator; Count: Integer;
  AValue: Cardinal; AComparator: TCardinalCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AValue) = 0))
    else
      Break;
end;

function CountObject(const First: IJclCardinalIterator; Count: Integer;
  AValue: Cardinal; AEqualityComparator: TCardinalEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AValue)))
    else
      Break;
end;

function CountObject(const First: IJclInt64Iterator; Count: Integer;
  const AValue: Int64; AComparator: TInt64Compare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AValue) = 0))
    else
      Break;
end;

function CountObject(const First: IJclInt64Iterator; Count: Integer;
  const AValue: Int64; AEqualityComparator: TInt64EqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AValue)))
    else
      Break;
end;

function CountObject(const First: IJclPtrIterator; Count: Integer;
  APtr: Pointer; AComparator: TPtrCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, APtr) = 0))
    else
      Break;
end;

function CountObject(const First: IJclPtrIterator; Count: Integer;
  APtr: Pointer; AEqualityComparator: TPtrEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, APtr)))
    else
      Break;
end;

function CountObject(const First: IJclIterator; Count: Integer;
  AObject: TObject; AComparator: TCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AObject) = 0))
    else
      Break;
end;

function CountObject(const First: IJclIterator; Count: Integer;
  AObject: TObject; AEqualityComparator: TEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AObject)))
    else
      Break;
end;


procedure Copy(const First: IJclIntfIterator; Count: Integer;
  const Output: IJclIntfIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetObject(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclAnsiStrIterator; Count: Integer;
  const Output: IJclAnsiStrIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetString(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclWideStrIterator; Count: Integer;
  const Output: IJclWideStrIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetString(First.Next);
    end
    else
      Break;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
procedure Copy(const First: IJclUnicodeStrIterator; Count: Integer;
  const Output: IJclUnicodeStrIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetString(First.Next);
    end
    else
      Break;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

procedure Copy(const First: IJclSingleIterator; Count: Integer;
  const Output: IJclSingleIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetValue(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclDoubleIterator; Count: Integer;
  const Output: IJclDoubleIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetValue(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclExtendedIterator; Count: Integer;
  const Output: IJclExtendedIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetValue(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclIntegerIterator; Count: Integer;
  const Output: IJclIntegerIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetValue(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclCardinalIterator; Count: Integer;
  const Output: IJclCardinalIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetValue(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclInt64Iterator; Count: Integer;
  const Output: IJclInt64Iterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetValue(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclPtrIterator; Count: Integer;
  const Output: IJclPtrIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetPointer(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclIterator; Count: Integer;
  const Output: IJclIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetObject(First.Next);
    end
    else
      Break;
end;


procedure Generate(const List: IJclIntfList; Count: Integer;
  const AInterface: IInterface);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AInterface);
end;

procedure Generate(const List: IJclAnsiStrList; Count: Integer;
  const AString: AnsiString);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AString);
end;

procedure Generate(const List: IJclWideStrList; Count: Integer;
  const AString: WideString);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AString);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
procedure Generate(const List: IJclUnicodeStrList; Count: Integer;
  const AString: UnicodeString);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AString);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

procedure Generate(const List: IJclSingleList; Count: Integer;
  const AValue: Single);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AValue);
end;

procedure Generate(const List: IJclDoubleList; Count: Integer;
  const AValue: Double);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AValue);
end;

procedure Generate(const List: IJclExtendedList; Count: Integer;
  const AValue: Extended);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AValue);
end;

procedure Generate(const List: IJclIntegerList; Count: Integer;
  AValue: Integer);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AValue);
end;

procedure Generate(const List: IJclCardinalList; Count: Integer;
  AValue: Cardinal);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AValue);
end;

procedure Generate(const List: IJclInt64List; Count: Integer;
  const AValue: Int64);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AValue);
end;

procedure Generate(const List: IJclPtrList; Count: Integer;
  APtr: Pointer);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(APtr);
end;

procedure Generate(const List: IJclList; Count: Integer;
  AObject: TObject);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AObject);
end;


procedure Fill(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetObject(AInterface);
    end
    else
      Break;
end;

procedure Fill(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetString(AString);
    end
    else
      Break;
end;

procedure Fill(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetString(AString);
    end
    else
      Break;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
procedure Fill(const First: IJclUnicodeStrIterator; Count: Integer;
  const AString: UnicodeString);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetString(AString);
    end
    else
      Break;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

procedure Fill(const First: IJclSingleIterator; Count: Integer;
  const AValue: Single);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetValue(AValue);
    end
    else
      Break;
end;

procedure Fill(const First: IJclDoubleIterator; Count: Integer;
  const AValue: Double);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetValue(AValue);
    end
    else
      Break;
end;

procedure Fill(const First: IJclExtendedIterator; Count: Integer;
  const AValue: Extended);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetValue(AValue);
    end
    else
      Break;
end;

procedure Fill(const First: IJclIntegerIterator; Count: Integer;
  AValue: Integer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetValue(AValue);
    end
    else
      Break;
end;

procedure Fill(const First: IJclCardinalIterator; Count: Integer;
  AValue: Cardinal);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetValue(AValue);
    end
    else
      Break;
end;

procedure Fill(const First: IJclInt64Iterator; Count: Integer;
  const AValue: Int64);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetValue(AValue);
    end
    else
      Break;
end;

procedure Fill(const First: IJclPtrIterator; Count: Integer;
  APtr: Pointer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetPointer(APtr);
    end
    else
      Break;
end;

procedure Fill(const First: IJclIterator; Count: Integer;
  AObject: TObject);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetObject(AObject);
    end
    else
      Break;
end;


procedure Reverse(const First, Last: IJclIntfIterator);
var
  Obj: IInterface;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetObject(Last.GetObject);
    Last.SetObject(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclAnsiStrIterator);
var
  Obj: AnsiString;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetString(Last.GetString);
    Last.SetString(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclWideStrIterator);
var
  Obj: WideString;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetString(Last.GetString);
    Last.SetString(Obj);
  end;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
procedure Reverse(const First, Last: IJclUnicodeStrIterator);
var
  Obj: UnicodeString;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetString(Last.GetString);
    Last.SetString(Obj);
  end;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

procedure Reverse(const First, Last: IJclSingleIterator);
var
  Obj: Single;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetValue(Last.GetValue);
    Last.SetValue(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclDoubleIterator);
var
  Obj: Double;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetValue(Last.GetValue);
    Last.SetValue(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclExtendedIterator);
var
  Obj: Extended;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetValue(Last.GetValue);
    Last.SetValue(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclIntegerIterator);
var
  Obj: Integer;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetValue(Last.GetValue);
    Last.SetValue(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclCardinalIterator);
var
  Obj: Cardinal;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetValue(Last.GetValue);
    Last.SetValue(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclInt64Iterator);
var
  Obj: Int64;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetValue(Last.GetValue);
    Last.SetValue(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclPtrIterator);
var
  Obj: Pointer;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetPointer(Last.GetPointer);
    Last.SetPointer(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclIterator);
var
  Obj: TObject;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetObject(Last.GetObject);
    Last.SetObject(Obj);
  end;
end;


procedure QuickSort(const AList: IJclIntfList; L, R: Integer;
  AComparator: TIntfCompare);
var
  I, J, P: Integer;
  Obj: IInterface;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetObject(P);
      while AComparator(AList.GetObject(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetObject(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Obj := AList.GetObject(I);
          AList.SetObject(I, AList.GetObject(J));
          AList.SetObject(J, Obj);
        end;
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclAnsiStrList; L, R: Integer;
  AComparator: TAnsiStrCompare);
var
  I, J, P: Integer;
  Obj: AnsiString;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetString(P);
      while AComparator(AList.GetString(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetString(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Obj := AList.GetString(I);
          AList.SetString(I, AList.GetString(J));
          AList.SetString(J, Obj);
        end;
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclWideStrList; L, R: Integer;
  AComparator: TWideStrCompare);
var
  I, J, P: Integer;
  Obj: WideString;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetString(P);
      while AComparator(AList.GetString(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetString(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Obj := AList.GetString(I);
          AList.SetString(I, AList.GetString(J));
          AList.SetString(J, Obj);
        end;
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
procedure QuickSort(const AList: IJclUnicodeStrList; L, R: Integer;
  AComparator: TUnicodeStrCompare);
var
  I, J, P: Integer;
  Obj: UnicodeString;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetString(P);
      while AComparator(AList.GetString(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetString(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Obj := AList.GetString(I);
          AList.SetString(I, AList.GetString(J));
          AList.SetString(J, Obj);
        end;
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

procedure QuickSort(const AList: IJclSingleList; L, R: Integer;
  AComparator: TSingleCompare);
var
  I, J, P: Integer;
  Obj: Single;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetValue(P);
      while AComparator(AList.GetValue(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetValue(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Obj := AList.GetValue(I);
          AList.SetValue(I, AList.GetValue(J));
          AList.SetValue(J, Obj);
        end;
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclDoubleList; L, R: Integer;
  AComparator: TDoubleCompare);
var
  I, J, P: Integer;
  Obj: Double;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetValue(P);
      while AComparator(AList.GetValue(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetValue(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Obj := AList.GetValue(I);
          AList.SetValue(I, AList.GetValue(J));
          AList.SetValue(J, Obj);
        end;
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclExtendedList; L, R: Integer;
  AComparator: TExtendedCompare);
var
  I, J, P: Integer;
  Obj: Extended;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetValue(P);
      while AComparator(AList.GetValue(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetValue(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Obj := AList.GetValue(I);
          AList.SetValue(I, AList.GetValue(J));
          AList.SetValue(J, Obj);
        end;
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclIntegerList; L, R: Integer;
  AComparator: TIntegerCompare);
var
  I, J, P: Integer;
  Obj: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetValue(P);
      while AComparator(AList.GetValue(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetValue(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Obj := AList.GetValue(I);
          AList.SetValue(I, AList.GetValue(J));
          AList.SetValue(J, Obj);
        end;
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclCardinalList; L, R: Integer;
  AComparator: TCardinalCompare);
var
  I, J, P: Integer;
  Obj: Cardinal;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetValue(P);
      while AComparator(AList.GetValue(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetValue(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Obj := AList.GetValue(I);
          AList.SetValue(I, AList.GetValue(J));
          AList.SetValue(J, Obj);
        end;
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclInt64List; L, R: Integer;
  AComparator: TInt64Compare);
var
  I, J, P: Integer;
  Obj: Int64;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetValue(P);
      while AComparator(AList.GetValue(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetValue(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Obj := AList.GetValue(I);
          AList.SetValue(I, AList.GetValue(J));
          AList.SetValue(J, Obj);
        end;
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclPtrList; L, R: Integer;
  AComparator: TPtrCompare);
var
  I, J, P: Integer;
  Obj: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetPointer(P);
      while AComparator(AList.GetPointer(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetPointer(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Obj := AList.GetPointer(I);
          AList.SetPointer(I, AList.GetPointer(J));
          AList.SetPointer(J, Obj);
        end;
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclList; L, R: Integer;
  AComparator: TCompare);
var
  I, J, P: Integer;
  Obj: TObject;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetObject(P);
      while AComparator(AList.GetObject(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetObject(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Obj := AList.GetObject(I);
          AList.SetObject(I, AList.GetObject(J));
          AList.SetObject(J, Obj);
        end;
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;


procedure Sort(const AList: IJclIntfList; First, Last: Integer; AComparator: TIntfCompare);
begin
  IntfSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclAnsiStrList; First, Last: Integer; AComparator: TAnsiStrCompare);
begin
  AnsiStrSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclWideStrList; First, Last: Integer; AComparator: TWideStrCompare);
begin
  WideStrSortProc(AList, First, Last, AComparator);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
procedure Sort(const AList: IJclUnicodeStrList; First, Last: Integer; AComparator: TUnicodeStrCompare);
begin
  UnicodeStrSortProc(AList, First, Last, AComparator);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

procedure Sort(const AList: IJclSingleList; First, Last: Integer; AComparator: TSingleCompare);
begin
  SingleSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclDoubleList; First, Last: Integer; AComparator: TDoubleCompare);
begin
  DoubleSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclExtendedList; First, Last: Integer; AComparator: TExtendedCompare);
begin
  ExtendedSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclIntegerList; First, Last: Integer; AComparator: TIntegerCompare);
begin
  IntegerSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclCardinalList; First, Last: Integer; AComparator: TCardinalCompare);
begin
  CardinalSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclInt64List; First, Last: Integer; AComparator: TInt64Compare);
begin
  Int64SortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclPtrList; First, Last: Integer; AComparator: TPtrCompare);
begin
  PtrSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclList; First, Last: Integer; AComparator: TCompare);
begin
  SortProc(AList, First, Last, AComparator);
end;

{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

class procedure TJclAlgorithms<T>.Iterate(const First: IJclIterator<T>; Count: Integer; F: TIterateProcedure<T>);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      F(First.Next)
    else
      Break;
end;

class procedure TJclAlgorithms<T>.Apply(const First: IJclIterator<T>; Count: Integer; F: TApplyFunction<T>);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetItem(F(First.Next))
    else
      Break;
end;

class function TJclAlgorithms<T>.Find(const First: IJclIterator<T>; Count: Integer;
  const AItem: T; AComparator: TCompare<T>): IJclIterator<T>;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AItem) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

class function TJclAlgorithms<T>.Find(const First: IJclIterator<T>; Count: Integer;
  const AItem: T; AEqualityComparator: TEqualityCompare<T>): IJclIterator<T>;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AItem) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

class function TJclAlgorithms<T>.CountObject(const First: IJclIterator<T>; Count: Integer;
  const AItem: T; AComparator: TCompare<T>): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AItem) = 0))
    else
      Break;
end;

class function TJclAlgorithms<T>.CountObject(const First: IJclIterator<T>; Count: Integer;
  const AItem: T; AEqualityComparator: TEqualityCompare<T>): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AItem)))
    else
      Break;
end;

class procedure TJclAlgorithms<T>.Copy(const First: IJclIterator<T>; Count: Integer;
  const Output: IJclIterator<T>);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetItem(First.Next);
    end
    else
      Break;
end;

class procedure TJclAlgorithms<T>.Generate(const List: IJclList<T>; Count: Integer;
  const AItem: T);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AItem);
end;

class procedure TJclAlgorithms<T>.Fill(const First: IJclIterator<T>; Count: Integer;
  const AItem: T);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetItem(AItem);
    end
    else
      Break;
end;

class procedure TJclAlgorithms<T>.Reverse(const First, Last: IJclIterator<T>);
var
  Obj: T;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetItem(Last.GetItem);
    Last.SetItem(Obj);
  end;
end;

class procedure TJclAlgorithms<T>.QuickSort(const AList: IJclList<T>; L, R: Integer;
  AComparator: TCompare<T>);
var
  I, J, P: Integer;
  Obj: T;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetItem(P);
      while AComparator(AList.GetItem(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetItem(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Obj := AList.GetItem(I);
          AList.SetItem(I, AList.GetItem(J));
          AList.SetItem(J, Obj);
        end;
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      TJclAlgorithms<T>.QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

class procedure TJclAlgorithms<T>.Sort(const AList: IJclList<T>; First, Last: Integer;
  AComparator: TCompare<T>);
begin
  TJclAlgorithms<T>.QuickSort(AList, First, Last, AComparator);
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