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

{$I containers\JclAlgorithms.int}
{$I containers\JclAlgorithms.imp}
interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase, JclContainerIntf;

// Compare functions
(*$JPPLOOP ALLTYPEINDEX ALLTYPECOUNT
{$JPPEXPANDMACRO SIMPLECOMPAREINT(,,)}
*)
{$JPPEXPANDMACRO SIMPLECOMPAREINT(IntegerCompare,,TObject)}

function AnsiStrSimpleCompareI(const Obj1, Obj2: AnsiString): Integer;
function WideStrSimpleCompareI(const Obj1, Obj2: WideString): Integer;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UnicodeStrSimpleCompareI(const Obj1, Obj2: UnicodeString): Integer;
{$ENDIF SUPPORTS_UNICODE_STRING}
function StrSimpleCompareI(const Obj1, Obj2: string): Integer;

// Compare functions for equality
(*$JPPLOOP ALLTYPEINDEX ALLTYPECOUNT
{$JPPEXPANDMACRO SIMPLEEQUALITYCOMPAREINT(,,)}
*)
function AnsiStrSimpleEqualityCompareI(const Obj1, Obj2: AnsiString): Boolean;
function WideStrSimpleEqualityCompareI(const Obj1, Obj2: WideString): Boolean;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UnicodeStrSimpleEqualityCompareI(const Obj1, Obj2: UnicodeString): Boolean;
{$ENDIF SUPPORTS_UNICODE_STRING}
function StrSimpleEqualityCompareI(const Obj1, Obj2: string): Boolean;

// Hash conversion functions
(*$JPPLOOP ALLTYPEINDEX ALLTYPECOUNT
{$JPPEXPANDMACRO SIMPLEHASHCONVERTINT(,,,)}
*)
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
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO MOVEARRAYINT(,, overload;)}
*)
{$JPPEXPANDMACRO MOVEARRAYINT(MoveArray,TDynSizeIntArray, overload;)}
{$IFNDEF FPC}
{$JPPEXPANDMACRO MOVEARRAYINT(MoveArray,TDynStringArray, overload;)}
{$JPPEXPANDMACRO MOVEARRAYINT(MoveArray,TDynFloatArray, overload;)}
{$ENDIF ~FPC}

// Iterate algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO ITERATEINT(,,, overload;)}
*)

// Apply algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO APPLYINT(,,, overload;)}
*)

// Find algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO FINDINT(,,,,,, overload;)}
{$JPPEXPANDMACRO FINDEQINT(,,,,,, overload;)}
*)

// CountObject algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO COUNTOBJECTINT(,,,,,, overload;)}
{$JPPEXPANDMACRO COUNTOBJECTEQINT(,,,,,, overload;)}
*)

// Copy algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO COPYINT(,, overload;)}
*)

// Generate algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO GENERATEINT(,,,,, overload;)}
*)

// Fill algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO FILLINT(,,,,, overload;)}
*)

// Reverse algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO REVERSEINT(,, overload;)}
*)

(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO QUICKSORTINT(,,,,, overload;)}
*)

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
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO SORTINT(,,First,Last,, overload;)}
*)

{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

type
  // cannot implement generic global functions
  TJclAlgorithms<T> = class
  private
    //FSortProc: TSortProc;
  public
    class {$JPPEXPANDMACRO ITERATEINT(Iterate,IJclIterator<T>,TIterateProcedure<T>,)}
    class {$JPPEXPANDMACRO APPLYINT(Apply,IJclIterator<T>,TApplyFunction<T>,)}
    class {$JPPEXPANDMACRO FINDINT(Find,IJclIterator<T>,const ,AItem,TCompare<T>,T, overload;)}
    class {$JPPEXPANDMACRO FINDEQINT(Find,IJclIterator<T>,const ,AItem,TEqualityCompare<T>,T, overload;)}
    class {$JPPEXPANDMACRO COUNTOBJECTINT(CountObject,IJclIterator<T>,const ,AItem,TCompare<T>,T, overload;)}
    class {$JPPEXPANDMACRO COUNTOBJECTEQINT(CountObject,IJclIterator<T>,const ,AItem,TEqualityCompare<T>,T, overload;)}
    class {$JPPEXPANDMACRO COPYINT(Copy,IJclIterator<T>,)}
    class {$JPPEXPANDMACRO GENERATEINT(Generate,IJclList<T>,const ,AItem,T,)}
    class {$JPPEXPANDMACRO FILLINT(Fill,IJclIterator<T>,const ,AItem,T,)}
    class {$JPPEXPANDMACRO REVERSEINT(Reverse,IJclIterator<T>,)}
    class {$JPPEXPANDMACRO SORTINT(QuickSort,IJclList<T>,L,R,TCompare<T>,)}
    class {$JPPEXPANDMACRO SORTINT(Sort,IJclList<T>,First,Last,TCompare<T>,)}
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

(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO MOVEARRAYIMP(,,,, overload;)}

*)
{$JPPUNDEF GENERIC}{$JPPUNDEF REFCOUNTED}{$JPPDEFINE ZEROINIT}{$JPPEXPANDMACRO MOVEARRAYIMP(MoveArray,TDynSizeIntArray,,, overload;)}

{$IFNDEF FPC}
{$JPPUNDEF GENERIC}{$JPPDEFINE REFCOUNTED}{$JPPUNDEF ZEROINIT}{$JPPEXPANDMACRO MOVEARRAYIMP(MoveArray,TDynStringArray,,, overload;)}

{$JPPUNDEF GENERIC}{$JPPUNDEF REFCOUNTED}{$JPPDEFINE ZEROINIT}{$JPPEXPANDMACRO MOVEARRAYIMP(MoveArray,TDynFloatArray,,, overload;)}
{$ENDIF ~FPC}

(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO ITERATEIMP(,,)}

*)
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO APPLYIMP(,,,)}

*)
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO FINDIMP(,,,,,)}

{$JPPEXPANDMACRO FINDEQIMP(,,,,,)}

*)
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO COUNTOBJECTIMP(,,,,,)}

{$JPPEXPANDMACRO COUNTOBJECTEQIMP(,,,,,)}

*)
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO COPYIMP(,,)}

*)
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO GENERATEIMP(,,,,)}

*)
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO FILLIMP(,,,,,)}

*)
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO REVERSEIMP(,,,,)}

*)
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO QUICKSORTIMP(,,,,,,,)}

*)
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

class {$JPPEXPANDMACRO ITERATEIMP(TJclAlgorithms<T>.Iterate,IJclIterator<T>,TIterateProcedure<T>)}

class {$JPPEXPANDMACRO APPLYIMP(TJclAlgorithms<T>.Apply,IJclIterator<T>,TApplyFunction<T>,SetItem)}

class {$JPPEXPANDMACRO FINDIMP(TJclAlgorithms<T>.Find,IJclIterator<T>,const ,AItem,TCompare<T>,T)}

class {$JPPEXPANDMACRO FINDEQIMP(TJclAlgorithms<T>.Find,IJclIterator<T>,const ,AItem,TEqualityCompare<T>,T)}

class {$JPPEXPANDMACRO COUNTOBJECTIMP(TJclAlgorithms<T>.CountObject,IJclIterator<T>,const ,AItem,TCompare<T>,T)}

class {$JPPEXPANDMACRO COUNTOBJECTEQIMP(TJclAlgorithms<T>.CountObject,IJclIterator<T>,const ,AItem,TEqualityCompare<T>,T)}

class {$JPPEXPANDMACRO COPYIMP(TJclAlgorithms<T>.Copy,IJclIterator<T>,SetItem)}

class {$JPPEXPANDMACRO GENERATEIMP(TJclAlgorithms<T>.Generate,IJclList<T>,const ,AItem,T)}

class {$JPPEXPANDMACRO FILLIMP(TJclAlgorithms<T>.Fill,IJclIterator<T>,const ,AItem,T,SetItem)}

class {$JPPEXPANDMACRO REVERSEIMP(TJclAlgorithms<T>.Reverse,IJclIterator<T>,T,GetItem,SetItem)}

class {$JPPEXPANDMACRO QUICKSORTIMP(TJclAlgorithms<T>.QuickSort,IJclList<T>,L,R,TCompare<T>,T,GetItem,SetItem)}

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