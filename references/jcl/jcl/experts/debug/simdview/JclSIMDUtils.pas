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
{ The Original Code is: JvSIMDUtils.pas, released on 2004-10-11.                                   }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{ [ouchet dott florent att laposte dott net]                                                       }
{ Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.                        }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ You may retrieve the latest version of this file at the Project JEDI's JCL home page,            }
{ located at https://github.com/project-jedi/jcl                                                   }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclSIMDUtils;

{$I jcl.inc}

interface

uses
  Windows,
  ToolsAPI,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclSysInfo,
  JclOtaResources;

type
  TJclMMContentType = (mt8Bytes, mt4Words, mt2DWords, mt1QWord, mt2Singles);

  TJclMMRegister = packed record
    case TJclMMContentType of
      mt8Bytes:
        (Bytes: array [0..7] of Byte;);
      mt4Words:
        (Words: array [0..3] of Word;);
      mt2DWords:
        (DWords: array [0..1] of Cardinal;);
      mt1QWord:
        (QWords: Int64;);
      mt2Singles:
        (Singles: array [0..1] of Single;);
  end;

  TJclFPUContentType = (ftExtended, ftMM);

  TJclFPUData = packed record
    case TJclFPUContentType of
      ftExtended:
        (FloatValue: Extended;);
      ftMM:
        (MMRegister: TJclMMRegister;
         Reserved: Word;);
  end;

  TJclFPURegister = packed record
    Data: TJclFPUData;
    Reserved: array [0..5] of Byte;
  end;

  TJclFPURegisters = array [0..7] of TJclFPURegister;

  TJclPackedContentType = (pctBytes, pctWords, pctDWords, pctQWords, pctSingles, pctDoubles);

  TJclXMMRegister = packed record
    case TJclPackedContentType of
      pctBytes:
        (Bytes: array [0..15] of Byte;);
      pctWords:
        (Words: array [0..7] of Word;);
      pctDWords:
        (DWords: array [0..3] of Cardinal;);
      pctQWords:
        (QWords: array [0..1] of Int64;);
      pctSingles:
        (Singles: array [0..3] of Single;);
      pctDoubles:
        (Doubles: array [0..1] of Double;);
  end;

  TJclProcessorSize = (ps32Bits, ps64Bits);

  TJclXMMRegisters = packed record
    case TJclProcessorSize of
      ps32Bits:
        (LegacyXMM: array [0..7] of TJclXMMRegister;
         LegacyReserved: array [0..127] of Byte;);
      ps64Bits:
        (LongXMM: array [0..15] of TJclXMMRegister;);
  end;

  //TJclRoundingControl = (rcRoundToNearest,   //=0
  //                       rcRoundDown,        //=1
  //                       rcRoundUp,          //=2
  //                       rcRoundTowardZero); //=3

  TJclVectorFrame = packed record
    FCW: Word;                           // bytes from 0   to 1
    FSW: Word;                           // bytes from 2   to 3
    FTW: Byte;                           // byte 4
    Reserved1: Byte;                     // byte 5
    FOP: Word;                           // bytes from 6   to 7
    FpuIp: Cardinal;                     // bytes from 8   to 11
    CS: Word;                            // bytes from 12  to 13
    Reserved2: Word;                     // bytes from 14  to 15
    FpuDp: Cardinal;                     // bytes from 16  to 19
    DS: Word;                            // bytes from 20  to 21
    Reserved3: Word;                     // bytes from 22  to 23
    MXCSR: Cardinal;                     // bytes from 24  to 27
    MXCSRMask: Cardinal;                 // bytes from 28  to 31
    FPURegisters: TJclFPURegisters;      // bytes from 32  to 159
    XMMRegisters: TJclXMMRegisters;      // bytes from 160 to 415
    Reserved4: array [416..511] of Byte; // bytes from 416 to 511
  end;

  // upper 128-bit of YMM registers (lower 128 bits are aliased to XMM registers)
  TJclYMMRegister = packed record
    case TJclPackedContentType of
      pctBytes:
        (Bytes: array [16..31] of Byte;);
      pctWords:
        (Words: array [8..15] of Word;);
      pctDWords:
        (DWords: array [4..7] of Cardinal;);
      pctQWords:
        (QWords: array [2..3] of Int64;);
      pctSingles:
        (Singles: array [4..7] of Single;);
      pctDoubles:
        (Doubles: array [2..3] of Double;);
  end;

  TJclXStateHeader = packed record
    XState_BV: Int64;
    Reserved: array [0..55] of Byte;
  end;

  TJclExtSaveArea2 = packed record
    case TJclProcessorSize of
      ps32Bits:
        (LegacyYMM: array [0..7] of TJclYMMRegister;
         LegacyReserved: array [0..127] of Byte;);
      ps64Bits:
        (LongYMM: array [0..15] of TJclYMMRegister;);
  end;
  PJclExtSaveArea2 = ^TJclExtSaveArea2;

  TJclXStateContext = packed record
    // vector context
    SaveArea: TJclVectorFrame;      // bytes 0 to 511
    Header: TJclXStateHeader;       // bytes 512 to 575
    ExtSaveArea2: TJclExtSaveArea2; // bytes 576 to 831
  end;

  TJclContext = packed record
    ScalarContext: Windows.TContext;
    ExtendedContext: TJclXStateContext;
  end;
  PJclContext = ^TJclContext;

  TBitDescription = record
    AndMask: Cardinal;
    Shifting: Cardinal;
    ShortName: PResStringRec;
    LongName: PResStringRec;
  end;

  TMXCSRRange = 0..14;

var
  MXCSRBitsDescriptions: array [TMXCSRRange] of TBitDescription =
   (
    (AndMask: MXCSR_IE;  Shifting: 0;  ShortName: nil; LongName: nil),
    (AndMask: MXCSR_DE;  Shifting: 1;  ShortName: nil; LongName: nil),
    (AndMask: MXCSR_ZE;  Shifting: 2;  ShortName: nil; LongName: nil),
    (AndMask: MXCSR_OE;  Shifting: 3;  ShortName: nil; LongName: nil),
    (AndMask: MXCSR_UE;  Shifting: 4;  ShortName: nil; LongName: nil),
    (AndMask: MXCSR_PE;  Shifting: 5;  ShortName: nil; LongName: nil),
    (AndMask: MXCSR_DAZ; Shifting: 6;  ShortName: nil; LongName: nil),
    (AndMask: MXCSR_IM;  Shifting: 7;  ShortName: nil; LongName: nil),
    (AndMask: MXCSR_DM;  Shifting: 8;  ShortName: nil; LongName: nil),
    (AndMask: MXCSR_ZM;  Shifting: 9;  ShortName: nil; LongName: nil),
    (AndMask: MXCSR_OM;  Shifting: 10; ShortName: nil; LongName: nil),
    (AndMask: MXCSR_UM;  Shifting: 11; ShortName: nil; LongName: nil),
    (AndMask: MXCSR_PM;  Shifting: 12; ShortName: nil; LongName: nil),
    (AndMask: MXCSR_RC;  Shifting: 13; ShortName: nil; LongName: nil),
    (AndMask: MXCSR_FZ;  Shifting: 15; ShortName: nil; LongName: nil)
   );

type
  TJclSIMDValue = packed record
    case Display: TJclPackedContentType of
      pctBytes:
        (ValueByte: Byte;);
      pctWords:
        (ValueWord: Word;);
      pctDWords:
        (ValueDWord: Cardinal;);
      pctQWords:
        (ValueQWord: Int64;);
      pctSingles:
        (ValueSingle: Single;);
      pctDoubles:
        (ValueDouble: Double;);
  end;

  TJclSIMDFormat = (sfBinary, sfSigned, sfUnsigned, sfHexa);

function FormatValue(Value: TJclSIMDValue; Format: TJclSIMDFormat): string;
function ParseValue(const StringValue: string; var Value: TJclSIMDValue;
  Format: TJclSIMDFormat): Boolean;
function ReplaceSIMDRegisters(var Expression: string; Is64Bits, YMMEnabled: Boolean;
  var JclContext: TJclContext): Boolean;

// return the XMM registers for the specified thread, this thread must be suspended
function GetThreadJclContext(AThread: IOTAThread; out JclContext: TJclContext): Boolean;
// return the XMM registers for the specified thread, this thread must be suspended
function SetThreadJclContext(AThread: IOTAThread; const JclContext: TJclContext): Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\debug\simdview';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils, Math,
  JclStrings,
  JclSysUtils,
  JclWin32,
  JclOtaUtils;

function FormatBinary(Value: TJclSIMDValue): string;
var
  I: Byte;
const
  Width: array [pctBytes..pctQWords] of Byte = (8, 16, 32, 64);
begin
  if not (Value.Display in [pctBytes, pctWords, pctDWords, pctQWords]) then
    raise EJclExpertException.CreateRes(@RsEBadRegisterDisplay);

  Assert(Value.Display < pctSingles);
  Result := StringOfChar('0', Width[Value.Display]);
  for I := 1 to Width[Value.Display] do
  begin
    if (Value.ValueQWord and 1) <> 0 then
      Result[Width[Value.Display] - I + 1] := '1';
    Value.ValueQWord := Value.ValueQWord shr 1;
  end;
end;

function FormatSigned(Value: TJclSIMDValue): string;
const
  Width: array [pctBytes..pctQWords] of Byte = (4, 6, 11, 20);
begin
  if not (Value.Display in [pctBytes, pctWords, pctDWords, pctQWords]) then
    raise EJclExpertException.CreateRes(@RsEBadRegisterDisplay);
    
  case Value.Display of
    pctBytes:
      Result := IntToStr(Shortint(Value.ValueByte));
    pctWords:
      Result := IntToStr(Smallint(Value.ValueWord));
    pctDWords:
      Result := IntToStr(Integer(Value.ValueDWord));
    pctQWords:
      Result := IntToStr(Value.ValueQWord);
  else
    Result := '';
    Exit;
  end;
  Result := StringOfChar(' ', Width[Value.Display] - Length(Result)) + Result;
end;

function FormatUnsigned(Value: TJclSIMDValue): string;
const
  Width: array [pctBytes..pctQWords] of Byte = (3, 5, 10, 20);
begin
  if not (Value.Display in [pctBytes, pctWords, pctDWords, pctQWords]) then
    raise EJclExpertException.CreateRes(@RsEBadRegisterDisplay);
    
  case Value.Display of
    pctBytes:
      Result := IntToStr(Byte(Value.ValueByte));
    pctWords:
      Result := IntToStr(Word(Value.ValueWord));
    pctDWords:
      Result := IntToStr(Cardinal(Value.ValueDWord));
    pctQWords:
      Result := IntToStr(Value.ValueQWord);
  else
    Result := '';
    Exit;
  end;
  Result := StringOfChar(' ', Width[Value.Display] - Length(Result)) + Result;
end;

function FormatHexa(Value: TJclSIMDValue): string;
const
  Width: array [pctBytes..pctQWords] of Byte = (2, 4, 8, 16);
begin
  if not (Value.Display in [pctBytes, pctWords, pctDWords, pctQWords]) then
    raise EJclExpertException.CreateRes(@RsEBadRegisterDisplay);
    
  case Value.Display of
    pctBytes:
      Result := IntToHex(Value.ValueByte, Width[pctBytes]);
    pctWords:
      Result := IntToHex(Value.ValueWord, Width[pctWords]);
    pctDWords:
      Result := IntToHex(Value.ValueDWord, Width[pctDWords]);
    pctQWords:
      Result := IntToHex(Value.ValueQWord, Width[pctQWords]);
  else
    Result := '';
  end;
end;

function FormatFloat(Value: TJclSIMDValue): string;
begin
  if not (Value.Display in [pctSingles, pctDoubles]) then
    raise EJclExpertException.CreateRes(@RsEBadRegisterDisplay);
    
  case Value.Display of
    pctSingles:
      Result := FloatToStr(Value.ValueSingle);
    pctDoubles:
      Result := FloatToStr(Value.ValueDouble);
  else
    Result := '';
  end;
  Result := StringOfChar(' ', 22 - Length(Result)) + Result; // 22 = max string length of a double value
end;

function FormatValue(Value: TJclSIMDValue; Format: TJclSIMDFormat): string;
type
  TFormatFunction = function(Value: TJclSIMDValue): string;
var
  FormatFunction: TFormatFunction;
begin
  Result := '';
  case Format of
    sfBinary:
      FormatFunction := FormatBinary;
    sfSigned:
      FormatFunction := FormatSigned;
    sfUnsigned:
      FormatFunction := FormatUnsigned;
    sfHexa:
      FormatFunction := FormatHexa;
  else
    Exit;
  end;
  case Value.Display of
    pctBytes..pctQWords:
      Result := FormatFunction(Value);
    pctSingles..pctDoubles:
      Result := FormatFloat(Value);
  end;
end;

function ParseBinary(StringValue: string; var Value: TJclSIMDValue): Boolean;
var
  TestValue: Int64;
  Index: Integer;
begin
  TestValue := 0;
  Result := False;
  if Length(StringValue) > 64 then
    Exit;
  for Index := 1 to Length(StringValue) do
  begin
    TestValue := TestValue shl 1;
    case StringValue[Index] of
      '0':
        ;
      '1':
        Inc(TestValue);
    else
      Exit;
    end;
  end;
  Result := True;
  case Value.Display of
    pctBytes:
      if (TestValue >= Byte($00)) and (TestValue <= Byte($FF)) then
        Value.ValueByte := TestValue
      else
        Result := False;
    pctWords:
      if (TestValue >= Word($0000)) and (TestValue <= Word($FFFF)) then
        Value.ValueWord := TestValue
      else
        Result := False;
    pctDWords:
      if (TestValue >= Cardinal($00000000)) and (TestValue <= Cardinal($FFFFFFFF)) then
        Value.ValueDWord := TestValue
      else
        Result := False;
    pctQWords:
      Value.ValueQWord := TestValue;
  else
    Result := False;
  end;
end;

function ParseSigned(StringValue: string; var Value: TJclSIMDValue): Boolean;
var
  TestValue: Int64;
  ErrorCode: Integer;
begin
  Val(StringValue, TestValue, ErrorCode);
  Result := ErrorCode = 0;
  if Result then
    case Value.Display of
      pctBytes:
        if (TestValue >= Shortint($80)) and (TestValue <= Shortint($7F)) then
          Value.ValueByte := TestValue
        else
          Result := False;
      pctWords:
        if (TestValue >= Smallint($8000)) and (TestValue <= Smallint($7FFF)) then
          Value.ValueWord := TestValue
        else
          Result := False;
      pctDWords:
        if (TestValue >= Integer($80000000)) and (TestValue <= Integer($7FFFFFFF)) then
          Value.ValueDWord := TestValue
        else
          Result := False;
      pctQWords:
        Value.ValueQWord := TestValue;
    else
      Result := False;
    end;
end;

function ParseUnsigned(StringValue: string; var Value: TJclSIMDValue): Boolean;
var
  TestValue: Int64;
  ErrorCode: Integer;
begin
  Val(StringValue, TestValue, ErrorCode);
  Result := ErrorCode = 0;
  if Result then
    case Value.Display of
      pctBytes:
        if (TestValue >= Byte($00)) and (TestValue <= Byte($FF)) then
          Value.ValueByte := TestValue
        else
          Result := False;
      pctWords:
        if (TestValue >= Word($0000)) and (TestValue <= Word($FFFF)) then
          Value.ValueWord := TestValue
        else
          Result := False;
      pctDWords:
        if (TestValue >= Cardinal($00000000)) and (TestValue <= Cardinal($FFFFFFFF)) then
          Value.ValueDWord := TestValue
        else
          Result := False;
      pctQWords:
        Value.ValueQWord := TestValue;
    else
      Result := False;
    end;
end;

function ParseHexa(StringValue: string; var Value: TJclSIMDValue): Boolean;
var
  TestValue: Int64;
  Index: Integer;
begin
  TestValue := 0;
  Result := False;
  if Length(StringValue) > 16 then
    Exit;
  for Index := 1 to Length(StringValue) do
  begin
    TestValue := TestValue shl 4;
    case StringValue[Index] of
      '0':
        ;
      '1'..'9':
        Inc(TestValue, Ord(StringValue[Index]) - Ord('0'));
      'A'..'F':
        Inc(TestValue, Ord(StringValue[Index]) - Ord('A') + 10);
      'a'..'f':
        Inc(TestValue, Ord(StringValue[Index]) - Ord('a') + 10);
    else
      Exit;
    end;
  end;
  Result := True;
  case Value.Display of
    pctBytes:
      if (TestValue >= Byte($00)) and (TestValue <= Byte($FF)) then
        Value.ValueByte := TestValue
      else
        Result := False;
    pctWords:
      if (TestValue >= Word($0000)) and (TestValue <= Word($FFFF)) then
        Value.ValueWord := TestValue
      else
        Result := False;
    pctDWords:
      if (TestValue >= Cardinal($00000000)) and (TestValue <= Cardinal($FFFFFFFF)) then
        Value.ValueDWord := TestValue
      else
        Result := False;
    pctQWords:
      Value.ValueQWord := TestValue;
  else
    Result := False;
  end;
end;

function ParseFloat(StringValue: string; var Value: TJclSIMDValue): Boolean;
var
  TestValue: Extended;
  ErrorCode: Integer;
begin
  if {$IFDEF RTL220_UP}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then
    StringValue := StringReplace(StringValue, {$IFDEF RTL220_UP}FormatSettings.{$ENDIF}DecimalSeparator, '.', [rfReplaceAll, rfIgnoreCase]);
  Val(StringValue, TestValue, ErrorCode);
  Result := ErrorCode = 0;
  if Result then
    case Value.Display of
      pctSingles:
        if (TestValue >= -MaxSingle) and (TestValue <= MaxSingle) then
          Value.ValueSingle := TestValue
        else
          Result := False;
      pctDoubles:
        if (TestValue >= MaxDouble) and (TestValue <= MaxDouble) then
          Value.ValueDouble := TestValue
        else
          Result := False;
    else
      Result := False;
    end;
end;

function ParseValue(const StringValue: string; var Value: TJclSIMDValue;
  Format: TJclSIMDFormat): Boolean;
type
  TParseFunction = function(StringValue: string; var Value: TJclSIMDValue): Boolean;
var
  ParseFunction: TParseFunction;
begin
  Result := False;
  case Format of
    sfBinary:
      ParseFunction := ParseBinary;
    sfSigned:
      ParseFunction := ParseSigned;
    sfUnsigned:
      ParseFunction := ParseUnsigned;
    sfHexa:
      ParseFunction := ParseHexa;
  else
    Exit;
  end;
  case Value.Display of
    pctBytes..pctQWords:
      Result := ParseFunction(StringValue, Value);
    pctSingles..pctDoubles:
      Result := ParseFloat(StringValue, Value);
  end;
end;

function ReplaceSIMDRegisters(var Expression: string; Is64Bits, YMMEnabled: Boolean;
  var JclContext: TJclContext): Boolean;
var
  LocalString: string;
  RegisterPosition: Integer;
  DataPosition: Integer;
  DataType: string;
  Index: Integer;
  RegisterIndex: Integer;
  DataIndex: Integer;
  ErrorCode: Integer;
  NumberOfXMMRegister: Integer;
  AValue: TJclSIMDValue;
  ValueStr: string;
  OldLength: Integer;
  XMMMatch: Boolean;
begin
  if Is64Bits then
    NumberOfXMMRegister := 16
  else
    NumberOfXMMRegister := 8;
  Result := False;
  LocalString := AnsiUpperCase(Expression);

  XMMMatch := False;
  RegisterPosition := AnsiPos('XMM', LocalString);
  if YMMEnabled and (RegisterPosition = 0) then
    RegisterPosition := AnsiPos('YMM', LocalString)
  else
    XMMMatch := True;
  while (RegisterPosition > 0) do
  begin
    for Index := RegisterPosition to Length(LocalString) do
      if LocalString[Index] = '.' then
        Break;
    if Index >= Length(LocalString) then
      Exit;
    Val(Copy(LocalString, RegisterPosition + 3, Index - RegisterPosition - 3), RegisterIndex, ErrorCode);
    if (ErrorCode <> 0) or (RegisterIndex < 0) or (RegisterIndex >= NumberOfXMMRegister) then
      Exit;

    DataPosition := Index + 1;
    if DataPosition > Length(LocalString) then
      Exit;
    for Index := DataPosition to Length(LocalString) do
      if CharIsDigit(LocalString[Index]) then
        Break;
    if Index > Length(LocalString) then
      Exit;
    DataType := Copy(LocalString, DataPosition, Index - DataPosition);

    DataPosition := Index;
    for Index := DataPosition to Length(LocalString) do
      if not CharIsDigit(LocalString[Index]) then
        Break;
    Val(Copy(LocalString, DataPosition, Index - DataPosition), DataIndex, ErrorCode);
    if (ErrorCode <> 0) or (DataIndex < 0) then
      Exit;

    if CompareStr(DataType, 'BYTE') = 0 then
    begin
      AValue.Display := pctBytes;
      if DataIndex >= Low(JclContext.ExtendedContext.ExtSaveArea2.LongYMM[RegisterIndex].Bytes) then
      begin
        if XMMMatch then
          Exit;
        AValue.ValueByte := JclContext.ExtendedContext.ExtSaveArea2.LongYMM[RegisterIndex].Bytes[DataIndex];
      end
      else
        AValue.ValueByte := JclContext.ExtendedContext.SaveArea.XMMRegisters.LongXMM[RegisterIndex].Bytes[DataIndex];
    end
    else
    if CompareStr(DataType, 'WORD') = 0 then
    begin
      AValue.Display := pctWords;
      if DataIndex >= Low(JclContext.ExtendedContext.ExtSaveArea2.LongYMM[RegisterIndex].Words) then
      begin
        if XMMMatch then
          Exit;
        AValue.ValueWord := JclContext.ExtendedContext.ExtSaveArea2.LongYMM[RegisterIndex].Words[DataIndex];
      end
      else
        AValue.ValueWord := JclContext.ExtendedContext.SaveArea.XMMRegisters.LongXMM[RegisterIndex].Words[DataIndex];
    end
    else
    if CompareStr(DataType, 'DWORD') = 0 then
    begin
      AValue.Display := pctDWords;
      if DataIndex >= Low(JclContext.ExtendedContext.ExtSaveArea2.LongYMM[RegisterIndex].DWords) then
      begin
        if XMMMatch then
          Exit;
        AValue.ValueDWord := JclContext.ExtendedContext.ExtSaveArea2.LongYMM[RegisterIndex].DWords[DataIndex];
      end
      else
        AValue.ValueDWord := JclContext.ExtendedContext.SaveArea.XMMRegisters.LongXMM[RegisterIndex].DWords[DataIndex];
    end
    else
    if CompareStr(DataType, 'QWORD') = 0 then
    begin
      AValue.Display := pctQWords;
      if DataIndex >= Low(JclContext.ExtendedContext.ExtSaveArea2.LongYMM[RegisterIndex].QWords) then
      begin
        if XMMMatch then
          Exit;
        AValue.ValueQWord := JclContext.ExtendedContext.ExtSaveArea2.LongYMM[RegisterIndex].QWords[DataIndex];
      end
      else
        AValue.ValueQWord := JclContext.ExtendedContext.SaveArea.XMMRegisters.LongXMM[RegisterIndex].QWords[DataIndex];
    end
    else
    if CompareStr(DataType, 'SINGLE') = 0 then
    begin
      AValue.Display := pctSingles;
      if DataIndex >= Low(JclContext.ExtendedContext.ExtSaveArea2.LongYMM[RegisterIndex].Singles) then
      begin
        if XMMMatch then
          Exit;
        AValue.ValueSingle := JclContext.ExtendedContext.ExtSaveArea2.LongYMM[RegisterIndex].Singles[DataIndex];
      end
      else
        AValue.ValueSingle := JclContext.ExtendedContext.SaveArea.XMMRegisters.LongXMM[RegisterIndex].Singles[DataIndex];
    end
    else
    if CompareStr(DataType, 'DOUBLE') = 0 then
    begin
      AValue.Display := pctDoubles;
      if DataIndex >= Low(JclContext.ExtendedContext.ExtSaveArea2.LongYMM[RegisterIndex].Doubles) then
      begin
        if XMMMatch then
          Exit;
        AValue.ValueDouble := JclContext.ExtendedContext.ExtSaveArea2.LongYMM[RegisterIndex].Doubles[DataIndex];
      end
      else
        AValue.ValueDouble := JclContext.ExtendedContext.SaveArea.XMMRegisters.LongXMM[RegisterIndex].Doubles[DataIndex];
    end
    else
      Exit;
    ValueStr := Trim(FormatValue(AValue, sfSigned));
    if {$IFDEF RTL220_UP}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then
      ValueStr := StringReplace(ValueStr, {$IFDEF RTL220_UP}FormatSettings.{$ENDIF}DecimalSeparator, '.', [rfReplaceAll, rfIgnoreCase]);
    if Length(ValueStr) >= Index - RegisterPosition then
    begin
      OldLength := Length(Expression);
      SetLength(Expression, Length(Expression) + Length(ValueStr) - (Index - RegisterPosition));
      if Length(ValueStr) > Index - RegisterPosition then
        Move(Expression[Index], Expression[RegisterPosition + Length(ValueStr)], OldLength - Index + 1);
      Move(ValueStr[1], Expression[RegisterPosition], Length(ValueStr));
    end
    else
    begin
      Move(ValueStr[1], Expression[RegisterPosition], Length(ValueStr));
      Move(Expression[Index], Expression[RegisterPosition + Length(ValueStr)], Length(Expression) - Index + 1);
      SetLength(Expression, Length(Expression) + Length(ValueStr) - (Index - RegisterPosition));
    end;
    LocalString := AnsiUpperCase(Expression);
    XMMMatch := False;
    RegisterPosition := AnsiPos('XMM', LocalString);
    if YMMEnabled and (RegisterPosition = 0) then
      RegisterPosition := AnsiPos('YMM', LocalString)
    else
      XMMMatch := True;
  end;

  RegisterPosition := AnsiPos('MM', LocalString);
  while (RegisterPosition > 0) do
  begin
    for Index := RegisterPosition to Length(LocalString) do
      if LocalString[Index] = '.' then
        Break;
    if Index >= Length(LocalString) then
      Exit;
    Val(Copy(LocalString, RegisterPosition + 2, Index - RegisterPosition - 2), RegisterIndex, ErrorCode);
    if (ErrorCode <> 0) or (RegisterIndex < 0) or (RegisterIndex >= 8) then
      Exit;

    DataPosition := Index + 1;
    if DataPosition > Length(LocalString) then
      Exit;
    for Index := DataPosition to Length(LocalString) do
      if CharIsDigit(LocalString[Index]) then
        Break;
    if Index > Length(LocalString) then
      Exit;
    DataType := Copy(LocalString, DataPosition, Index - DataPosition);

    DataPosition := Index;
    for Index := DataPosition to Length(LocalString) do
      if not CharIsDigit(LocalString[Index]) then
        Break;
    Val(Copy(LocalString, DataPosition, Index - DataPosition), DataIndex, ErrorCode);
    if (ErrorCode <> 0) or (DataIndex < 0) then
      Exit;

    if CompareStr(DataType, 'BYTE') = 0 then
    begin
      if DataIndex >= 8 then
        Exit;
      AValue.Display := pctBytes;
      AValue.ValueByte := JclContext.ExtendedContext.SaveArea.FPURegisters[RegisterIndex].Data.MMRegister.Bytes[DataIndex];
    end
    else
    if CompareStr(DataType, 'WORD') = 0 then
    begin
      if DataIndex >= 4 then
        Exit;
      AValue.Display := pctWords;
      AValue.ValueWord := JclContext.ExtendedContext.SaveArea.FPURegisters[RegisterIndex].Data.MMRegister.Words[DataIndex];
    end
    else
    if CompareStr(DataType, 'DWORD') = 0 then
    begin
      if DataIndex >= 2 then
        Exit;
      AValue.Display := pctDWords;
      AValue.ValueDWord := JclContext.ExtendedContext.SaveArea.FPURegisters[RegisterIndex].Data.MMRegister.DWords[DataIndex];
    end
    else
    if CompareStr(DataType, 'QWORD') = 0 then
    begin
      if DataIndex >= 1 then
        Exit;
      AValue.Display := pctQWords;
      AValue.ValueQWord := JclContext.ExtendedContext.SaveArea.FPURegisters[RegisterIndex].Data.MMRegister.QWords;
    end
    else
    if CompareStr(DataType, 'SINGLE') = 0 then
    begin
      if DataIndex >= 2 then
        Exit;
      AValue.Display := pctSingles;
      AValue.ValueSingle := JclContext.ExtendedContext.SaveArea.FPURegisters[RegisterIndex].Data.MMRegister.Singles[DataIndex];
    end
    else
      Exit;
    ValueStr := Trim(FormatValue(AValue, sfSigned));
    if {$IFDEF RTL220_UP}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then
      ValueStr := StringReplace(ValueStr, {$IFDEF RTL220_UP}FormatSettings.{$ENDIF}DecimalSeparator, '.', [rfReplaceAll, rfIgnoreCase]);
    if Length(ValueStr) >= Index - RegisterPosition then
    begin
      OldLength := Length(Expression);
      SetLength(Expression, Length(Expression) + Length(ValueStr) - (Index - RegisterPosition));
      if Length(ValueStr) > Index - RegisterPosition then
        Move(Expression[Index], Expression[RegisterPosition + Length(ValueStr)], OldLength - Index + 1);
      Move(ValueStr[1], Expression[RegisterPosition], Length(ValueStr));
    end
    else
    begin
      Move(ValueStr[1], Expression[RegisterPosition], Length(ValueStr));
      Move(Expression[Index], Expression[RegisterPosition + Length(ValueStr)], Length(Expression) - Index + 1);
      SetLength(Expression, Length(Expression) + Length(ValueStr) - (Index - RegisterPosition));
    end;
    LocalString := AnsiUpperCase(Expression);
    RegisterPosition := AnsiPos('MM', LocalString);
  end;

  Result := True;
end;

// return the processor frame for the specified thread, this thread must be suspended
function GetThreadContext(hThread: THandle; lpContext: Pointer): BOOL; stdcall; external kernel32 name 'GetThreadContext';

// set the processor frame for the specified thread, this thread must be suspended
function SetThreadContext(hThread: THandle; lpContext: Pointer): BOOL; stdcall; external kernel32 name 'SetThreadContext';

function GetThreadJclContext(AThread: IOTAThread; out JclContext: TJclContext): Boolean;
var
  {$IFDEF COMPILER9_UP}
  OTAXMMRegs: TOTAXMMRegs;
  OTAThreadContext: TOTAThreadContext;
  {$ELSE ~COMPILER9_UP}
  ContextMemory: Pointer;
  AlignedContext: PJclContext;
  {$ENDIF ~COMPILER9_UP}
  ExtendedContextLength: DWORD;
  ExtendedContextMemory: Pointer;
  ExtendedContext: PCONTEXT_EX;
  LegacyContext: PContext;
  AVXContext: PJclExtSaveArea2;
begin
  // get YMM registers
  if oefAVX in GetOSEnabledFeatures then
  begin
    // allocate enough memory to get this extended context
    Result := GetExtendedContextLength(CONTEXT_XSTATE, @ExtendedContextLength);
    if Result then
    begin
      GetMem(ExtendedContextMemory, ExtendedContextLength);
      try
        Result := InitializeExtendedContext(ExtendedContextMemory, CONTEXT_XSTATE, ExtendedContext);
        if Result then
        begin
          // find usefull part locations in this extended context
          LegacyContext := LocateLegacyContext(ExtendedContext, nil);
          AVXContext := LocateExtendedFeature(ExtendedContext, XSTATE_GSSE, nil);
          // get the context
          Result := GetThreadContext(AThread.Handle, LegacyContext) and
            ((LegacyContext.ContextFlags and CONTEXT_XSTATE) <> 0);
          if Result then
            // copy the data
            JclContext.ExtendedContext.ExtSaveArea2 := AVXContext^
          else
            ResetMemory(JclContext.ExtendedContext.ExtSaveArea2, SizeOf(JclContext.ExtendedContext.ExtSaveArea2));
        end;
      finally
        FreeMem(ExtendedContextMemory);
      end;
    end;
  end
  else
  begin
    Result := True;
    ResetMemory(JclContext.ExtendedContext.ExtSaveArea2, SizeOf(JclContext.ExtendedContext.ExtSaveArea2));
  end;
  {$IFDEF COMPILER9_UP}
  // get XMM registers
  if Result then
    Result := AThread.GetOTAXMMRegisters(OTAXMMRegs);
  if Result then
  begin
    // get other registers
    JclContext.ExtendedContext.SaveArea.MXCSR := OTAXMMRegs.MXCSR;
    JclContext.ExtendedContext.SaveArea.MXCSRMask := $FFFFFFFF;
    Move(OTAXMMRegs,JclContext.ExtendedContext.SaveArea.XMMRegisters, SizeOf(TOTAXMMReg) * 8);
    OTAThreadContext := AThread.OTAThreadContext;
    JclContext.ExtendedContext.SaveArea.FCW := OTAThreadContext.FloatSave.ControlWord;
    JclContext.ExtendedContext.SaveArea.FSW := OTAThreadContext.FloatSave.StatusWord;
    JclContext.ExtendedContext.SaveArea.FTW := OTAThreadContext.FloatSave.TagWord;
    Move(OTAThreadContext.FloatSave.RegisterArea[00],JclContext.ExtendedContext.SaveArea.FPURegisters[0],SizeOf(Extended));
    Move(OTAThreadContext.FloatSave.RegisterArea[10],JclContext.ExtendedContext.SaveArea.FPURegisters[1],SizeOf(Extended));
    Move(OTAThreadContext.FloatSave.RegisterArea[20],JclContext.ExtendedContext.SaveArea.FPURegisters[2],SizeOf(Extended));
    Move(OTAThreadContext.FloatSave.RegisterArea[30],JclContext.ExtendedContext.SaveArea.FPURegisters[3],SizeOf(Extended));
    Move(OTAThreadContext.FloatSave.RegisterArea[40],JclContext.ExtendedContext.SaveArea.FPURegisters[4],SizeOf(Extended));
    Move(OTAThreadContext.FloatSave.RegisterArea[50],JclContext.ExtendedContext.SaveArea.FPURegisters[5],SizeOf(Extended));
    Move(OTAThreadContext.FloatSave.RegisterArea[60],JclContext.ExtendedContext.SaveArea.FPURegisters[6],SizeOf(Extended));
    Move(OTAThreadContext.FloatSave.RegisterArea[70],JclContext.ExtendedContext.SaveArea.FPURegisters[7],SizeOf(Extended));
  end;
  {$ELSE COMPILER9_UP}
  // get XMM registers
  if Result then
  begin
    GetMem(ContextMemory, SizeOf(TJclContext) + 15);
    try
      if (Cardinal(ContextMemory) and 15) <> 0 then
        AlignedContext := PJclContext((Cardinal(ContextMemory) + 16) and $FFFFFFF0)
      else
        AlignedContext := ContextMemory;
      AlignedContext^.ScalarContext.ContextFlags := CONTEXT_EXTENDED_REGISTERS;
      Result := GetThreadContext(AThread.Handle,AlignedContext) and
        ((AlignedContext^.ScalarContext.ContextFlags and CONTEXT_EXTENDED_REGISTERS)<>0);
      ResetMemory(AlignedContext.ExtendedContext.ExtSaveArea2, SizeOf(AlignedContext.ExtendedContext.ExtSaveArea2));
      if Result then
        JclContext := AlignedContext^
      else
        ResetMemory(JclContext, SizeOf(JclContext));
    finally
      FreeMem(ContextMemory);
    end;
  end;
  {$ENDIF COMPILER9_UP}
end;

function SetThreadJclContext(AThread: IOTAThread; const JclContext: TJclContext): Boolean;
var
  {$IFDEF COMPILER9_UP}
  OTAXMMRegs: TOTAXMMRegs;
  {$ELSE ~COMPILER9_UP}
  ContextMemory: Pointer;
  AlignedContext: PJclContext;
  {$ENDIF ~COMPILER9_UP}
  ExtendedContextLength: DWORD;
  ExtendedContextMemory: Pointer;
  ExtendedContext: PCONTEXT_EX;
  LegacyContext: PContext;
  AVXContext: PJclExtSaveArea2;
begin
  // save YMM registers
  if oefAVX in GetOSEnabledFeatures then
  begin
    // allocate enough memory to get this extended context
    Result := GetExtendedContextLength(CONTEXT_XSTATE, @ExtendedContextLength);
    if Result then
    begin
      GetMem(ExtendedContextMemory, ExtendedContextLength);
      try
        Result := InitializeExtendedContext(ExtendedContextMemory, CONTEXT_XSTATE, ExtendedContext);
        if Result then
        begin
          // find usefull part locations in this extended context
          LegacyContext := LocateLegacyContext(ExtendedContext, nil);
          AVXContext := LocateExtendedFeature(ExtendedContext, XSTATE_GSSE, nil);
          // get the context
          Result := GetThreadContext(AThread.Handle, LegacyContext) and
            ((LegacyContext.ContextFlags and CONTEXT_XSTATE) <> 0);
          if Result then
          begin
            // copy the data
            AVXContext^ := JclContext.ExtendedContext.ExtSaveArea2;
            // set the context
            Result := SetThreadContext(AThread.Handle, LegacyContext);
          end;
        end;
      finally
        FreeMem(ExtendedContextMemory);
      end;
    end;
  end
  else
    Result := True;
  {$IFDEF COMPILER9_UP}
  if Result then
  begin
    try
      // save XMM registers
      OTAXMMRegs.MXCSR := JclContext.ExtendedContext.SaveArea.MXCSR;
      Move(JclContext.ExtendedContext.SaveArea.XMMRegisters,OTAXMMRegs,SizeOf(TOTAXMMReg) * 8);
      AThread.SetOTAXMMRegisters(OTAXMMRegs);
    except
      Result := False;
    end;
  end;
  {$ELSE ~COMPILER9_UP}
  if Result then
  begin
    GetMem(ContextMemory, SizeOf(TJclContext) + 15);
    try
      if (Cardinal(ContextMemory) and 15) <> 0 then
        AlignedContext := PJclContext((Cardinal(ContextMemory) + 16) and $FFFFFFF0)
      else
        AlignedContext := ContextMemory;
      AlignedContext^.ScalarContext.ContextFlags := CONTEXT_EXTENDED_REGISTERS;
      Result := GetThreadContext(AThread.Handle,AlignedContext) and
        ((AlignedContext^.ScalarContext.ContextFlags and CONTEXT_EXTENDED_REGISTERS) = CONTEXT_EXTENDED_REGISTERS);
      AlignedContext^ := JclContext;
      if Result then
        Result := SetThreadContext(AThread.Handle,AlignedContext);
      // TODO set the YMM registers
    finally
      FreeMem(ContextMemory);
    end;
  end;
  {$ENDIF COMPILER9_UP}
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
