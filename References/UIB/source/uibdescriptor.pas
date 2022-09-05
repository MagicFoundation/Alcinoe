(********************************************************************************)
(*                        UNIFIED INTERBASE (UIB)                               *)
(*                                                                              *)
(* The contents of this file are subject to the Mozilla Public License Version  *)
(* 1.1 (the "License"); you may not use this file except in compliance with the *)
(* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ *)
(*                                                                              *)
(* Software distributed under the License is distributed on an "AS IS" basis,   *)
(* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for *)
(* the specific language governing rights and limitations under the License.    *)
(*                                                                              *)
(* Unit owner : Henri Gourvest <hgourvest@progdigy.com>                         *)
(********************************************************************************)

unit uibdescriptor;
{$I uib.inc}

interface

{$IFDEF FB102_UP}
uses classes, uibase, uiblib;

function DescGetIsNull(Param: PParamDsc): boolean;
procedure DescSetIsNull(Param: PParamDsc; Value: boolean);

function DescGetIsNullable(Param: PParamDsc): boolean;
procedure DescSetIsNullable(Param: PParamDsc; Value: boolean);

function DescGetIsNoSubtype(Param: PParamDsc): boolean;
procedure DescSetIsNoSubtype(Param: PParamDsc; Value: boolean);

function DescGetFieldType(Param: PParamDsc): TUIBFieldType;

function DescGetAsInteger(Param: PParamDsc): Integer;
procedure DescSetAsInteger(Param: PParamDsc; Value: Integer);
function DescGetAsDouble(Param: PParamDsc): Double;
procedure DescSetAsDouble(Param: PParamDsc; Value: Double);
function DescGetAsCurrency(Param: PParamDsc): Currency;
procedure DescSetAsCurrency(Param: PParamDsc; const Value: Currency);
function DescGetAsInt64(Param: PParamDsc): Int64;
procedure DescSetAsInt64(Param: PParamDsc; const Value: Int64);
function DescGetAsSingle(Param: PParamDsc): Single;
procedure DescSetAsSingle(Param: PParamDsc; const Value: Single);
function DescGetAsSmallint(Param: PParamDsc): Smallint;
procedure DescSetAsSmallint(Param: PParamDsc; const Value: Smallint);
function DescGetAsString(Param: PParamDsc): AnsiString;
procedure DescSetAsString(Param: PParamDsc; const Value: AnsiString);
function DescGetAsDateTime(Param: PParamDsc): TDateTime;
procedure DescSetAsDateTime(Param: PParamDsc; const Value: TDateTime);
function DescGetAsDate(Param: PParamDsc): Integer;
procedure DescSetAsDate(Param: PParamDsc; const Value: Integer);
function DescGetAsTime(Param: PParamDsc): Cardinal;
procedure DescSetAsTime(Param: PParamDsc; const Value: Cardinal);

function DescBlobGetSegment(Param: PBlobCallBack; out length: Word;
  BufferLength: Cardinal; Buffer: Pointer): boolean;
procedure DescBlobReadString(Param: PBlobCallBack; var Str: AnsiString);
procedure DescBlobSaveToStream(Param: PBlobCallBack; Stream: TStream);
procedure DescBlobReadSizedBuffer(Param: PBlobCallBack; Buffer: Pointer);
procedure DescBlobWriteSegment(Param: PBlobCallBack; BufferLength: Cardinal; Buffer: Pointer);
procedure DescBlobWriteString(Param: PBlobCallBack; var Str: AnsiString);
procedure DescBlobWriteStream(Param: PBlobCallBack; Stream: TStream);

{$ENDIF}

implementation
{$IFDEF FB102_UP}
uses dateutils, uibconst;

type
  TDescFlag = set of 0..15;
  PDescFlag = ^TDescFlag;

const
  DefaultSegmentSize = 16*1024;

function IntToStr(value: integer): AnsiString;
begin
  str(value, result);
end;

function WordToStr(value: word): AnsiString;
begin
  str(value, result);
end;

function CardToStr(value: Cardinal): AnsiString;
begin
  str(value, result);
end;

function SmallToStr(value: smallint): AnsiString;
begin
  str(value, result);
end;

function Int64ToStr(value: int64): AnsiString;
begin
  str(value, result);
end;

function FloatToStr(value: double): AnsiString;
begin
  str(value, result);
end;

function SingToStr(value: Single): AnsiString;
begin
  str(value, result);
end;

function CurrToStr(value: currency): AnsiString;
begin
  str(value, result);
end;

function StrToInt(const value: AnsiString): integer;
var
  code: integer;
begin
  val(string(value), Result, code);
  if code <> 0 then
    Result := 0;
end;

function StrToWord(const value: AnsiString): word;
var
  code: integer;
begin
  val(string(value), Result, code);
  if code <> 0 then
    Result := 0;
end;

function StrToCard(const value: AnsiString): Cardinal;
var
  code: integer;
begin
  val(string(value), Result, code);
  if code <> 0 then
    Result := 0;
end;

function StrToSmall(const value: AnsiString): smallint;
var
  code: integer;
begin
  val(string(value), Result, code);
  if code <> 0 then
    Result := 0;
end;

function StrToInt64(const value: AnsiString): int64;
var
  code: integer;
begin
  val(string(value), Result, code);
  if code <> 0 then
    Result := 0;
end;

function StrToFloat(const value: AnsiString): double;
var
  code: integer;
begin
  val(string(value), Result, code);
  if code <> 0 then
    Result := 0;
end;

function StrToSing(const value: AnsiString): Single;
var
  code: integer;
begin
  val(string(value), Result, code);
  if code <> 0 then
    Result := 0;
end;


procedure DescDecodeString(Param: PParamDsc; out str: AnsiString); overload;
begin
  case Param.dsc_dtype of
    dtype_text    : SetString(str, Param.dsc_address, Param.dsc_length);
    dtype_varying : SetString(str, PVary(Param.dsc_address).vary_string, PVary(Param.dsc_address).vary_length);
  end;
end;

function DescDecodeString(Param: PParamDsc): AnsiString; overload;
begin
  case Param.dsc_dtype of
    dtype_text    : SetString(Result, Param.dsc_address, Param.dsc_length);
    dtype_varying : SetString(Result, PVary(Param.dsc_address).vary_string, PVary(Param.dsc_address).vary_length);
  end;
end;

function TimeStampToSQLStr(TimeStamp: PISCTimeStamp): AnsiString;
var
  Year: SmallInt;
  Month, Day: Word;
  Hour, Minute, Second: Word;
  Fractions: LongWord;
begin
  DecodeSQLDate(TimeStamp.timestamp_date, Year, Month, Day);
  DecodeSQLTime(TimeStamp.timestamp_time, Hour, Minute, Second, Fractions);
  Result := SmallToStr(Year) + '-' + WordToStr(Month) + '-' + WordToStr(Day) + ' ' +
    WordToStr(Hour) + ':' + WordToStr(Minute) + ':' + WordToStr(Second) + '.' + CardToStr(Fractions);
end;

function DateTimeToSQLStr(value: TDateTime): AnsiString;
var
  Year: Word;
  Month, Day: Word;
  Hour, Minute, Second: Word;
  Fractions: Word;
begin
  DecodeDateTime(value, Year, Month, Day, Hour, Minute, Second, Fractions);
  Result := SmallToStr(Year) + '-' + WordToStr(Month) + '-' + WordToStr(Day) + ' ' +
    WordToStr(Hour) + ':' + WordToStr(Minute) + ':' + WordToStr(Second) + '.' + CardToStr(Fractions * 10);
end;

function DateToSQLStr(value: integer): AnsiString;
var
  Year: SmallInt;
  Month, Day: Word;
begin
  DecodeSQLDate(value, Year, Month, Day);
  Result := SmallToStr(Year) + '-' + WordToStr(Month) + '-' + WordToStr(Day);
end;

function TimeToSQLStr(Value: cardinal): AnsiString;
var
  Hour, Minute, Second: Word;
  Fractions: LongWord;
begin
  DecodeSQLTime(Value, Hour, Minute, Second, Fractions);
  Result := WordToStr(Hour) + ':' + WordToStr(Minute) + ':' + WordToStr(Second) + '.' + CardToStr(Fractions);
end;

function DecodeSQLStrDateTime(const str: AnsiString; out Year: SmallInt; out Month, Day: Word;
  out Hour, Minute, Second: Word; out Fractions: LongWord): boolean;
var
  p: PAnsiChar;
  v: Word;
  c: integer;
  s: AnsiChar;
label
  lbMinutes, lbError;
begin
  Result := True;
  Year := 0;
  Month := 0;
  Day := 0;
  Hour := 0;
  Minute := 0;
  Second := 0;
  Fractions := 0;

  p := PAnsiChar(str);

  // year
  val(string(p), v, c);
  if c = 0 then goto lbError;
  inc(p, c);
  case p[-1] of
    '-', '/':
      begin
        Year := v;
        s := p[-1];
      end;
    ':':
      begin
        Hour := v;
        goto lbMinutes;
      end
  else
    goto lbError;
  end;

  // month
  val(string(p), Month, c);
  if (c = 0) then
    goto lbError;
  inc(p, c);
  if p[-1] <> s then
    goto lbError;

  // Day
  val(string(p), Day, c);
  if c = 0 then exit;
  inc(p, c);

  // Hour
  val(string(p), Hour, c);
  if c = 0 then goto lbError;
  inc(p, c);
  if p[-1] <> ':' then
    goto lbError;

lbMinutes:
  // Minute
  val(string(p), Minute, c);
  if c = 0 then exit;
  inc(p, c);

  if (p[-1] <> ':') then
    Exit;

  // Second
  val(string(p), Second, c);
  if c = 0 then exit;
  inc(p, c);

  if p[-1] = '.' then
    val(string(p), Fractions, c) else
    Exit;

lbError:
  Result := False;
end;

function DecodeSQLStrDate(const str: AnsiString; out Year: SmallInt; out Month, Day: Word): boolean;
var
  p: PAnsiChar;
  v: Word;
  c: integer;
  s: AnsiChar;
label
  lbError;
begin
  Result := True;
  Year := 0;
  Month := 0;
  Day := 0;

  p := PAnsiChar(str);

  // year
  val(string(p), v, c);
  if c = 0 then goto lbError;
  inc(p, c);
  case p[-1] of
    '-', '/':
      begin
        Year := v;
        s := p[-1];
      end;
    ':': Exit;
  else
    goto lbError;
  end;

  // month
  val(string(p), Month, c);
  if (c = 0) then
    goto lbError;
  inc(p, c);
  if p[-1] <> s then
    goto lbError;

  // Day
  val(string(p), Day, c);
  if c = 0 then exit;

  Exit;

lbError:
  Result := False;
end;

function DecodeSQLStrTime(const str: AnsiString; out Hour, Minute, Second: Word; out Fractions: LongWord): boolean;
var
  p: PAnsiChar;
  c: integer;
label
  lbError;
begin
  Result := True;
  Hour := 0;
  Minute := 0;
  Second := 0;
  Fractions := 0;

  p := PAnsiChar(str);

  // Hour
  val(string(p), Hour, c);
  if c = 0 then goto lbError;
  inc(p, c);
  if p[-1] <> ':' then
    goto lbError;

  // Minute
  val(string(p), Minute, c);
  if c = 0 then exit;
  inc(p, c);

  if (p[-1] <> ':') then
    Exit;

  // Second
  val(string(p), Second, c);
  if c = 0 then exit;
  inc(p, c);

  if p[-1] = '.' then
    val(string(p), Fractions, c) else
    Exit;

lbError:
  Result := False;
end;

function EncodeSQLStrDateTime(const str: AnsiString; Value: PISCTimeStamp): boolean;
var
  Year: SmallInt;
  Month, Day: Word;
  Hour, Minute, Second: Word;
  Fractions: LongWord;
begin
  Result := DecodeSQLStrDateTime(str, Year, Month, Day, Hour, Minute, Second, Fractions);
  if Result then
  begin
     Value.timestamp_date := EncodeSQLDate(Year, Month, Day);
     Value.timestamp_time := EncodeSQLTime(Hour, Minute, Second, Fractions);
  end;
end;

function EncodeSQLStrDate(const str: AnsiString; out Value: Integer): boolean;
var
  Year: SmallInt;
  Month, Day: Word;
begin
  Result := DecodeSQLStrDate(str, Year, Month, Day);
  if Result then
     Value := EncodeSQLDate(Year, Month, Day);
end;

function EncodeSQLStrTime(const str: AnsiString; out Value: Cardinal): boolean;
var
  Hour, Minute, Second: Word;
  Fractions: LongWord;
begin
  Result := DecodeSQLStrTime(str, Hour, Minute, Second, Fractions);
  if Result then
     Value := EncodeSQLTime(Hour, Minute, Second, Fractions);
end;

procedure DescEncodeString(Param: PParamDsc; const str: AnsiString);
var
  len: word;
begin
  len := length(str);
  case Param.dsc_dtype of
    dtype_text    :
      begin
        if len > Param.dsc_length then
          len := Param.dsc_length;
        FillChar(Param.dsc_address^, Param.dsc_length, 0);
        move(PAnsiChar(str)^, Param.dsc_address^, len);
      end;
    dtype_varying :
      begin
        if len > Param.dsc_length - 2 then
          len := Param.dsc_length - 2;
        move(PAnsiChar(str)^, PVary(Param.dsc_address).vary_string, len);
        PVary(Param.dsc_address).vary_length := len;
      end
  end;
end;

procedure DescConvertString(Param: PParamDsc; out value: Integer); overload;
var
  code: integer;
begin
  val(string(DescDecodeString(Param)), value, code);
  if code <> 0 then
    value := 0;
end;

procedure DescConvertString(Param: PParamDsc; out value: Smallint); overload;
var
  code: integer;
begin
  val(string(DescDecodeString(Param)), value, code);
  if code <> 0 then
    value := 0;
end;

procedure DescConvertString(Param: PParamDsc; out value: double); overload;
var
  code: integer;
begin
  val(string(DescDecodeString(Param)), value, code);
  if code <> 0 then
    value := 0;
end;

procedure DescConvertString(Param: PParamDsc; out value: TDateTime); overload;
begin
//todo
end;

procedure DescConvertString(Param: PParamDsc; out value: single); overload;
var
  code: integer;
begin
  val(string(DescDecodeString(Param)), value, code);
  if code <> 0 then
    value := 0;
end;

procedure DescConvertString(Param: PParamDsc; out value: int64); overload;
var
  code: integer;
begin
  val(string(DescDecodeString(Param)), value, code);
  if code <> 0 then
    value := 0;
end;

procedure DescConvertString(Param: PParamDsc; out value: currency); overload;
var
  code: integer;
  d: double;
begin
  val(string(DescDecodeString(Param)), d, code);
  if code = 0 then
    value := d else
    value := 0;
end;

function DescGetIsNull(Param: PParamDsc): boolean;
begin
  Result := ((Param.dsc_flags and DSC_null) <> 0) or
             (Param.dsc_address = nil) or
             (Param.dsc_length = 0);
end;

procedure DescSetIsNull(Param: PParamDsc; Value: boolean);
begin
  case Value of
    true: include(PDescFlag(@Param.dsc_flags)^, 0);
    false: exclude(PDescFlag(@Param.dsc_flags)^, 0);
  end;
end;

function DescGetIsNoSubtype(Param: PParamDsc): boolean;
begin
  Result := ((Param.dsc_flags and DSC_no_subtype) <> 0);
end;

procedure DescSetIsNoSubtype(Param: PParamDsc; Value: boolean);
begin
  case Value of
    true: include(PDescFlag(@Param.dsc_flags)^, 1);
    false: exclude(PDescFlag(@Param.dsc_flags)^, 1);
  end;
end;

function DescGetIsNullable(Param: PParamDsc): boolean;
begin
  Result := ((Param.dsc_flags and DSC_nullable) <> 0);
end;

procedure DescSetIsNullable(Param: PParamDsc; Value: boolean);
begin
  case Value of
    true: include(PDescFlag(@Param.dsc_flags)^, 2);
    false: exclude(PDescFlag(@Param.dsc_flags)^, 2);
  end;
end;


function DescGetFieldType(Param: PParamDsc): TUIBFieldType;
begin
  if (Param.dsc_scale < 0) then
  begin
    if (param.dsc_dtype in [dtype_double, dtype_d_float]) then
      Result := uftDoublePrecision else
      Result := uftNumeric;
  end else
  case Param.dsc_dtype of
    dtype_text      : Result := uftChar;
    dtype_varying   : Result := uftVarchar;
    dtype_short     : Result := uftSmallint;
    dtype_long      : Result := uftInteger;
    dtype_real      : Result := uftFloat;
    dtype_double,
    dtype_d_float   : Result := uftDoublePrecision;
    dtype_timestamp : Result := uftTimestamp;
    dtype_blob      : Result := uftBlob;
    dtype_quad      : Result := uftQuad;
    dtype_sql_time  : Result := uftTime;
    dtype_sql_date  : Result := uftDate;
    dtype_int64     : Result := uftInt64;
    dtype_array     : Result := uftArray;
  else
    Result := uftUnKnown;
  end;
end;

//******************************************************************************
// Standard data types
//******************************************************************************

function DescGetAsInteger(Param: PParamDsc): Integer;
begin
  Result := 0;
  if DescGetIsNull(Param) then Exit;
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : Result := PSmallInt(Param.dsc_address)^ div ScaleDivisor[Param.dsc_scale];
      dtype_long   : Result := PInteger(Param.dsc_address)^  div ScaleDivisor[Param.dsc_scale];
      dtype_int64,
      dtype_quad   : Result := PInt64(Param.dsc_address)^ div ScaleDivisor[Param.dsc_scale];
      dtype_d_float,
      dtype_double : Result := Trunc(PDouble(Param.dsc_address)^);
    else
      Exit; //unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_long      : Result := PInteger(Param.dsc_address)^;
      dtype_int64     : Result := PInt64(Param.dsc_address)^;
      dtype_short     : Result := PSmallint(Param.dsc_address)^;
      dtype_d_float,
      dtype_double    : Result := Trunc(PDouble(Param.dsc_address)^);

      dtype_real      : Result := Trunc(PSingle(Param.dsc_address)^);
      dtype_timestamp : Result := PISCTimeStamp(Param.dsc_address).timestamp_date - DateOffset; // Only Date
      dtype_sql_date  : Result := PInteger(Param.dsc_address)^ - DateOffset;
      dtype_sql_time  : ; // Result := 0; What else ??
      dtype_text,
      dtype_varying   : DescConvertString(Param, result);
    else
      Exit;
    end;
end;

procedure DescSetAsInteger(Param: PParamDsc; Value: Integer);
begin
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : PSmallInt(Param.dsc_address)^ := Value * ScaleDivisor[Param.dsc_scale];
      dtype_long   : PInteger(Param.dsc_address)^  := Value * ScaleDivisor[Param.dsc_scale];
      dtype_int64,
      dtype_quad   : PInt64(Param.dsc_address)^    := Value * ScaleDivisor[Param.dsc_scale];
      dtype_d_float,
      dtype_double : PDouble(Param.dsc_address)^   := Value;
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_d_float,
      dtype_double    : PDouble(Param.dsc_address)^   := Value;
      dtype_timestamp : EncodeTimeStamp(Value, PISCTimeStamp(Param.dsc_address));
      dtype_sql_date  : PInteger(Param.dsc_address)^ := Value + DateOffset;
      dtype_sql_time  : PCardinal(Param.dsc_address)^ := 0;
      dtype_long      : PInteger(Param.dsc_address)^ := Value;
      dtype_real      : PSingle(Param.dsc_address)^ := Value;
      dtype_short     : PSmallint(Param.dsc_address)^ := Value;
      dtype_int64     : PInt64(Param.dsc_address)^ := Value;
      dtype_text,
      dtype_varying   : DescEncodeString(Param, IntToStr(Value));
    else
      Exit; // unexpected
    end;
    DescSetIsNull(Param, false);
end;

function DescGetAsDouble(Param: PParamDsc): Double;
begin
  Result := 0;
  if DescGetIsNull(Param) then Exit;
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : Result := PSmallInt(Param.dsc_address)^ / ScaleDivisor[Param.dsc_scale];
      dtype_long   : Result := PInteger(Param.dsc_address)^  / ScaleDivisor[Param.dsc_scale];
      dtype_int64,
      dtype_quad   : Result := PInt64(Param.dsc_address)^    / ScaleDivisor[Param.dsc_scale];
      dtype_d_float,
      dtype_double : Result := PDouble(Param.dsc_address)^;
    else
      exit; // unexpected;
    end;
  end else
    case Param.dsc_dtype of
      dtype_d_float,
      dtype_double    : Result := PDouble(Param.dsc_address)^;
      dtype_timestamp : DecodeTimeStamp(PISCTimeStamp(Param.dsc_address), Result);
      dtype_sql_date  : Result := PInteger(Param.dsc_address)^ - DateOffset;
      dtype_sql_time  : Result := PCardinal(Param.dsc_address)^ / TimeCoeff;
      dtype_real      : Result := PSingle(Param.dsc_address)^;
      dtype_long      : Result := PInteger(Param.dsc_address)^;
      dtype_short     : Result := PSmallint(Param.dsc_address)^;
      dtype_int64     : Result := PInt64(Param.dsc_address)^;
      dtype_text,
      dtype_varying   : DescConvertString(Param, Result);
    else
      exit; // unexpected;
    end;
end;

procedure DescSetAsDouble(Param: PParamDsc; Value: Double);
begin
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : PSmallInt(Param.dsc_address)^ := Round(Value * ScaleDivisor[Param.dsc_scale]);
      dtype_long   : PInteger(Param.dsc_address)^  := Round(Value * ScaleDivisor[Param.dsc_scale]);
      dtype_int64,
      dtype_quad   : PInt64(Param.dsc_address)^    := Round(Value * ScaleDivisor[Param.dsc_scale]);
      dtype_d_float,
      dtype_double : PDouble(Param.dsc_address)^   := Value;
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_d_float,
      dtype_double    : PDouble(Param.dsc_address)^   := Value;
      dtype_timestamp : EncodeTimeStamp(Value, PISCTimeStamp(Param.dsc_address));
      dtype_sql_date  : PInteger(Param.dsc_address)^ := Round(int(Value)) + DateOffset;
      dtype_sql_time  : PCardinal(Param.dsc_address)^ := Round(Frac(Value) * TimeCoeff);
      dtype_long      : PInteger(Param.dsc_address)^ := Trunc(Value);
      dtype_real      : PSingle(Param.dsc_address)^ := Value;
      dtype_short     : PSmallint(Param.dsc_address)^ := Trunc(Value);
      dtype_int64     : PInt64(Param.dsc_address)^ := Trunc(Value);
      dtype_text,
      dtype_varying   : DescEncodeString(Param, FloatToStr(Value));
    else
      exit;
    end;
    DescSetIsNull(Param, false);
end;

function DescGetAsCurrency(Param: PParamDsc): Currency;
begin
  Result := 0;
  if DescGetIsNull(Param) then Exit;
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_int64,
      dtype_quad   : if (Param.dsc_scale = -4) then
                    PInt64(@Result)^ := PInt64(Param.dsc_address)^ else
                    if Param.dsc_scale > -4 then
                      PInt64(@Result)^ := PInt64(Param.dsc_address)^ * CurrencyDivisor[Param.dsc_scale] else
                      PInt64(@Result)^ := PInt64(Param.dsc_address)^ div CurrencyDivisor[Param.dsc_scale];
      dtype_long   : if (Param.dsc_scale = -4) then
                    PInt64(@Result)^ := PInteger(Param.dsc_address)^ else
                    if Param.dsc_scale > -14 then
                    begin
                      if Param.dsc_scale > -4 then
                        PInt64(@Result)^ := PInteger(Param.dsc_address)^ * Integer(CurrencyDivisor[Param.dsc_scale]) else
                        PInt64(@Result)^ := PInteger(Param.dsc_address)^ div Integer(CurrencyDivisor[Param.dsc_scale]);
                    end else
                    begin
                      if Param.dsc_scale > -4 then
                        PInt64(@Result)^ := PInteger(Param.dsc_address)^ * CurrencyDivisor[Param.dsc_scale] else
                        PInt64(@Result)^ := PInteger(Param.dsc_address)^ div CurrencyDivisor[Param.dsc_scale];
                    end;
      dtype_short  : if (Param.dsc_scale = -4) then
                    PInt64(@Result)^ := PSmallint(Param.dsc_address)^ else
                    if Param.dsc_scale > -14 then
                    begin
                      if Param.dsc_scale > -4 then
                        PInt64(@Result)^ := PSmallint(Param.dsc_address)^ * Integer(CurrencyDivisor[Param.dsc_scale]) else
                        PInt64(@Result)^ := PSmallint(Param.dsc_address)^ div Integer(CurrencyDivisor[Param.dsc_scale]);
                    end else
                    begin
                      if Param.dsc_scale > -4 then
                        PInt64(@Result)^ := PSmallint(Param.dsc_address)^ * CurrencyDivisor[Param.dsc_scale] else
                        PInt64(@Result)^ := PSmallint(Param.dsc_address)^ div CurrencyDivisor[Param.dsc_scale];
                    end;
      dtype_d_float,
      dtype_double : Result := PDouble(Param.dsc_address)^;
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_d_float,
      dtype_double    : Result := PDouble(Param.dsc_address)^;
      dtype_real      : Result := PSingle(Param.dsc_address)^;
      dtype_timestamp : Result := DecodeTimeStamp(PISCTimeStamp(Param.dsc_address));
      dtype_sql_date  : Result := PInteger(Param.dsc_address)^ - DateOffset;
      dtype_sql_time  : Result := PCardinal(Param.dsc_address)^ / TimeCoeff;
      dtype_long      : Result := PInteger(Param.dsc_address)^;
      dtype_short     : Result := PSmallint(Param.dsc_address)^;
      dtype_int64     : Result := PInt64(Param.dsc_address)^;
      dtype_text,
      dtype_varying   : DescConvertString(Param, result);
    else
      exit;
    end;
end;


procedure DescSetAsCurrency(Param: PParamDsc; const Value: Currency);
begin
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : PSmallInt(Param.dsc_address)^ := Round(Value * ScaleDivisor[Param.dsc_scale]);
      dtype_long   : PInteger(Param.dsc_address)^  := Round(Value * ScaleDivisor[Param.dsc_scale]);
      dtype_int64,
      dtype_quad   : if (Param.dsc_scale = -4) then
                     PInt64(Param.dsc_address)^ := PInt64(@Value)^ else
                     if Param.dsc_scale > -4 then
                       PInt64(Param.dsc_address)^ := PInt64(@Value)^ div CurrencyDivisor[Param.dsc_scale] else
                       PInt64(Param.dsc_address)^ := PInt64(@Value)^ * CurrencyDivisor[Param.dsc_scale];
      dtype_d_float,
      dtype_double : PDouble(Param.dsc_address)^   := Value;
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_d_float,
      dtype_double    : PDouble(Param.dsc_address)^   := Value;
      dtype_timestamp : EncodeTimeStamp(Value, PISCTimeStamp(Param.dsc_address));
      dtype_sql_date  : PInteger(Param.dsc_address)^ := Round(int(Value) + DateOffset);
      dtype_sql_time  : PCardinal(Param.dsc_address)^ := Round(Frac(Value) * TimeCoeff);
      dtype_long      : PInteger(Param.dsc_address)^ := Trunc(Value);
      dtype_real      : PSingle(Param.dsc_address)^ := Value;
      dtype_short     : PSmallint(Param.dsc_address)^ := Trunc(Value);
      dtype_int64     : PInt64(Param.dsc_address)^ := Trunc(Value);
      dtype_text,
      dtype_varying   : DescEncodeString(Param, CurrToStr(Value));
    else
      exit;
    end;
    DescSetIsNull(Param, false);
end;

function DescGetAsInt64(Param: PParamDsc): Int64;
begin
  Result := 0;
  if DescGetIsNull(Param) then Exit;
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : Result := PSmallInt(Param.dsc_address)^ div ScaleDivisor[Param.dsc_scale];
      dtype_long   : Result := PInteger(Param.dsc_address)^  div ScaleDivisor[Param.dsc_scale];
      dtype_int64,
      dtype_quad   : Result := PInt64(Param.dsc_address)^ div ScaleDivisor[Param.dsc_scale];
      dtype_d_float,
      dtype_double : Result := Trunc(PDouble(Param.dsc_address)^);
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_int64     : Result := PInt64(Param.dsc_address)^;
      dtype_long      : Result := PInteger(Param.dsc_address)^;
      dtype_short     : Result := PSmallint(Param.dsc_address)^;
      dtype_d_float,
      dtype_double    : Result := Trunc(PDouble(Param.dsc_address)^);
      dtype_timestamp : Result := PISCTimeStamp(Param.dsc_address).timestamp_date - DateOffset; // Only Date
      dtype_sql_date  : Result := PInteger(Param.dsc_address)^ - DateOffset;
      dtype_sql_time  : ; // Result := 0; What else ??
      dtype_real      : Result := Trunc(PSingle(Param.dsc_address)^);
      dtype_text,
      dtype_varying   : DescConvertString(Param, Result);
    else
      exit;
    end;
end;

procedure DescSetAsInt64(Param: PParamDsc; const Value: Int64);
begin
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : PSmallInt(Param.dsc_address)^ := Value * ScaleDivisor[Param.dsc_scale];
      dtype_long   : PInteger(Param.dsc_address)^  := Value * ScaleDivisor[Param.dsc_scale];
      dtype_int64,
      dtype_quad   : PInt64(Param.dsc_address)^    := Value * ScaleDivisor[Param.dsc_scale];
      dtype_d_float,
      dtype_double : PDouble(Param.dsc_address)^   := Value;
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_d_float,
      dtype_double    : PDouble(Param.dsc_address)^   := Value;
      dtype_timestamp : EncodeTimeStamp(Integer(Value), PISCTimeStamp(Param.dsc_address));
      dtype_sql_date  : PInteger(Param.dsc_address)^ := Value + DateOffset;
      dtype_sql_time  : PCardinal(Param.dsc_address)^ := 0;
      dtype_long      : PInteger(Param.dsc_address)^ := Value;
      dtype_real      : PSingle(Param.dsc_address)^ := Value;
      dtype_short     : PSmallint(Param.dsc_address)^ := Value;
      dtype_int64     : PInt64(Param.dsc_address)^ := Value;
      dtype_text,
      dtype_varying   : DescEncodeString(Param, Int64ToStr(Value));
    else
      exit;
    end;
    DescSetIsNull(Param, false);
end;

function DescGetAsSingle(Param: PParamDsc): Single;
begin
  Result := 0;
  if DescGetIsNull(Param) then Exit;
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : Result := PSmallInt(Param.dsc_address)^ / ScaleDivisor[Param.dsc_scale];
      dtype_long   : Result := PInteger(Param.dsc_address)^  / ScaleDivisor[Param.dsc_scale];
      dtype_int64,
      dtype_quad   : Result := PInt64(Param.dsc_address)^    / ScaleDivisor[Param.dsc_scale];
      dtype_d_float,
      dtype_double : Result := PDouble(Param.dsc_address)^;
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_real     : Result := PSingle(Param.dsc_address)^;
      dtype_d_float,
      dtype_double    : Result := PDouble(Param.dsc_address)^;
      dtype_timestamp : Result := DecodeTimeStamp(PISCTimeStamp(Param.dsc_address));
      dtype_sql_date  : Result := PInteger(Param.dsc_address)^ - DateOffset;
      dtype_sql_time  : Result := PCardinal(Param.dsc_address)^ / TimeCoeff;
      dtype_long      : Result := PInteger(Param.dsc_address)^;
      dtype_short     : Result := PSmallint(Param.dsc_address)^;
      dtype_int64     : Result := PInt64(Param.dsc_address)^;
      dtype_text,
      dtype_varying   : DescConvertString(Param, result);
    else
      exit;
    end;
end;

procedure DescSetAsSingle(Param: PParamDsc; const Value: Single);
begin
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : PSmallInt(Param.dsc_address)^ := Round(Value * ScaleDivisor[Param.dsc_scale]);
      dtype_long   : PInteger(Param.dsc_address)^  := Round(Value * ScaleDivisor[Param.dsc_scale]);
      dtype_int64,
      dtype_quad   : PInt64(Param.dsc_address)^    := Round(Value * ScaleDivisor[Param.dsc_scale]);
      dtype_d_float,
      dtype_double : PDouble(Param.dsc_address)^   := Value;
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_d_float,
      dtype_double    : PDouble(Param.dsc_address)^   := Value;
      dtype_timestamp : EncodeTimeStamp(Value, PISCTimeStamp(Param.dsc_address));
      dtype_sql_date  : PInteger(Param.dsc_address)^ := Round(int(Value)) + DateOffset;
      dtype_sql_time  : PCardinal(Param.dsc_address)^ := Round(Frac(Value) * TimeCoeff);
      dtype_long      : PInteger(Param.dsc_address)^ := Trunc(Value);
      dtype_real      : PSingle(Param.dsc_address)^ := Value;
      dtype_short     : PSmallint(Param.dsc_address)^ := Trunc(Value);
      dtype_int64     : PInt64(Param.dsc_address)^ := Trunc(Value);
      dtype_text,
      dtype_varying   : DescEncodeString(Param, SingToStr(Value));
    else
      exit;
    end;
    DescSetIsNull(Param, false);
end;

function DescGetAsSmallint(Param: PParamDsc): Smallint;
begin
  Result := 0;
  if DescGetIsNull(Param) then Exit;
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : Result := PSmallInt(Param.dsc_address)^ div ScaleDivisor[Param.dsc_scale];
      dtype_long   : Result := PInteger(Param.dsc_address)^  div ScaleDivisor[Param.dsc_scale];
      dtype_int64,
      dtype_quad   : Result := PInt64(Param.dsc_address)^ div ScaleDivisor[Param.dsc_scale];
      dtype_d_float,
      dtype_double : Result := Trunc(PDouble(Param.dsc_address)^);
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_short     : Result := PSmallint(Param.dsc_address)^;
      dtype_long      : Result := PInteger(Param.dsc_address)^;
      dtype_int64     : Result := PInt64(Param.dsc_address)^;
      dtype_d_float,
      dtype_double    : Result := Trunc(PDouble(Param.dsc_address)^);
      dtype_real     : Result := Trunc(PSingle(Param.dsc_address)^);
      dtype_timestamp : Result := PISCTimeStamp(Param.dsc_address).timestamp_date - DateOffset; // Only Date
      dtype_sql_date : Result := PInteger(Param.dsc_address)^ - DateOffset;
      dtype_text,
      dtype_varying   : DescConvertString(Param, result);
      dtype_sql_time : ; // Result := 0; What else ??
    else
      exit;
    end;
end;

procedure DescSetAsSmallint(Param: PParamDsc; const Value: Smallint);
begin
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : PSmallInt(Param.dsc_address)^ := Value * ScaleDivisor[Param.dsc_scale];
      dtype_long   : PInteger(Param.dsc_address)^  := Value * ScaleDivisor[Param.dsc_scale];
      dtype_int64,
      dtype_quad   : PInt64(Param.dsc_address)^    := Value * ScaleDivisor[Param.dsc_scale];
      dtype_d_float,
      dtype_double : PDouble(Param.dsc_address)^   := Value;
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_d_float,
      dtype_double    : PDouble(Param.dsc_address)^   := Value;
      dtype_timestamp : EncodeTimeStamp(Value, PISCTimeStamp(Param.dsc_address));
      dtype_sql_date : PInteger(Param.dsc_address)^ := Value + DateOffset;
      dtype_sql_time : PCardinal(Param.dsc_address)^ := 0;
      dtype_long      : PInteger(Param.dsc_address)^ := Value;
      dtype_real     : PSingle(Param.dsc_address)^ := Value;
      dtype_short     : PSmallint(Param.dsc_address)^ := Value;
      dtype_int64     : PInt64(Param.dsc_address)^ := Value;
      dtype_text,
      dtype_varying   : DescEncodeString(Param, SmallToStr(Value));
    else
      exit;
    end;
    DescSetIsNull(Param, false);
end;

function DescGetAsString(Param: PParamDsc): AnsiString;
  function BoolToStr(const Value: boolean): AnsiString;
  begin if Value then result := sUIBTrue else result := sUIBFalse; end;
begin
  Result := '';
  if DescGetIsNull(Param) then Exit;
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : Result := FloatToStr(PSmallInt(Param.dsc_address)^ / ScaleDivisor[Param.dsc_scale]);
      dtype_long   : Result := FloatToStr(PInteger(Param.dsc_address)^  / ScaleDivisor[Param.dsc_scale]);
      dtype_int64,
      dtype_quad   : Result := FloatToStr(PInt64(Param.dsc_address)^    / ScaleDivisor[Param.dsc_scale]);
      dtype_d_float,
      dtype_double : Result := FloatToStr(PDouble(Param.dsc_address)^);
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_varying,
      dtype_text      : DescDecodeString(Param, Result);
      dtype_timestamp : Result := TimeStampToSQLStr(PISCTimeStamp(Param.dsc_address));
      dtype_sql_date : Result := DateToSQLStr(PInteger(Param.dsc_address)^);
      dtype_sql_time : Result := TimeToSQLStr(PCardinal(Param.dsc_address)^);
      dtype_d_float,
      dtype_double    : Result := FloatToStr(PDouble(Param.dsc_address)^);
      dtype_long      : Result := IntToStr(PInteger(Param.dsc_address)^);
      dtype_real     : Result := FloatToStr(PSingle(Param.dsc_address)^);
      dtype_short     : Result := SmallToStr(PSmallint(Param.dsc_address)^);
      dtype_int64     : Result := Int64ToStr(PInt64(Param.dsc_address)^);
    else
      exit;
    end;
end;


procedure DescSetAsString(Param: PParamDsc; const Value: AnsiString);
begin
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : PSmallInt(Param.dsc_address)^ := Trunc(StrToFloat(Value) * ScaleDivisor[Param.dsc_scale]);
      dtype_long   : PInteger(Param.dsc_address)^  := Trunc(StrToFloat(Value) * ScaleDivisor[Param.dsc_scale]);
      dtype_int64,
      dtype_quad   : PInt64(Param.dsc_address)^    := Trunc(StrToFloat(Value) * ScaleDivisor[Param.dsc_scale]);
      dtype_d_float,
      dtype_double : PDouble(Param.dsc_address)^   := StrToFloat(Value);
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_d_float,
      dtype_double    : PDouble(Param.dsc_address)^   := StrToFloat(Value);
      dtype_timestamp : EncodeSQLStrDateTime(Value, PISCTimeStamp(Param.dsc_address));
      dtype_sql_date : EncodeSQLStrDate(Value, PInteger(Param.dsc_address)^);
      dtype_sql_time : EncodeSQLStrTime(Value, PCardinal(Param.dsc_address)^);
      dtype_long      : PInteger(Param.dsc_address)^ := StrToInt(Value);
      dtype_real     : PSingle(Param.dsc_address)^ := StrToSing(Value);
      dtype_short     : PSmallint(Param.dsc_address)^ := StrToSmall(Value);
      dtype_int64     : PInt64(Param.dsc_address)^ := StrToInt64(Value);
      dtype_text,
      dtype_varying   : DescEncodeString(Param, Value);
    else
      exit;
    end;
    DescSetIsNull(Param, false);
end;

function DescGetAsDateTime(Param: PParamDsc): TDateTime;
begin
  Result := 0;
  if DescGetIsNull(Param) then Exit;
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : Result := PSmallInt(Param.dsc_address)^ / ScaleDivisor[Param.dsc_scale];
      dtype_long   : Result := PInteger(Param.dsc_address)^  / ScaleDivisor[Param.dsc_scale];
      dtype_int64,
      dtype_quad   : Result := PInt64(Param.dsc_address)^    / ScaleDivisor[Param.dsc_scale];
      dtype_d_float,
      dtype_double : Result := PDouble(Param.dsc_address)^;
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_timestamp : DecodeTimeStamp(PISCTimeStamp(Param.dsc_address), Double(Result));
      dtype_sql_date  : Result := PInteger(Param.dsc_address)^ - DateOffset;
      dtype_sql_time  : Result := PCardinal(Param.dsc_address)^ / TimeCoeff;
      dtype_d_float,
      dtype_double    : Result := PDouble(Param.dsc_address)^;
      dtype_real      : Result := PSingle(Param.dsc_address)^;
      dtype_long      : Result := PInteger(Param.dsc_address)^;
      dtype_int64     : Result := PInt64(Param.dsc_address)^;
      dtype_short     : Result := PSmallint(Param.dsc_address)^;
      dtype_text,
      dtype_varying   : DescConvertString(Param, result);
    else
      exit;
    end;
end;

procedure DescSetAsDateTime(Param: PParamDsc;
  const Value: TDateTime);
begin
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : PSmallInt(Param.dsc_address)^ := Round(Value * ScaleDivisor[Param.dsc_scale]);
      dtype_long   : PInteger(Param.dsc_address)^  := Round(Value * ScaleDivisor[Param.dsc_scale]);
      dtype_int64,
      dtype_quad   : PInt64(Param.dsc_address)^    := Round(Value * ScaleDivisor[Param.dsc_scale]);
      dtype_d_float,
      dtype_double : PDouble(Param.dsc_address)^   := Value;
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_d_float,
      dtype_double    : PDouble(Param.dsc_address)^   := Value;
      dtype_timestamp : EncodeTimeStamp(Value, PISCTimeStamp(Param.dsc_address));
      dtype_sql_date : PInteger(Param.dsc_address)^ := Round(int(Value)) + DateOffset;
      dtype_sql_time : PCardinal(Param.dsc_address)^ := Round(Frac(Value) * TimeCoeff);
      dtype_long      : PInteger(Param.dsc_address)^ := Trunc(Value);
      dtype_real     : PSingle(Param.dsc_address)^ := Value;
      dtype_short     : PSmallint(Param.dsc_address)^ := Trunc(Value);
      dtype_int64     : PInt64(Param.dsc_address)^ := Trunc(Value);
      dtype_text,
      dtype_varying   : DescEncodeString(Param, DateTimeToSQLStr(Value));
    else
      exit;
    end;
    DescSetIsNull(Param, false);
end;

function DescGetAsDate(Param: PParamDsc): Integer;
begin
  Result := 0;
  if DescGetIsNull(Param) then Exit;
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : Result := PSmallInt(Param.dsc_address)^ div ScaleDivisor[Param.dsc_scale];
      dtype_long   : Result := PInteger(Param.dsc_address)^  div ScaleDivisor[Param.dsc_scale];
      dtype_int64,
      dtype_quad   : Result := PInt64(Param.dsc_address)^ div ScaleDivisor[Param.dsc_scale];
      dtype_d_float,
      dtype_double : Result := Trunc(PDouble(Param.dsc_address)^);
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_sql_date : Result := PInteger(Param.dsc_address)^ - DateOffset;
      dtype_timestamp : Result := PISCTimeStamp(Param.dsc_address).timestamp_date - DateOffset;
      dtype_d_float,
      dtype_double    : Result := Trunc(PDouble(Param.dsc_address)^);
      dtype_real      : Result := Trunc(PSingle(Param.dsc_address)^);
      dtype_int64     : Result := PInt64(Param.dsc_address)^;
      dtype_long      : Result := PInteger(Param.dsc_address)^;
      dtype_short     : Result := PSmallint(Param.dsc_address)^;
      dtype_text,
      dtype_varying   : EncodeSQLStrDate(DescDecodeString(Param), result);
      dtype_sql_time  : Result := 0;
    else
      exit;
    end;
end;


procedure DescSetAsDate(Param: PParamDsc; const Value: Integer);
begin
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : PSmallInt(Param.dsc_address)^ := Value * ScaleDivisor[Param.dsc_scale];
      dtype_long   : PInteger(Param.dsc_address)^  := Value * ScaleDivisor[Param.dsc_scale];
      dtype_int64,
      dtype_quad   : PInt64(Param.dsc_address)^    := Value * ScaleDivisor[Param.dsc_scale];
      dtype_d_float,
      dtype_double : PDouble(Param.dsc_address)^   := Value;
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_d_float,
      dtype_double    : PDouble(Param.dsc_address)^   := Value;
      dtype_timestamp : EncodeTimeStamp(Value, PISCTimeStamp(Param.dsc_address));
      dtype_sql_date  : PInteger(Param.dsc_address)^ := Value + DateOffset;
      dtype_sql_time  : PCardinal(Param.dsc_address)^ := 0;
      dtype_long      : PInteger(Param.dsc_address)^ := Value;
      dtype_real      : PSingle(Param.dsc_address)^ := Value;
      dtype_short     : PSmallint(Param.dsc_address)^ := Value;
      dtype_int64     : PInt64(Param.dsc_address)^ := Value;
      dtype_text,
      dtype_varying   : DescEncodeString(Param, DateToSQLStr(Value));
    else
      exit;
    end;
    DescSetIsNull(Param, false);
end;

function DescGetAsTime(Param: PParamDsc): Cardinal;
begin
  Result := 0;
  if DescGetIsNull(Param) then Exit;
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : Result := PSmallInt(Param.dsc_address)^ div ScaleDivisor[Param.dsc_scale];
      dtype_long   : Result := PInteger(Param.dsc_address)^  div ScaleDivisor[Param.dsc_scale];
      dtype_int64,
      dtype_quad   : Result := PInt64(Param.dsc_address)^ div ScaleDivisor[Param.dsc_scale];
      dtype_d_float,
      dtype_double : Result := Trunc(PDouble(Param.dsc_address)^);
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_sql_time : Result := PCardinal(Param.dsc_address)^;
      dtype_timestamp : Result := PISCTimeStamp(Param.dsc_address).timestamp_time;
      dtype_d_float,
      dtype_double    : Result := Trunc(PDouble(Param.dsc_address)^);
      dtype_long      : Result := PInteger(Param.dsc_address)^;
      dtype_real      : Result := Trunc(PSingle(Param.dsc_address)^);
      dtype_short     : Result := PSmallint(Param.dsc_address)^;
      dtype_int64     : Result := PInt64(Param.dsc_address)^;
      dtype_text,
      dtype_varying   : EncodeSQLStrTime(DescDecodeString(Param), result);
      dtype_sql_date  : Result := 0;
    else
      exit;
    end;
end;


procedure DescSetAsTime(Param: PParamDsc; const Value: Cardinal);
begin
  // Is Numeric ?
  if (Param.dsc_scale < 0)  then
  begin
    case Param.dsc_dtype of
      dtype_short  : PSmallInt(Param.dsc_address)^ := Value * ScaleDivisor[Param.dsc_scale];
      dtype_long   : PInteger(Param.dsc_address)^  := Value * ScaleDivisor[Param.dsc_scale];
      dtype_int64,
      dtype_quad   : PInt64(Param.dsc_address)^    := Value * ScaleDivisor[Param.dsc_scale];
      dtype_d_float,
      dtype_double : PDouble(Param.dsc_address)^   := Value;
    else
      exit; // unexpected
    end;
  end else
    case Param.dsc_dtype of
      dtype_d_float,
      dtype_double    : PDouble(Param.dsc_address)^   := Value;
      dtype_timestamp : EncodeTimeStamp(Value, PISCTimeStamp(Param.dsc_address));
      dtype_sql_date : PInteger(Param.dsc_address)^ := 0;
      dtype_sql_time : PCardinal(Param.dsc_address)^ := Value;
      dtype_long      : PInteger(Param.dsc_address)^ := Value;
      dtype_real     : PSingle(Param.dsc_address)^ := Value;
      dtype_short     : PSmallint(Param.dsc_address)^ := Value;
      dtype_int64     : PInt64(Param.dsc_address)^ := Value;
      dtype_text,
      dtype_varying   : DescEncodeString(Param, TimeToSQLStr(Value));
    else
      exit;
    end;
    DescSetIsNull(Param, false);
end;

//******************************************************************************
// BLOBS
//******************************************************************************

function DescBlobGetSegment(Param: PBlobCallBack; out length: Word;
  BufferLength: Cardinal; Buffer: Pointer): boolean;
var
  AStatus: ISCStatus;
begin
  if BufferLength > High(Word) then
    BufferLength := High(Word);
    AStatus := Param.blob_get_segment(Param.blob_handle, Buffer, Word(BufferLength), length);
  Result := (AStatus <> 0);
end;

procedure DescBlobReadString(Param: PBlobCallBack; var Str: AnsiString);
var
  CurrentLength: Word;
  Buffer: Pointer;
  Len: Cardinal;
begin
  SetLength(Str, Param.blob_total_length);
  Buffer := PAnsiChar(Str);
  len := 0;
  while DescBlobGetSegment(Param, CurrentLength, Cardinal(Param.blob_total_length) - len, Buffer) do
  begin
    inc(PtrInt(Buffer), CurrentLength);
    inc(len, CurrentLength);
    if len = Cardinal(Param.blob_total_length) then
      Break;
  end;
end;

procedure DescBlobSaveToStream(Param: PBlobCallBack; Stream: TStream);
var
  Buffer: Pointer;
  CurrentLength: Word;
begin
  Stream.Seek(0, soFromBeginning);
  Getmem(Buffer, Param.blob_max_segment);
  try
    while DescBlobGetSegment(Param, CurrentLength, Param.blob_max_segment, Buffer) do
      Stream.Write(Buffer^, CurrentLength);
  finally
    FreeMem(Buffer);
  end;
  Stream.Seek(0, soFromBeginning);
end;

procedure DescBlobWriteSegment(Param: PBlobCallBack; BufferLength: Cardinal; Buffer: Pointer);
var
  size: Word;
begin
  while BufferLength > 0 do
  begin
    if BufferLength > DefaultSegmentSize then
      size := DefaultSegmentSize else
      size := Word(BufferLength);

    Param.blob_put_segment(Param.blob_handle, Buffer, Size);
    dec(BufferLength, size);
    inc(PByte(Buffer), size);
  end;
end;

procedure DescBlobWriteString(Param: PBlobCallBack; var Str: AnsiString);
begin
  DescBlobWriteSegment(Param, Length(Str), PAnsiChar(Str));
end;

procedure DescBlobWriteStream(Param: PBlobCallBack; Stream: TStream);
var
  Buffer: PAnsiChar;
begin
  Stream.Seek(0, soFromBeginning);
  if Stream is TCustomMemoryStream then
    DescBlobWriteSegment(Param, Cardinal(TCustomMemoryStream(Stream).Size),
      TCustomMemoryStream(Stream).Memory) else

  begin
    GetMem(Buffer, Cardinal(Stream.Size));
    try
      Stream.Read(Buffer^, Cardinal(Stream.Size));
      DescBlobWriteSegment(Param, Cardinal(Stream.Size), Buffer);
      Stream.Seek(0, soFromBeginning);
    finally
      FreeMem(buffer);
    end;
  end;
end;

procedure DescBlobReadSizedBuffer(Param: PBlobCallBack; Buffer: Pointer);
var
  CurrentLength: Word;
  TMP: Pointer;
  Len: Integer;
begin
  TMP := Buffer;
  Len := 0;
  while DescBlobGetSegment(Param, CurrentLength, Param.blob_total_length - len, TMP) do
  begin
    inc(PtrInt(TMP), CurrentLength);
    inc(Len, CurrentLength);
    if len = Param.blob_total_length then
      break;
  end;
end;

{$ENDIF}

end.
