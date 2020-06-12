/// `buffer` module support bindings for SyNode
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNodeBinding_buffer;

interface
{$I Synopse.inc}
{$I SyNode.inc}
{$UNDEF HASINLINE}
type
  TEncoding = (ASCII, UTF8, BASE64, UCS2, BINARY, HEX, BUFFER);
  TEndianness = (kLittleEndian, kBigEndian);
// BINARY is a deprecated alias of LATIN1.
const  LATIN1 = BINARY;
implementation
uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SynCommons,
  SyNode,
  SpiderMonkey;

//const
//  BUFFER_PROTO_SLOT = JSCLASS_GLOBAL_SLOT_COUNT + 1;


const
  _Endianness: record
  case boolean of
    true: (u8: array[0..1] of UInt8);
    false: (u16: uint16);
  end = (u8: (1, 0));

var
  Endianness: TEndianness;

const kMaxLength = (1 shl 28) - 16;

const OUT_OF_RANGE = 'out of range index';
      NOT_A_HEX = 'Not a hex';

procedure getBufDataAndLength(cx: PJSContext; bufObj: PJSRootedObject; out bufData: pointer; out  bufLen: size_t);{$ifdef HASINLINE}inline;{$endif}
const MUST_BE_A_BUFFER = 'argument should be a Buffer';
begin
  if not bufObj.ptr.IsTypedArrayObject then
    raise ESMTypeException.Create(MUST_BE_A_BUFFER);
  bufData := Pointer(bufObj.ptr.GetUint8ArrayData);{ + bufObj.ptr.GetTypedArrayByteOffset}
  bufLen := bufObj.ptr.GetTypedArrayByteLength;
end;

function ParseEncoding(const encoding: RawUTF8; default_encoding: TEncoding): TEncoding; {$ifdef HASINLINE}inline;{$endif}
begin
  Result := default_encoding;
  case Length(encoding) of
    3: if IdemPChar(Pointer(encoding), 'HEX') then
         Result := HEX;
    4: if IdemPChar(Pointer(encoding), 'UTF8') then
         Result := UTF8
       else if IdemPChar(Pointer(encoding), 'UCS2') then
         Result := UCS2;
    5: if IdemPChar(Pointer(encoding), 'UTF-8') then
         Result := UTF8
       else if IdemPChar(Pointer(encoding), 'UCS-2') then
         Result := UCS2
       else if IdemPChar(Pointer(encoding), 'ASCII') then
         Result := ASCII;
    6: if IdemPChar(Pointer(encoding), 'BASE64') then
         Result := BASE64
       else if IdemPChar(Pointer(encoding), 'LATIN1') then
         Result := LATIN1
       else if IdemPChar(Pointer(encoding), 'BINARY') then
         Result := LATIN1 // BINARY is a deprecated alias of LATIN1.
       else if IdemPChar(Pointer(encoding), 'BUFFER') then
         Result := BUFFER;
    7: if IdemPChar(Pointer(encoding), 'UTF16LE') then
         Result := UCS2;
    8: if IdemPChar(Pointer(encoding), 'UTF16-LE') then
         Result := UCS2;
  end;
end;

function StringSlice(cx: PJSContext; argc: uintN; var vp: jsargRec; encoding: TEncoding): Boolean; {$ifdef HASINLINE}inline;{$endif}
var
  bufData: pointer;
  length: size_t;
  tmp_str: RawByteString;
  p: PAnsiChar;
  in_argv: PjsvalVector;
  start: size_t;
  end_: size_t;
  bufLen: size_t;
  this: PJSRootedObject;
const
  TWO_ARGRUMENTS_EXPECTED = '2 arguments expected';
begin
  try
    Result := True;
    this := cx.NewRootedObject(vp.thisObject[cx]);
    try
      getBufDataAndLength(cx, this, bufData, bufLen);
    finally
      cx.FreeRootedObject(this);
    end;
    if bufLen = 0 then begin
{$IFDEF SM52}
      vp.rval := cx.EmptyString.ToJSVal;
{$ELSE}
      vp.rval := cx.rt.EmptyString.ToJSVal;
{$ENDIF}
      Exit;
    end;

    if argc < 2 then
      raise ESMException.Create(TWO_ARGRUMENTS_EXPECTED);

    in_argv := vp.argv;
    if in_argv[0].isNumber then
      start := in_argv[0].asInt64
    else
      start := 0;

    bufData := PAnsiChar(bufData) + start; //Pointer(UIntPtr(bufData) + start);

    if in_argv[1].isNumber then
      end_ := in_argv[1].asInt64
    else
      end_ := bufLen;
    if end_ < start then
      end_ := start;
    if end_ > bufLen then
      raise ESMRangeException.Create(OUT_OF_RANGE);
    length := end_ - start;
    case encoding of
    // todo: check ascii(and $7F)
      ASCII: begin
        SetString(tmp_str, PAnsiChar(bufData), length);
        p := pointer(tmp_str);
        while length >= 4 do begin
          PCardinal(p)^ := PCardinal(p)^ and $7F7F7F7F;
          Inc(p,4);
          dec(length, 4);
        end;
        if length >= 2 then begin
          PWord(p)^ := PWord(p)^ and $7F7F;
          Inc(p,2);
          dec(length, 2);
        end;
        if length >= 1 then begin
          PByte(p)^ := PByte(p)^ and $7F;
          Inc(p,1);
        end;

        vp.rval := JS_NewStringCopyN(cx, Pointer(tmp_str), System.length(tmp_str)).ToJSVal;
      end;

      LATIN1: vp.rval := JS_NewStringCopyN(cx, bufData, length).ToJSVal;
      UTF8: vp.rval := cx.NewJSString(bufData, length, CP_UTF8).ToJSVal;
      BASE64: vp.rval := cx.NewJSString(BinToBase64(bufData, length)).ToJSVal;
      //todo: optimize
      HEX: vp.rval := cx.NewJSString(LowerCase(BinToHex(bufData, length))).ToJSVal;
      UCS2: vp.rval := cx.NewJSString(bufData, length div 2).ToJSVal;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function cpSlice(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  bufData: pointer;
  length: size_t;
  in_argv: PjsvalVector;
  start: size_t;
  end_: size_t;
  bufLen: size_t;
  this: PJSRootedObject;
  destCP: integer;
const
  USAGE = 'cpSlice(start, length, cp: integer)';
begin
  try
    Result := True;
    this := cx.NewRootedObject(vp.thisObject[cx]);
    try
      getBufDataAndLength(cx, this, bufData, bufLen);
    finally
      cx.FreeRootedObject(this);
    end;
    if bufLen = 0 then begin
{$IFDEF SM52}
      vp.rval := cx.EmptyString.ToJSVal;
{$ELSE}
      vp.rval := cx.rt.EmptyString.ToJSVal;
{$ENDIF}
      Exit;
    end;

    if argc < 3 then
      raise ESMException.Create(USAGE);

    in_argv := vp.argv;
    if in_argv[0].isNumber then
      start := in_argv[0].asInt64
    else
      start := 0;

    bufData := PAnsiChar(bufData) + start;

    if in_argv[1].isNumber then
      end_ := in_argv[1].asInt64
    else
      end_ := bufLen;

    if end_ < start then
      end_ := start;
    if end_ > bufLen then
      raise ESMRangeException.Create(OUT_OF_RANGE);
    length := end_ - start;

    if in_argv[2].isNumber then
      destCP := in_argv[2].asInteger
    else
      destCP := CP_UTF8;
    vp.rval := cx.NewJSString(bufData, length, destCP).ToJSVal;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function asciiSlice(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := StringSlice(cx, argc, vp, ASCII);
end;

function latin1Slice(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := StringSlice(cx, argc, vp, LATIN1);
end;

function utf8Slice(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := StringSlice(cx, argc, vp, UTF8);
end;

function base64Slice(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := StringSlice(cx, argc, vp, BASE64);
end;

function hexSlice(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := StringSlice(cx, argc, vp, HEX);
end;

function ucs2Slice(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := StringSlice(cx, argc, vp, UCS2);
end;

function isFirstByteInChar(value: byte): boolean; {$ifdef HASINLINE}inline;{$endif}
//0xxxxxxx or 11xxxxxx
begin
  result := (value < 128) or (value >= 192);
end;

/// This array is a lookup table that translates AnsiChar characters drawn from the "Base64 Alphabet" (as specified
// in Table 1 of RFC 2045) into their 6-bit positive integer equivalents. Characters that are not in the Base64
// alphabet but fall within the bounds of the array are translated to -1.
//
// Whitespaces are translated to -2.
//
// Note: '+' and '-' both decode to 62. '/' and '_' both decode to 63. This means decoder seamlessly handles both
// URL_SAFE and STANDARD base64.
//
// Thanks to "commons" project in ws.apache.org for this code.
// http://svn.apache.org/repos/asf/webservices/commons/trunk/modules/util/
const
  DECODE_TABLE: array[AnsiChar] of shortint = (
//   0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -2, -1, -1, -2, -1, -1, // 00-0f
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 10-1f
    -2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, 62, -1, 63, // 20-2f + - /
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1, // 30-3f 0-9
    -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, // 40-4f A-O
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, 63, // 50-5f P-Z _
    -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, // 60-6f a-o
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1, // 70-7a p-z
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
);

/// Conversion from URL_SAFE and STANDARD base64 encoded text into binary data with features:
// - padding is optional
// - whitespaces (' ', \r, \n) are skipped to be compartible with
//   transfer encoding for MIME (RFC 2045);
//   for example SOAP/E-Mail use \r\n to split long Base64 string into parts
// - will decode up to maxBinLength into bin buffer
// - can optinaly accept a 2 bytes representation of str (0xC1 0x0 0xC2 0x0 ...) as
//   returned by SpiderMonkey engine GetTwoByteStringCharsAndLength function.
//   In this case strLen is duplet count
// Return length of resulting bin or 0 in case of invalid Base64 input
//
// To estimate a length of result buffer function should be called as
//   estimatedLength := anyBase64ToBin(str, strLen, nil, strLen);
function anyBase64ToBin(str: PAnsiChar; strLen: PtrInt;
  bin: PAnsiChar; maxBinLength: PtrInt;
  strIsTwoBytes: boolean = false): PtrInt;
var
  maxS, maxB, bPos, sPos: PtrInt;
  v: uint32;
  c: AnsiChar;
  mul: byte;

  function failTolerantDecode: boolean;
  var lo, hi: smallint;
  begin
    repeat
      c := str[sPos]; lo := DECODE_TABLE[c];
      inc(sPos, 1 shl mul);
      if (lo >= 0) then break; // Legal character
      if ((c = '=') or (sPos >= strLen)) then
        exit(false); // Stop decoding
    until false;
    //exp
    if (sPos >= strLen) then exit(false);
    if (bPos >= maxBinLength) then exit(false);
    hi := lo;

    // first byte
    repeat
      c := str[sPos]; lo := DECODE_TABLE[c];
      inc(sPos, 1 shl mul);
      if (lo >= 0) then break; // Legal character
      if ((c = '=') or (sPos >= strLen)) then
        exit(false); // Stop decoding
    until false;
    if bin <> nil then
      bin[bPos] := AnsiChar( ((hi and $3F) shl 2) or ((lo and $30) shr 4) );
    inc(bPos);
    if (sPos >= strLen) then exit(false);
    if (bPos >= maxBinLength) then exit(false);
    hi := lo;

    // second byte
    repeat
      c := str[sPos]; lo := DECODE_TABLE[c];
      inc(sPos, 1 shl mul);
      if (lo >= 0) then break; // Legal character
      if ((c = '=') or (sPos >= strLen)) then
        exit(false); // Stop decoding
    until false;
    if bin <> nil then
      bin[bPos] := AnsiChar( ((hi and $0F) shl 4) or ((lo and $3C) shr 2) );
    inc(bPos);
    if (sPos >= strLen) then exit(false);
    if (bPos >= maxBinLength) then exit(false);
    hi := lo;

    // third byte
    repeat
      c := str[sPos]; lo := DECODE_TABLE[c];
      inc(sPos, 1 shl mul);
      if (lo >= 0) then break; // Legal character
      if ((c = '=') or (sPos >= strLen)) then
        exit(false); // Stop decoding
    until false;
    if bin <> nil then
      bin[bPos] := AnsiChar( ((hi and $03) shl 6) or ((lo and $3F) shr 0) );
    inc(bPos);
    if (sPos >= strLen) then exit(false);
    if (bPos >= maxBinLength) then exit(false);
    Result := true;
  end;
begin
  Result := 0;
  if strIsTwoBytes then begin
    mul := 1; strLen := strLen shl mul;
  end else
    mul := 0;
  maxB := (maxBinLength div 3) * 3;
  maxS := (strLen div (4 shl mul)) * (4 shl mul);
  bPos := 0;
  sPos := 0;
  while (bPos < maxB) and (sPos < maxS) do begin
    v := DECODE_TABLE[str[sPos + (0 shl mul)]] shl 24 or
         DECODE_TABLE[str[sPos + (1 shl mul)]] shl 16 or
         DECODE_TABLE[str[sPos + (2 shl mul)]] shl 8 or
         DECODE_TABLE[str[sPos + (3 shl mul)]];
    // If MSB is set, input contains whitespace or is not valid base64
    if v and $80808080 > 0 then begin
      if not failTolerantDecode() then
        exit(bPos);
      maxB := bPos + (strLen shr mul - bPos) div 4 * 4; // Align maxB again
    end else begin
      if bin <> nil then begin
        bin[bPos + 0] := AnsiChar( ((v shr 22) and $FC) or ((v shr 20) and $03) );
        bin[bPos + 1] := AnsiChar( ((v shr 12) and $F0) or ((v shr 10) and $0F) );
        bin[bPos + 2] := AnsiChar( ((v shr  2) and $C0) or ((v shr  0) and $3F) );
      end;
      inc(bPos, 3);
      inc(sPos, 4 shl mul);
    end;
  end;
  if (sPos < strLen) and (bPos < maxBinLength) then
    failTolerantDecode();
  Result := bPos;
end;

function StringBytesWrite(bufData: Pointer; max_length: size_t; str: Pointer; strLen: size_t; isLatin1: Boolean; encoding:TEncoding): size_t; {$ifdef HASINLINE}inline;{$endif}
var
  strLenInBytes: size_t;
  i: PtrInt;
  ch: PAnsiChar;
begin
  if isLatin1 then
    strLenInBytes := strLen
  else
    strLenInBytes := strLen * 2;

  Result := strLenInBytes;
  if (encoding = HEX) then Result := Result shr 1;
  if isLatin1  and (encoding = UCS2) then Result := Result shl 1;
  if not isLatin1 and (encoding in [ASCII, LATIN1]) then Result := Result shr 1;
  if Result > max_length then
    Result := max_length;

  case encoding of
    ASCII, LATIN1: begin
      if isLatin1 then begin
        MoveFast(str^, bufData^, Result)
      end else begin
        for i := 0 to Result - 1 do
          Puint8Vector(bufData)[i] := Puint8Vector(str)[i*2];
      end;
    end;
    UTF8: begin
      if isLatin1 then begin
        if Result < strLen then
        while (Result > 0) and not isFirstByteInChar(pByte(PtrUInt(str)+Result)^) do
          Dec(Result);
        MoveFast(str^, bufData^, Result);
      end else begin
        raise ESMException.Create('Critical Error');
      end;
    end;
    UCS2: begin
      if isLatin1 then begin
        // todo: optimize
//        SetLength(UnicodeBuf, Result shr 1);
//        WinAnsiConvert.AnsiBufferToUnicode(Pointer(UnicodeBuf), str, Result shr 1);
//        MoveFast(Pointer(UnicodeBuf)^, bufData^, Result);
        Latin1AnsiConvert.AnsiBufferToUnicode(bufData, str, Result shr 1, true);
      end else begin
        Result := Result and (not 1);
        MoveFast(str^, bufData^, Result);
      end;
    end;
    BASE64: begin
      i := anyBase64ToBin(str, strLen, bufData, max_length, not isLatin1);
      if i = -1 then
        Result := 0 // error in base64
      else if i > max_length then
        Result := max_length
      else
        Result := i;
    end;
    HEX: begin
      if isLatin1 then begin
        ch := PAnsiChar(str);
        for i := 0 to Result - 1 do begin
          if (ConvertHexToBin[Ord(ch^)] > 15) or
             (ConvertHexToBin[Ord((ch+1)^)] > 15)
          then begin
            Result := i;
            Break;
          end;
          inc(ch, 2);
        end;
        HexToBin(str, bufData, Result);
      end else begin
        raise ESMTypeException.Create(NOT_A_HEX);
      end;
    end;
  end;
end;

function StringWrite(cx: PJSContext; argc: uintN; var vp: jsargRec; encoding: TEncoding): Boolean; {$ifdef HASINLINE}inline;{$endif}
var
  bufData: pointer;
  str: Pointer;
  strUtf8: RawUTF8;
  strLen: size_t;
  bytesWrite: size_t;
  isLatin1: Boolean;

  in_argv: PjsvalVector;
  _str: PJSString;
  offset: size_t;
  max_length: size_t;
  bufLen: size_t;
  this: PJSRootedObject;

const
  THREE_ARGRUMENTS_EXPECTED = '3 arguments expected';
  FIRST_ARGRUMENT_MUST_BE_A_STRING = 'Argument must be a string';
  OFFSET_IS_OUT_OF_BOUNDS = 'Offset is out of bounds';
begin
  try
    Result := True;

    if argc <3 then
      raise ESMException.Create(THREE_ARGRUMENTS_EXPECTED);
    in_argv := vp.argv;
    if not in_argv[0].isString then
      raise ESMException.Create(FIRST_ARGRUMENT_MUST_BE_A_STRING);

    _str :=  in_argv[0].asJSString;
    if encoding = UTF8 then begin
      strUtf8 := _str.ToUTF8(cx);
      str := Pointer(strUtf8);
      strLen := Length(strUtf8);
      isLatin1 := True;
      encoding := UTF8;
    end else begin
      isLatin1 := _str.HasLatin1Chars;
      if isLatin1 then
        str := _str.GetLatin1StringCharsAndLength(cx, strLen)
      else begin
        str := _str.GetTwoByteStringCharsAndLength(cx, strLen);
      end;
    end;

    if (encoding = HEX) and (strLen and 1 = 1) then
      raise ESMTypeException.Create(NOT_A_HEX);

    this := cx.NewRootedObject(vp.thisObject[cx]);
    try
      getBufDataAndLength(cx, this,bufData, bufLen);
    finally
      cx.FreeRootedObject(this);
    end;

    if in_argv[1].isNumber then
      offset := in_argv[1].asInt64
    else
      offset := 0;

    bufData := Pointer(UIntPtr(bufData) + offset);

    if offset > bufLen then
      raise ESMRangeException.Create(OFFSET_IS_OUT_OF_BOUNDS);

    if in_argv[2].isNumber then
      max_length := in_argv[2].asInt64
    else
      max_length := bufLen - offset;

    if max_length > bufLen - offset then
      max_length := bufLen - offset;

    if max_length = 0 then begin
      vp.rval := SimpleVariantToJSval(cx, 0);
      exit;
    end;

    bytesWrite := StringBytesWrite(bufData, max_length, str, strLen, isLatin1, encoding);

    vp.rval := SimpleVariantToJSval(cx, bytesWrite);
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function asciiWrite(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := StringWrite(cx, argc, vp, ASCII);
end;

function Latin1Write(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := StringWrite(cx, argc, vp, LATIN1);
end;

function utf8Write(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := StringWrite(cx, argc, vp, UTF8);
end;

function ucs2Write(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := StringWrite(cx, argc, vp, UCS2);
end;

function base64Write(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := StringWrite(cx, argc, vp, BASE64);
end;

function hexWrite(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := StringWrite(cx, argc, vp, HEX);
end;

// bytesCopied = buffer.copy(target[, targetStart][, sourceStart][, sourceEnd]);
function Copy(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  this, target: PJSRootedObject;
  sourceData, targetData: Pointer;
  val_i64: Int64;
  sourceLength, targetLength: size_t;
  targetStart, sourceStart, sourceEnd: size_t;
  to_copy: UInt32;
  in_argv: PjsvalVector;
const
  sInvalidCall = 'usage: copy(target: Buffer, [targetStart = 0], [sourceStart = 0], [sourceEnd = this.length]);';
begin
  try
    Result := True;
    in_argv := vp.argv;
    if (argc < 1) or (not in_argv[0].isObject) then
      raise ESMException.Create(sInvalidCall);

    this := cx.NewRootedObject(vp.thisObject[cx]);
    try
      getBufDataAndLength(cx, this, sourceData, sourceLength);
    finally
      cx.FreeRootedObject(this);
    end;

    target := cx.NewRootedObject(in_argv[0].asObject);
    try
      getBufDataAndLength(cx, target, targetData, targetLength);
    finally
      cx.FreeRootedObject(target);
    end;

    if (argc > 1) and (in_argv[1].isNumber) then begin
      val_i64 := in_argv[1].asInt64;
      if (val_i64 < $FFFFFFFF) and (val_i64 >= 0) then
        targetStart := val_i64
      else
        raise ESMRangeException.Create(OUT_OF_RANGE);
    end else
      targetStart := 0;

    if (argc > 2) and (in_argv[2].isNumber) then begin
      val_i64 := in_argv[2].asInt64;
      if (val_i64 < $FFFFFFFF) and (val_i64 >= 0) then
        sourceStart := val_i64
      else
        raise ESMRangeException.Create(OUT_OF_RANGE);
    end else
      sourceStart := 0;

    if (argc > 3) and (in_argv[3].isNumber) then begin
      val_i64 := in_argv[3].asInt64;
      if (val_i64 < $FFFFFFFF) and (val_i64 >= 0) then
        sourceEnd := val_i64
      else
        raise ESMRangeException.Create(OUT_OF_RANGE);
    end else
      sourceEnd := sourceLength;

    if (targetStart >= targetLength) or (sourceStart >= sourceEnd) then begin
      vp.rval := SimpleVariantToJSval(cx, 0);
      exit;
    end;

    if (sourceStart > sourceLength) then
      raise ESMRangeException.Create(OUT_OF_RANGE);

    if (sourceEnd - sourceStart > targetLength - targetStart) then
      sourceEnd := sourceStart + targetLength - targetStart;

    if sourceEnd - sourceStart > targetLength - targetStart then
      to_copy := targetLength - targetStart
    else
      to_copy := sourceEnd - sourceStart;

    if to_copy > targetLength - sourceStart then
      to_copy := targetLength - sourceStart;

    MoveFast(Pointer(UIntPtr(sourceData) + sourceStart)^, Pointer(UIntPtr(targetData) + targetStart)^, to_copy);

    vp.rval := SimpleVariantToJSval(cx, to_copy);
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function setupBufferJS(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  proto: PJSRootedObject;
const
  sInvalidCall = 'usage: setupBufferJS(proto: Object);';
  props = JSPROP_ENUMERATE or JSPROP_ENUMERATE or JSPROP_PERMANENT;
  // From GlobalObject.h getOrCreateTypedArrayPrototype
  TypedArraySlotIndex = JSCLASS_GLOBAL_APPLICATION_SLOTS + Ord(JSProto_LIMIT) + Ord(JSProto_TypedArray);
begin
  try
    Result := True;

    in_argv := vp.argv;
    if (argc < 1) or not (in_argv[0].isObject) then
      raise ESMException.Create(sInvalidCall);

    proto := cx.NewRootedObject(in_argv[0].asObject);

    try
      //set_buffer_prototype_object
      vp.thisObject[cx].ReservedSlot[0] := proto.ptr.ToJSValue;
      proto.ptr.DefineFunction(cx, 'asciiSlice', asciiSlice, 2, props);
      proto.ptr.DefineFunction(cx, 'base64Slice', base64Slice, 2, props);
      proto.ptr.DefineFunction(cx, 'latin1Slice', latin1Slice, 2, props);
      proto.ptr.DefineFunction(cx, 'hexSlice', hexSlice, 2, props);
      proto.ptr.DefineFunction(cx, 'ucs2Slice', ucs2Slice, 2, props);
      proto.ptr.DefineFunction(cx, 'utf8Slice', utf8Slice, 2, props);
      proto.ptr.DefineFunction(cx, 'cpSlice', cpSlice, 2, props);

      proto.ptr.DefineFunction(cx, 'asciiWrite', asciiWrite, 3, props);
      proto.ptr.DefineFunction(cx, 'base64Write', base64Write, 3, props);
      proto.ptr.DefineFunction(cx, 'latin1Write', asciiWrite, 3, props); // the same as asciiWrite
      proto.ptr.DefineFunction(cx, 'hexWrite', hexWrite, 3, props);
      proto.ptr.DefineFunction(cx, 'ucs2Write', ucs2Write, 3, props);
      proto.ptr.DefineFunction(cx, 'utf8Write', utf8Write, 3, props);

      proto.ptr.DefineFunction(cx, 'copy', Copy, 0, props);
    finally
      cx.FreeRootedObject(proto);
    end;

  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function createFromString(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  encoding: TEncoding;
  tmp: RawByteString;

  _str: PJSString;
  isLatin1: Boolean;
  str: Pointer;
  strLen: size_t;
  strUtf8: RawUTF8;

  buf: PJSRootedObject;
  bufData: Pointer;
  size: PtrInt;
  actual: size_t;
  proto: PJSRootedObject;
  val: jsval;
const
  sInvalidCall = 'usage: createFromString(string, encoding: String);';
  UNKNOWN_ENCODING = 'unknown encoding';
begin
  try
    Result := True;

    in_argv := vp.argv;
    if (argc < 2) or (not in_argv[0].isString) or (not in_argv[1].isString) then
      raise ESMException.Create(sInvalidCall);
    encoding := ParseEncoding(in_argv[1].asJSString.ToUTF8(cx), UTF8);
    _str := in_argv[0].asJSString;

    if encoding = UTF8 then begin
      strUtf8 := _str.ToUTF8(cx);
      str := Pointer(strUtf8);
      strLen := Length(strUtf8);
      isLatin1 := True;
      encoding := UTF8;
    end else begin
      isLatin1 := _str.HasLatin1Chars;
      if isLatin1 then
        str := _str.GetLatin1StringCharsAndLength(cx, strLen)
      else begin
        str := _str.GetTwoByteStringCharsAndLength(cx, strLen);
      end;
    end;

    case encoding of
      ASCII, LATIN1: size := strLen;
      BUFFER, UTF8: size := Length(_str.ToUTF8(cx));//todo fix it
      UCS2: size := strLen shl 1;
      BASE64: begin
        size := (strLen * 3) shr 2 + 2; // estimate
        setLength(tmp, size);
        size := anyBase64ToBin(str, strLen, pointer(tmp), size, not isLatin1);
        if size < 0 then size := 0;
        proto := cx.NewRootedObject(vp.thisObject[cx].ReservedSlot[0].asObject.Ctor[cx]);
        try
          val.asInteger := size;
          buf := cx.NewRootedObject(cx.New(proto.ptr, 1, @val));
          try
            bufData := buf.ptr.GetUint8ArrayData;
            vp.rval := buf.ptr.ToJSValue;
          finally
            cx.FreeRootedObject(buf);
          end;
          MoveFast(pointer(tmp)^, bufData^, size);
        finally
          cx.FreeRootedObject(proto);
        end;
        exit;
      end;
      HEX: size := strLen shr 1;
      else
        raise ESMException.Create(UNKNOWN_ENCODING);
    end;

    proto := cx.NewRootedObject(vp.thisObject[cx].ReservedSlot[0].asObject.Ctor[cx]);
    try
      val.asInteger := size;
      buf := cx.NewRootedObject(cx.New(proto.ptr, 1, @val));
      try
        bufData := buf.ptr.GetUint8ArrayData;
        vp.rval := buf.ptr.ToJSValue;
      finally
        cx.FreeRootedObject(buf);
      end;

      actual := StringBytesWrite(bufData, size, str, strLen, isLatin1, encoding);
      if actual = 0 then begin
        val.asInteger := 0;
        buf := cx.NewRootedObject(cx.New(proto.ptr, 1, @val));
        try
          vp.rval := buf.ptr.ToJSValue;
        finally
          cx.FreeRootedObject(buf);
        end;
      end
    finally
      cx.FreeRootedObject(proto);
    end;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function byteLengthUtf8(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
const
  sInvalidCall = 'usage: byteLengthUtf8(string: String);';
begin
  try
    Result := True;
    in_argv := vp.argv;
    if (argc < 1) or (not in_argv[0].isString) then
      raise ESMException.Create(sInvalidCall);
    vp.rval := SimpleVariantToJSval(cx, Length(in_argv[0].asJSString.ToUTF8(cx)));
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function memcmp(const X, Y; Size: size_t): Integer;
begin
  result := StrLIComp(PAnsiChar(@x), PAnsiChar(@y) ,size);
end;
//asm
//  mov esi,X
//  mov edi,Y
//  mov ecx,Size
//  mov dl,cl
//  and dl,3
//  shr ecx,2
//  xor eax,eax
//  rep cmpsd
//  jb @@less
//  ja @@great
//  mov cl,dl
//  rep cmpsb
//  jz @@end
//  ja @@great
// @@less:
//  dec eax
//  jmp @@end
// @@great:
//  inc eax
// @@end:
//end;

function compare(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  bufa, bufb: PJSRootedObject;
  bufa_data, bufb_data: Pointer;
  bufa_length, bufb_length: size_t;
  cmp_length: size_t;
  res: Integer;
const
  sInvalidCall = 'usage: compare(buf1, buf: Buffer);';
begin
  try
    Result := True;
    in_argv := vp.argv;
    if (argc < 2) or not (in_argv[0].isObject) or not (in_argv[1].isObject) then
      raise ESMException.Create(sInvalidCall);

    bufa := cx.NewRootedObject(in_argv[0].asObject);
    try
      getBufDataAndLength(cx, bufa, bufa_data, bufa_length);
    finally
      cx.FreeRootedObject(bufa);
    end;

    bufb := cx.NewRootedObject(in_argv[1].asObject);
    try
      getBufDataAndLength(cx, bufb, bufb_data, bufb_length);
    finally
      cx.FreeRootedObject(bufb);
    end;

    if bufa_length > bufb_length then
      cmp_length := bufb_length
    else
      cmp_length := bufa_length;

    if cmp_length > 0 then
      res := memcmp(bufa_data^, bufb_data^, cmp_length)
    else
      res := 0;

    if res = 0 then begin
      if bufa_length > bufb_length then
        res := 1
      else if bufb_length > bufa_length then
        res := -1;
    end else begin
      if res > 0 then
        res := 1
      else
        res := -1;
    end;

    vp.rval := SimpleVariantToJSval(cx, res);
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function compareOffset(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;

  target, source: PJSRootedObject;
  targetData, sourceData: Pointer;
  target_start, source_start, source_end, target_end, source_length, target_length: size_t;
  to_cmp: size_t;
  res: Integer;
const
  sInvalidCall = 'usage: compareOffset(source, target: Buffer; start, sourceStart[, targetEnd= target.length][, sourceEnd=source.length]: Number);';
begin
  try
    Result := True;
    in_argv := vp.argv;
    if (argc < 4) or not (in_argv[0].isObject) or not (in_argv[1].isObject)
      or not(in_argv[2].isNumber) or not (in_argv[3].isNumber)
    then
      raise ESMException.Create(sInvalidCall);

    source := cx.NewRootedObject(in_argv[0].asObject);
    try
      getBufDataAndLength(cx, source, sourceData, source_length);
    finally
      cx.FreeRootedObject(source);
    end;

    target := cx.NewRootedObject(in_argv[1].asObject);
    try
      getBufDataAndLength(cx, target, targetData, target_length);
    finally
      cx.FreeRootedObject(target);
    end;

    target_start := in_argv[2].asInt64;
    source_start := in_argv[3].asInt64;
    if (argc > 4) and in_argv[4].isNumber then
      target_end := in_argv[4].asInt64
    else
      target_end := source_length;
    if (argc > 5) and in_argv[4].isNumber then
      source_end := in_argv[5].asInt64
    else
      source_end := target_length;

    if source_start > source_length then
      raise ESMRangeException.Create(OUT_OF_RANGE);
    if target_start > target_length then
      raise ESMRangeException.Create(OUT_OF_RANGE);

    if source_start > source_end then
      raise ESMRangeException.Create(OUT_OF_RANGE);
    if target_start > target_end then
      raise ESMRangeException.Create(OUT_OF_RANGE);

    if source_end - source_start > target_end - target_start then
      to_cmp := target_end - target_start
    else
      to_cmp := source_end - source_start;

    if to_cmp > source_length - source_start then
      to_cmp := source_length - source_start;

    if to_cmp > 0 then
      res := memcmp(Pointer(UIntPtr(sourceData) + source_start)^, Pointer(UIntPtr(targetData) + target_start)^, to_cmp)
    else
      res := 0;

    if res = 0 then begin
      if source_end - source_start > target_end - target_start then
        res := 1
      else if target_end - target_start > source_end - source_start then
        res := -1;
    end else begin
      if res > 0 then
        res := 1
      else
        res := -1;
    end;

    vp.rval := SimpleVariantToJSval(cx, res);
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function fill(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;

  target: PJSRootedObject;
  targetData: Pointer;
  targetLength: size_t;
  _start, _end, fill_length: size_t;
  str: PJSString;
  str_length: size_t;
  str_lengthInBytes: size_t;
  str_data: Pointer;
  isLatin1: Boolean;
  encoding: TEncoding;
  strUtf8: RawUTF8;

  valObj: PJSRootedObject;
  valData: Pointer;
  valLength: size_t;
  Need_start_fill: boolean;

  valByte: Byte;

  in_there: size_t;
  ptr: Pointer;
const
  sInvalidCall = 'usage: fill(target: Buffer; val: *; start, end: Number[; encoding: String = "UTF8"]);';
begin
  try
    Result := True;
    in_argv := vp.argv;
    if (argc < 4) or (not in_argv[0].isObject) or (not in_argv[2].isNumber) or (not in_argv[3].isNumber) then
      raise ESMException.Create(sInvalidCall);

    vp.rval := JSVAL_VOID;
    target := cx.NewRootedObject(in_argv[0].asObject);
    try
      getBufDataAndLength(cx, target, targetData, targetLength);
    finally
      cx.FreeRootedObject(target);
    end;

    _start := in_argv[2].asInt64;
    _end := in_argv[3].asInt64;
    fill_length := _end - _start;

    if fill_length + _start > targetLength then
      raise ESMRangeException.Create(OUT_OF_RANGE);
    Need_start_fill := False;
    // First check if Buffer has been passed.
    if in_argv[1].isObject and not in_argv[1].isNull then begin
      valObj := cx.NewRootedObject(in_argv[1].asObject);
      try
        if valObj.ptr.IsTypedArrayObject then begin
          getBufDataAndLength(cx, valObj, valData, valLength);
          str_length := valLength;
          if str_length > fill_length then
            str_lengthInBytes := fill_length
          else
            str_lengthInBytes := str_length;
          MoveFast(valData^, Pointer(UIntPtr(targetData) + _start)^, str_lengthInBytes);
          Need_start_fill := true;
        end;
      finally
        cx.FreeRootedObject(valObj);
      end;
    end;
    if not Need_start_fill then begin
      // Then coerce everything that's not a string.
      if not in_argv[1].isString then begin
        valByte := in_argv[1].asInt64 and 255;
        FillChar(Pointer(UIntPtr(targetData) + _start)^, fill_length, valByte);
        Exit;
      end;

      if (argc > 4) and (in_argv[4].isString) then
        encoding := ParseEncoding(in_argv[4].asJSString.ToUTF8(cx), UTF8)
      else
        encoding := UTF8;

      str :=  in_argv[1].asJSString;
      if encoding = UTF8 then begin
        strUtf8 := str.ToUTF8(cx);
        str_data := Pointer(strUtf8);
        str_length := Length(strUtf8);
        isLatin1 := True;
        encoding := UTF8;
      end else begin
        isLatin1 := str.HasLatin1Chars;
        if isLatin1 then begin
          str_data := str.GetLatin1StringCharsAndLength(cx, str_length);
        end else begin
          str_data := str.GetTwoByteStringCharsAndLength(cx, str_length);
        end;
      end;
      if encoding = UCS2 then
        str_lengthInBytes := str_length * 2
      else
        str_lengthInBytes := str_length;

      if str_lengthInBytes > fill_length then
        str_lengthInBytes := fill_length;

      case encoding of
        UTF8: begin
          if isLatin1 then
            MoveFast(str_data^, Pointer(UIntPtr(targetData) + _start)^, str_lengthInBytes)
//          else
//            RawUnicodeToUtf8(Pointer(UIntPtr(targetData) + _start), str_length, str_data, str_length,[ccfNoTrailingZero]);
        end;
        UCS2: begin
          if isLatin1 then begin
            Latin1AnsiConvert.AnsiBufferToUnicode(Pointer(UIntPtr(targetData) + _start), str_data, str_lengthInBytes shr 1);
          end else
            MoveFast(str_data^, Pointer(UIntPtr(targetData) + _start)^, str_lengthInBytes);
        end;
        else begin
          str_lengthInBytes := StringBytesWrite(Pointer(UIntPtr(targetData) + _start), fill_length, str_data, str_length, isLatin1, encoding);
          if str_lengthInBytes = 0 then
            Exit;
        end;
      end;
    end;

    if str_lengthInBytes >= fill_length then
      exit;

    in_there := str_lengthInBytes;
    ptr := Pointer(UIntPtr(targetData) + _start + str_lengthInBytes);
    while (in_there < fill_length - in_there) do begin
      MoveFast(Pointer(UIntPtr(targetData) + _start)^, ptr^, in_there);
      ptr := Pointer(UIntPtr(ptr) + in_there);
      in_there := in_there shl 1;
    end;

    if in_there < fill_length then
      MoveFast(Pointer(UIntPtr(targetData) + _start)^, ptr^, fill_length - in_there);

  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

// Computes the offset for starting an indexOf or lastIndexOf search.
// Returns either a valid offset in [0...<length - 1>], ie inside the Buffer,
// or -1 to signal that there is no possible match.
function IndexOfOffset(length: size_t; offset_i64: Int64; is_forward: Boolean): Int64; {$ifdef HASINLINE}inline;{$endif}
var
  length_i64: Int64;
begin
  length_i64 := length;
  if length_i64 = 0 then
    // Empty buffer, no match.
    Result := -1
  else if offset_i64 < 0 then begin
    if offset_i64 + length_i64 >= 0 then
      // Negative offsets count backwards from the end of the buffer.
      Result := length_i64 + offset_i64
    else if is_forward then
      // indexOf from before the start of the buffer: search the whole buffer.
      Result := 0
    else
      // lastIndexOf from before the start of the buffer: no match.
      Result := -1
  end else begin
    if offset_i64 < length_i64 then
      // Valid positive offset.
      Result := offset_i64
    else if is_forward then
      // indexOf from past the end of the buffer: no match.
      Result := -1
    else
      // lastIndexOf from past the end of the buffer: search the whole buffer.
      Result := length_i64 - 1;
  end;
end;

function SearchString(haystack: Pointer; haystack_length: size_t; needle: Pointer; needle_length: size_t; offset: size_t; is_forward: Boolean; isTwoByte: Boolean): size_t;  {$ifdef HASINLINE}inline;{$endif}
var
  pStart, pEnd: Pointer;
  delta: integer;
begin;
// todo: optimize it
  if is_forward then begin
      pStart := @Puint8Vector(haystack)[offset];
      pEnd := @Puint8Vector(haystack)[haystack_length - needle_length];
  end else begin
  // todo check is offset for start or for end in this case
      if haystack_length - needle_length < offset then
        offset := haystack_length - needle_length;
      pStart := @Puint8Vector(haystack)[offset];
      pEnd := haystack;
  end;

  if isTwoByte then
    delta := 2
  else
    delta := 1;

  Result := haystack_length;

  while
    (is_forward and (PtrUInt(pStart) <= PtrUInt(pEnd)))
    or
    (not is_forward and (PtrUInt(pEnd) <= PtrUInt(pStart)))
  do begin
    if CompareMem(pStart, needle, needle_length) then begin
      Result := PtrUInt(pStart) - PtrUInt(haystack);
      Break;
    end;
    if is_forward then
      pStart := Pointer(PtrUInt(pStart) + delta)
    else
      pStart := Pointer(PtrUInt(pStart) - delta)
  end;

end;

function indexOfBuffer(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;

  sourceObj: PJSRootedObject;
  sourceData: Pointer;
  sourceLength: size_t;

  bufObj: PJSRootedObject;
  bufData: Pointer;
  bufLength: size_t;

  offset_i64: Int64;
  opt_offset: Int64;
  offset: size_t;
  encoding: TEncoding;
  is_forward: Boolean;

  haystack: Pointer;
  haystack_length: size_t;
  needle: Pointer;
  needle_length: size_t;

  res: size_t;
const
  sInvalidCall = 'usage: indexOfBuffer(source, buf: Buffer; offset: Number; encoding: String; is_forward: boolean);';
begin
  try
    Result := True;
    in_argv := vp.argv;
    if (argc < 5) or (not in_argv[0].isObject) or (not in_argv[1].isObject)
      or (not in_argv[2].isNumber) or (not in_argv[4].isBoolean)
    then
      raise ESMException.Create(sInvalidCall);

    vp.rval := SimpleVariantToJSval(cx, -1);

    sourceObj := cx.NewRootedObject(in_argv[0].asObject);
    try
      getBufDataAndLength(cx, sourceObj, sourceData, sourceLength);
    finally
      cx.FreeRootedObject(sourceObj);
    end;

    bufObj := cx.NewRootedObject(in_argv[1].asObject);
    try
      getBufDataAndLength(cx, bufObj, bufData, bufLength);
    finally
      cx.FreeRootedObject(bufObj);
    end;


    if in_argv[3].isString then
      encoding := ParseEncoding(in_argv[3].asJSString.ToUTF8(cx), UTF8)
    else
      encoding := UTF8;

    is_forward := in_argv[4].asBoolean;

    haystack := sourceData;
    haystack_length := sourceLength;
    needle := bufData;
    needle_length := bufLength;

    if (needle_length = 0) or (haystack_length = 0) then
      Exit;

    offset_i64 := in_argv[2].asInt64;

    opt_offset := IndexOfOffset(haystack_length, offset_i64, is_forward);
    if opt_offset <= -1 then
      Exit;
    offset := opt_offset;
    if offset >=  haystack_length then
      raise ESMRangeException.Create(OUT_OF_RANGE);
    if (is_forward and (needle_length + offset > haystack_length)) or (needle_length > haystack_length) then
      Exit;

    if encoding = UCS2 then begin
      if (haystack_length < 2) or (needle_length < 2) then
        Exit;
        res := SearchString(haystack, haystack_length, needle, needle_length, offset, is_forward, true);
    end else
      res := SearchString(haystack, haystack_length, needle, needle_length, offset, is_forward, false);
    if res = haystack_length then
      Exit;
    vp.rval := SimpleVariantToJSval(cx, res);
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function indexOfNumber(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;

  sourceObj: PJSRootedObject;
  sourceData: Pointer;
  sourceLength: size_t;

  needle: UInt32;

  offset_i64: Int64;
  opt_offset: Int64;
  offset: size_t;
  is_forward: Boolean;

  res: size_t;

const
  sInvalidCall = 'usage: indexOfNumber(source: Buffer; val, offset: Number; is_forward: boolean);';
begin
  try
    Result := True;
    in_argv := vp.argv;
    if (argc < 3) or (not in_argv[0].isObject) or (not in_argv[1].isNumber) or (not in_argv[2].isNumber) or (not in_argv[3].isBoolean) then
      raise ESMException.Create(sInvalidCall);

    vp.rval := SimpleVariantToJSval(cx, -1);
    sourceObj := cx.NewRootedObject(in_argv[0].asObject);
    try
      getBufDataAndLength(cx, sourceObj, sourceData, sourceLength);
    finally
      cx.FreeRootedObject(sourceObj);
    end;

    needle := in_argv[1].asInt64;
    offset_i64 := in_argv[2].asInt64;
    is_forward := in_argv[3].asBoolean;

    opt_offset := IndexOfOffset(sourceLength, offset_i64, is_forward);

    if opt_offset <= -1 then
      Exit;
    offset := opt_offset;

    if offset >=  sourceLength then
      raise ESMRangeException.Create(OUT_OF_RANGE);
    //todo: optimize
    res := SearchString(sourceData, sourceLength, @needle, 1, offset, is_forward, False);
    if res = sourceLength then
      Exit;
    vp.rval := SimpleVariantToJSval(cx, res);
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function indexOfString(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;

  sourceObj: PJSRootedObject;
  sourceData: Pointer;
  sourceLength: size_t;

//  bufObj: PJSRootedObject;
//  bufData: Pointer;
//  bufLength: size_t;

  offset_i64: Int64;
  opt_offset: Int64;
  offset: size_t;
  encoding: TEncoding;
  is_forward: Boolean;

  haystack: Pointer;
  haystack_length: size_t;
  needle: PJSString;
  needle_length: size_t;
  needleData: Pointer;
  isLatin1: Boolean;
  needleUtf8: RawUTF8;
  needleSynUnicode: SynUnicode;

  res: size_t;

const
  sInvalidCall = 'usage: indexOfString(source: Buffer; val: String; offset: Number; encoding: String; is_forward: boolean);';
begin
  try
    Result := True;
    in_argv := vp.argv;
    if (argc < 5) or (not in_argv[0].isObject) or (not in_argv[1].isString) or (not in_argv[2].isNumber)
        or (not in_argv[4].isBoolean) then
      raise ESMException.Create(sInvalidCall);

    vp.rval := SimpleVariantToJSval(cx, -1);
    if in_argv[3].isString then
      encoding := ParseEncoding(in_argv[3].asJSString.ToUTF8(cx), UTF8)
    else
      encoding := UTF8;

    sourceObj := cx.NewRootedObject(in_argv[0].asObject);
    try
      getBufDataAndLength(cx, sourceObj, sourceData, sourceLength);
    finally
      cx.FreeRootedObject(sourceObj);
    end;

    needle := in_argv[1].asJSString;
    offset_i64 := in_argv[2].asInt64;
    is_forward := in_argv[4].asBoolean;

    haystack := sourceData;
    // Round down to the nearest multiple of 2 in case of UCS2.
    if encoding = UCS2 then
      haystack_length := sourceLength and (not 1)
    else
      haystack_length := sourceLength;

    isLatin1 := needle.HasLatin1Chars;

    if isLatin1 then
      needleData := needle.GetLatin1StringCharsAndLength(cx, needle_length)
    else begin
      needleData := needle.GetTwoByteStringCharsAndLength(cx, needle_length);
    end;
    if encoding = UCS2 then
      needle_length := needle_length shl 1;

    if (needle_length = 0) or (haystack_length = 0) then
      Exit;

    opt_offset := IndexOfOffset(haystack_length, offset_i64, is_forward);
    if opt_offset <= -1 then
      Exit;
    offset := opt_offset;
    if offset >=  haystack_length then
      raise ESMRangeException.Create(OUT_OF_RANGE);
    if (is_forward and (needle_length + offset > haystack_length)) or (needle_length > haystack_length) then
      Exit;

    res := haystack_length;

    if encoding = UCS2 then begin
      if (needleData = nil) or (haystack_length < 2) or (needle_length < 2) then
        Exit;
      if Endianness = kBigEndian then begin
        raise ESMException.Create('Not implemented yet'); //todo
      end else begin
        if isLatin1 then
          needleSynUnicode := needle.ToSynUnicode(cx)
        else
          SetString(needleSynUnicode, PWideChar(needleData), needle_length);
        res := SearchString(haystack, haystack_length, Pointer(needleSynUnicode), Length(needleSynUnicode), offset, is_forward, true);
      end;
    end else if encoding = UTF8 then begin
    //todo: optimize
      needleUtf8 := needle.ToUTF8(cx);
      if needleUtf8 = '' then
        Exit;
      res := SearchString(haystack, haystack_length, Pointer(needleUtf8), Length(needleUtf8), offset, is_forward, false);
    end else if encoding = LATIN1 then begin
      res := SearchString(haystack, haystack_length, needleData, needle_length, offset, is_forward, false);
    end;

    if res = haystack_length then
      Exit;

    vp.rval := SimpleVariantToJSval(cx, res);
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

procedure Swizzle(start: Pointer; len: size_t);  {$ifdef HASINLINE}inline;{$endif}
var
  _start: PAnsiChar absolute start;
  _end: PAnsiChar;
  tmp: AnsiChar;
begin
  _end := _start + (len - 1);
  while (_start < _end) do begin
    tmp := _start^;
    _start^ := _end^;
    _end^ := tmp;
    inc(_start);
    Dec(_end);
  end;
end;

const
  sFloatType: array[boolean] of RawUTF8 = ('Float', 'Double');
  sFloatEndian: array[boolean] of RawUTF8 = ('LE', 'BE');
  cFloatSize: array[boolean] of size_t = (4, 8);

function ReadFloatGeneric(cx: PJSContext; argc: uintN; var vp: jsargRec; endian: TEndianness; isDouble: Boolean): Boolean;  {$ifdef HASINLINE}inline;{$endif}
var
  in_argv: PjsvalVector;

  bufObj: PJSRootedObject;
  bufData: Pointer;
  bufLength: size_t;

  offset: size_t;

  d: Double;
  s: Single absolute d;
const
  sInvalidCall = 'usage: read%%(buf: Buffer, offset: Number);';
begin
  try
    Result := True;
    in_argv := vp.argv;
    if (argc < 2) or (not in_argv[0].isObject)  or (not in_argv[1].isNumber) then
      raise ESMException.CreateUTF8(sInvalidCall, [sFloatType[isDouble], sFloatEndian[endian = kLittleEndian]]);

    bufObj := cx.NewRootedObject(in_argv[0].asObject);
    try
      getBufDataAndLength(cx, bufObj, bufData, bufLength);
    finally
      cx.FreeRootedObject(bufObj);
    end;

    offset := in_argv[1].asInt64;
    if offset + cFloatSize[isDouble] > bufLength then
      raise ESMRangeException.Create(OUT_OF_RANGE);

    MoveFast(Pointer(PtrUInt(bufData) + offset)^, d, cFloatSize[isDouble]);

    if endian <> Endianness then
      Swizzle(@d, cFloatSize[isDouble]);

    if isDouble then
      vp.rval := SimpleVariantToJSval(cx, d)
    else
      vp.rval := SimpleVariantToJSval(cx, s);
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function readDoubleBE(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := ReadFloatGeneric(cx, argc, vp, kBigEndian, true);
end;

function readDoubleLE(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := ReadFloatGeneric(cx, argc, vp, kLittleEndian, true);
end;

function readFloatBE(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := ReadFloatGeneric(cx, argc, vp, kBigEndian, False);
end;

function readFloatLE(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := ReadFloatGeneric(cx, argc, vp, kLittleEndian, False);
end;

function WriteFloatGeneric(cx: PJSContext; argc: uintN; var vp: jsargRec; endian: TEndianness; isDouble: Boolean): Boolean;  {$ifdef HASINLINE}inline;{$endif}
var
  in_argv: PjsvalVector;

  bufObj: PJSRootedObject;
  bufData: Pointer;
  bufLength: size_t;

  offset: size_t;
  should_assert: Boolean;
  memcpy_num: size_t;

  d: Double;
  s: Single absolute d;
const
  sInvalidCall = 'usage: Write%%(buf: Buffer; val: Number; offset: Number; [shouldAssert: boolean = false]);';
begin
  try
    Result := True;
    in_argv := vp.argv;
    if (argc < 3) or (not in_argv[0].isObject)  or (not in_argv[1].isNumber) or (not in_argv[2].isNumber) then
      raise ESMException.CreateUTF8(sInvalidCall, [sFloatType[isDouble], sFloatEndian[endian = kLittleEndian]]);

    should_assert := argc < 4;
    bufObj := cx.NewRootedObject(in_argv[0].asObject);
    try
      getBufDataAndLength(cx, bufObj, bufData, bufLength);
    finally
      cx.FreeRootedObject(bufObj);
    end;

    if isDouble then
      d := in_argv[1].asDouble
    else
      s := in_argv[1].asDouble;

    offset := in_argv[2].asInt64;

    if should_assert and ((offset + cFloatSize[isDouble] > bufLength) or (offset + cFloatSize[isDouble] < cFloatSize[isDouble]))then
      raise ESMRangeException.Create(OUT_OF_RANGE);

    if (offset + cFloatSize[isDouble] > bufLength) then
      memcpy_num := bufLength - offset
    else
      memcpy_num := cFloatSize[isDouble];

    if endian <> Endianness then
      Swizzle(@d, cFloatSize[isDouble]);

    MoveFast(d, Pointer(PtrUInt(bufData) + offset)^, memcpy_num);

    vp.rval := JSVAL_VOID;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function writeDoubleBE(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := WriteFloatGeneric(cx, argc, vp, kBigEndian, True);
end;

function writeDoubleLE(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := WriteFloatGeneric(cx, argc, vp, kLittleEndian, True);
end;

function writeFloatBE(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := WriteFloatGeneric(cx, argc, vp, kBigEndian, False);
end;

function writeFloatLE(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := WriteFloatGeneric(cx, argc, vp, kLittleEndian, False);
end;

function swap(cx: PJSContext; argc: uintN; var vp: jsargRec; bytesCount: byte): Boolean;  {$ifdef HASINLINE}inline;{$endif}
var
  in_argv: PjsvalVector;

  bufObj: PJSRootedObject;
  bufData: Pointer;
  bufLength: size_t;

  i: size_t;
const
  sInvalidCall = 'usage: swap%(buf: Buffer);';
begin
  try
    Result := True;
    in_argv := vp.argv;
    if (argc < 1) or (not in_argv[0].isObject)then
      raise ESMException.CreateUTF8(sInvalidCall, [1 shl bytesCount]);

    bufObj := cx.NewRootedObject(in_argv[0].asObject);
    try
      getBufDataAndLength(cx, bufObj, bufData, bufLength);
    finally
      cx.FreeRootedObject(bufObj);
    end;

    if bufLength mod bytesCount <> 0 then
      raise ESMRangeException.Create(OUT_OF_RANGE);

     i := 0;
     while (i < bufLength) do begin
       Swizzle(bufData, bytesCount);
       bufData := Pointer(PtrUInt(bufData) + bytesCount);
       Inc(i, bytesCount);
     end;

    vp.rval := in_argv[0];
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function swap16(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := swap(cx, argc, vp, 2);
end;

function swap32(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := swap(cx, argc, vp, 4);
end;

function swap64(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := swap(cx, argc, vp, 8);
end;

const
  BufferBinding_class: JSClass = (name: 'BufferBinding';
    flags:
      (1 shl JSCLASS_RESERVED_SLOTS_SHIFT);
    );

function SyNodeBindingProc_buffer(const aEngine: TSMEngine; const bindingNamespaceName: SynUnicode): jsval;
var
  obj: PJSRootedObject;
  cx: PJSContext;
const
  props = JSPROP_ENUMERATE or JSPROP_ENUMERATE or JSPROP_PERMANENT;
begin
  cx := aEngine.cx;
  obj := cx.NewRootedObject(cx.NewObject(@BufferBinding_class));

  obj.ptr.DefineFunction(cx, 'setupBufferJS', setupBufferJS, 0, props);
  obj.ptr.DefineFunction(cx, 'createFromString', createFromString, 0, props);

  obj.ptr.DefineFunction(cx, 'byteLengthUtf8', byteLengthUtf8, 0, props);
  obj.ptr.DefineFunction(cx, 'compare', compare, 0, props);
  obj.ptr.DefineFunction(cx, 'compareOffset', compareOffset, 0, props);
  obj.ptr.DefineFunction(cx, 'fill', fill, 0, props);
  obj.ptr.DefineFunction(cx, 'indexOfBuffer', indexOfBuffer, 0, props);
  obj.ptr.DefineFunction(cx, 'indexOfNumber', indexOfNumber, 0, props);
  obj.ptr.DefineFunction(cx, 'indexOfString', indexOfString, 0, props);

  obj.ptr.DefineFunction(cx, 'readDoubleBE', readDoubleBE, 0, props);
  obj.ptr.DefineFunction(cx, 'readDoubleLE', readDoubleLE, 0, props);
  obj.ptr.DefineFunction(cx, 'readFloatBE', readFloatBE, 0, props);
  obj.ptr.DefineFunction(cx, 'readFloatLE', readFloatLE, 0, props);

  obj.ptr.DefineFunction(cx, 'writeDoubleBE', writeDoubleBE, 0, props);
  obj.ptr.DefineFunction(cx, 'writeDoubleLE', writeDoubleLE, 0, props);
  obj.ptr.DefineFunction(cx, 'writeFloatBE', writeFloatBE, 0, props);
  obj.ptr.DefineFunction(cx, 'writeFloatLE', writeFloatLE, 0, props);

  obj.ptr.DefineFunction(cx, 'swap16', swap16, 0, props);
  obj.ptr.DefineFunction(cx, 'swap32', swap32, 0, props);
  obj.ptr.DefineFunction(cx, 'swap64', swap64, 0, props);

  obj.ptr.DefineProperty(cx, 'kMaxLength', SimpleVariantToJSval(cx, kMaxLength), props);
  obj.ptr.DefineProperty(cx, 'kStringMaxLength', SimpleVariantToJSval(cx, kMaxLength), props);

  Result := obj.ptr.ToJSValue;

  cx.FreeRootedObject(obj);
end;

initialization
  if _Endianness.u16 = 1 then
    Endianness := kLittleEndian
  else
    Endianness := kBigEndian;

  TSMEngineManager.RegisterBinding('buffer', SyNodeBindingProc_buffer);

end.
