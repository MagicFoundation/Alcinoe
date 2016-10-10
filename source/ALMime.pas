{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Jedi Project - JCL
              Stéphane Vander Clock (skype/email: svanderclock@yahoo.fr)             

product:      Alcinoe Mime Functions
Version:      4.00

Description:  Function mime encode and decode from JCL and function
              to get default Mime content type from file extension

Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :     04-05-2012:
              - Non-thread-safe THashedStringList was replaced to
                TALStringList, before it was crushing sometimes when
                searching in multi-thread environment.
              26/06/2012: Add xe2 support
Link :

**************************************************************}
unit ALMime;

interface

{$IF CompilerVersion < 29} {Delphi XE8}
  {$IF defined(CPUX64)} // The CPU supports the x86-64 instruction set, and is in a 64-bit environment. *New* in XE2/x64
    {$DEFINE CPU64BITS} // The CPU is in a 64-bit environment, such as DCC64.EXE. *New* in XE8
  {$ENDIF}
  {$IF defined(CPUX86)} // 	The CPU supports the x86-64 instruction set, and is in a 64-bit environment. *New* in XE2/x64
    {$DEFINE CPU32BITS} // The CPU is in a 32-bit environment, such as DCC32.EXE. *New* in XE8
  {$ENDIF}
{$ENDIF}

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses System.Classes,
     System.Types,
     System.sysutils,
     ALStringList;

type
  TALDynByteArray = array of Byte;
  {$IF defined(CPU64BITS)}
  ALSizeInt = NativeInt;
  {$ELSE}
  ALSizeInt = Integer;
  {$IFEND}

type
  TALAddr32 = Cardinal;
  TALAddr64 = Int64;
  {$IF defined(CPU64BITS)}
  TALAddr = TALAddr64;
  {$ELSE}
  TALAddr = TALAddr32;
  {$IFEND}

//From jcl
{$IFNDEF NEXTGEN}
function ALMimeEncodeString(const S: AnsiString): AnsiString;
function ALMimeEncodeStringNoCRLF(const S: AnsiString): AnsiString;
function ALMimeDecodeString(const S: AnsiString): AnsiString;
{$ENDIF}
function ALMimeEncodeStringU(const S: String; const AEncoding: TEncoding = nil): String;  // AEncoding = how the 16 bits string will be encoded in 8 bits string to be later encoded via base64 (who only support 8 bits string)
function ALMimeEncodeStringNoCRLFU(const S: String; const AEncoding: TEncoding = nil): String;  // AEncoding = how the 16 bits string will be encoded in 8 bits string to be later encoded via base64 (who only support 8 bits string)
function ALMimeDecodeStringU(const S: String; const AEncoding: TEncoding = nil): String; // AEncoding = how the 8 bits string returned by the output of the base64 will be encoded in 16 bits string.
//------
function ALMimeEncodeBytesU(const Bytes: Tbytes): String;
function ALMimeEncodeBytesNoCRLFU(const Bytes: Tbytes): String;
function ALMimeDecodeBytesU(const S: String): Tbytes;


function ALMimeEncodedSize(const InputSize: ALSizeInt): ALSizeInt;
function ALMimeEncodedSizeNoCRLF(const InputSize: ALSizeInt): ALSizeInt;
function ALMimeDecodedSize(const InputSize: ALSizeInt): ALSizeInt;

procedure ALMimeEncode(const InputBuffer: TALDynByteArray; InputOffset: ALSizeInt;
  const InputByteCount: ALSizeInt; out OutputBuffer: TALDynByteArray; OutputOffset: ALSizeInt = 0); overload;
procedure ALMimeEncodeNoCRLF(const InputBuffer: TALDynByteArray; InputOffset: ALSizeInt;
  const InputByteCount: ALSizeInt; out OutputBuffer: TALDynByteArray; OutputOffset: ALSizeInt = 0); overload;
procedure ALMimeEncodeFullLines(const InputBuffer: TALDynByteArray; InputOffset: ALSizeInt;
  const InputByteCount: ALSizeInt; out OutputBuffer: TALDynByteArray; OutputOffset: ALSizeInt = 0); overload;
function ALMimeDecode(const InputBuffer: TALDynByteArray; InputOffset: ALSizeInt;
  const InputByteCount: ALSizeInt; out OutputBuffer: TALDynByteArray; OutputOffset: ALSizeInt = 0): ALSizeInt; overload;
function ALMimeDecodePartial(const InputBuffer: TALDynByteArray; InputOffset: ALSizeInt;
  const InputByteCount: ALSizeInt; out OutputBuffer: TALDynByteArray; OutputOffset: ALSizeInt;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): ALSizeInt; overload;
function ALMimeDecodePartialEnd(out OutputBuffer: TALDynByteArray; OutputOffset: ALSizeInt;
  const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): ALSizeInt; overload;

procedure ALMimeEncode(const InputBuffer: TALDynByteArray; const InputByteCount: ALSizeInt;
  out OutputBuffer: TALDynByteArray); overload;
procedure ALMimeEncodeNoCRLF(const InputBuffer: TALDynByteArray; const InputByteCount: ALSizeInt;
  out OutputBuffer: TALDynByteArray); overload;
procedure ALMimeEncodeFullLines(const InputBuffer: TALDynByteArray; const InputByteCount: ALSizeInt;
  out OutputBuffer: TALDynByteArray); overload;
function ALMimeDecode(const InputBuffer: TALDynByteArray; const InputByteCount: ALSizeInt;
  out OutputBuffer: TALDynByteArray): ALSizeInt; overload;
function ALMimeDecodePartial(const InputBuffer: TALDynByteArray; const InputByteCount: ALSizeInt;
  out OutputBuffer: TALDynByteArray; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): ALSizeInt; overload;
function ALMimeDecodePartialEnd(out OutputBuffer: TALDynByteArray; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): ALSizeInt; overload;

procedure ALMimeEncode(const InputBuffer; const InputByteCount: ALSizeInt; out OutputBuffer); overload;
procedure ALMimeEncodeNoCRLF(const InputBuffer; const InputByteCount: ALSizeInt; out OutputBuffer); overload;
procedure ALMimeEncodeFullLines(const InputBuffer; const InputByteCount: ALSizeInt; out OutputBuffer); overload;
function ALMimeDecode(const InputBuffer; const InputByteCount: ALSizeInt; out OutputBuffer): ALSizeInt; overload;
function ALMimeDecodePartial(const InputBuffer; const InputByteCount: ALSizeInt; out OutputBuffer;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): ALSizeInt; overload;
function ALMimeDecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): ALSizeInt; overload;

procedure ALMimeEncodeFile(const InputFileName, OutputFileName: TFileName);
procedure ALMimeEncodeFileNoCRLF(const InputFileName, OutputFileName: TFileName);
procedure ALMimeDecodeFile(const InputFileName, OutputFileName: TFileName);
procedure ALMimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
procedure ALMimeEncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);
procedure ALMimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);

const
  cALMIME_ENCODED_LINE_BREAK = 76;
  cALMIME_DECODED_LINE_BREAK = cALMIME_ENCODED_LINE_BREAK div 4 * 3;
  cALMIME_BUFFER_SIZE = cALMIME_DECODED_LINE_BREAK * 3 * 4 * 4;

//From indy
{$IFNDEF NEXTGEN}
Function  ALGetDefaultFileExtFromMimeContentType(aContentType: AnsiString): AnsiString;
Function  ALGetDefaultMIMEContentTypeFromExt(const aExt: AnsiString): AnsiString;

Var vAlMimeContentTypeByExtList: TALStrings; {.htm=text/html}
    vAlExtbyMimeContentTypeList: TALStrings; {text/html=.htm}
{$ENDIF}

implementation

uses ALString,
     ALCommon;

// Caution: For MimeEncodeStream and all other kinds of multi-buffered
// Mime encodings (i.e. Files etc.), BufferSize must be set to a multiple of 3.
// Even though the implementation of the Mime decoding routines below
// do not require a particular buffer size, they work fastest with sizes of
// multiples of four. The chosen size is a multiple of 3 and of 4 as well.
// The following numbers are, in addition, also divisible by 1024:
// $2400, $3000, $3C00, $4800, $5400, $6000, $6C00.

{***}
const
  { The mime encoding table. Do not alter. }
  cALMIME_ENCODE_TABLE: array [0..63] of Byte = (
    065, 066, 067, 068, 069, 070, 071, 072, //  00 - 07
    073, 074, 075, 076, 077, 078, 079, 080, //  08 - 15
    081, 082, 083, 084, 085, 086, 087, 088, //  16 - 23
    089, 090, 097, 098, 099, 100, 101, 102, //  24 - 31
    103, 104, 105, 106, 107, 108, 109, 110, //  32 - 39
    111, 112, 113, 114, 115, 116, 117, 118, //  40 - 47
    119, 120, 121, 122, 048, 049, 050, 051, //  48 - 55
    052, 053, 054, 055, 056, 057, 043, 047); // 56 - 63

  cALMIME_PAD_CHAR = Byte('=');

  cALMIME_DECODE_TABLE: array [Byte] of Byte = (
    255, 255, 255, 255, 255, 255, 255, 255, //   0 -   7
    255, 255, 255, 255, 255, 255, 255, 255, //   8 -  15
    255, 255, 255, 255, 255, 255, 255, 255, //  16 -  23
    255, 255, 255, 255, 255, 255, 255, 255, //  24 -  31
    255, 255, 255, 255, 255, 255, 255, 255, //  32 -  39
    255, 255, 255, 062, 255, 255, 255, 063, //  40 -  47
    052, 053, 054, 055, 056, 057, 058, 059, //  48 -  55
    060, 061, 255, 255, 255, 255, 255, 255, //  56 -  63
    255, 000, 001, 002, 003, 004, 005, 006, //  64 -  71
    007, 008, 009, 010, 011, 012, 013, 014, //  72 -  79
    015, 016, 017, 018, 019, 020, 021, 022, //  80 -  87
    023, 024, 025, 255, 255, 255, 255, 255, //  88 -  95
    255, 026, 027, 028, 029, 030, 031, 032, //  96 - 103
    033, 034, 035, 036, 037, 038, 039, 040, // 104 - 111
    041, 042, 043, 044, 045, 046, 047, 048, // 112 - 119
    049, 050, 051, 255, 255, 255, 255, 255, // 120 - 127
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255);

{*****************************************************************************************}
procedure ALMimeEncode(const InputBuffer: TALDynByteArray; const InputByteCount: ALSizeInt;
  out OutputBuffer: TALDynByteArray);
begin
  ALMimeEncode(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

{***********************************************************************************************}
procedure ALMimeEncodeNoCRLF(const InputBuffer: TALDynByteArray; const InputByteCount: ALSizeInt;
  out OutputBuffer: TALDynByteArray);
begin
  ALMimeEncodeNoCRLF(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

{**************************************************************************************************}
procedure ALMimeEncodeFullLines(const InputBuffer: TALDynByteArray; const InputByteCount: ALSizeInt;
  out OutputBuffer: TALDynByteArray);
begin
  ALMimeEncodeFullLines(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

{****************************************************************************************}
function ALMimeDecode(const InputBuffer: TALDynByteArray; const InputByteCount: ALSizeInt;
  out OutputBuffer: TALDynByteArray): ALSizeInt;
begin
  Result := ALMimeDecode(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

{***********************************************************************************************}
function ALMimeDecodePartial(const InputBuffer: TALDynByteArray; const InputByteCount: ALSizeInt;
  out OutputBuffer: TALDynByteArray; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): ALSizeInt;
begin
  Result := ALMimeDecodePartial(InputBuffer, 0, InputByteCount, OutputBuffer, 0, ByteBuffer, ByteBufferSpace);
end;

{********************************************************************************************}
function ALMimeDecodePartialEnd(out OutputBuffer: TALDynByteArray; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): ALSizeInt;
begin
  Result := ALMimeDecodePartialEnd(OutputBuffer, 0, ByteBuffer, ByteBufferSpace);
end;

{**}
type
  PByte4 = ^TByte4;
  TByte4 = packed record
    B1: Byte;
    B2: Byte;
    B3: Byte;
    B4: Byte;
  end;

  PByte3 = ^TByte3;
  TByte3 = packed record
    B1: Byte;
    B2: Byte;
    B3: Byte;
  end;

{*******************************}
// Wrapper functions & procedures
{$IFNDEF NEXTGEN}
function ALMimeEncodeString(const S: AnsiString): AnsiString;
var
  L: ALSizeInt;
begin
  if S <> '' then
  begin
    L := Length(S);
    SetLength(Result, ALMimeEncodedSize(L));
    ALMimeEncode(PAnsiChar(S)^, L, PAnsiChar(Result)^);
  end
  else
    Result := '';
end;
{$ENDIF}

{***************}
{$IFNDEF NEXTGEN}
function ALMimeEncodeStringNoCRLF(const S: AnsiString): AnsiString;
var
  L: ALSizeInt;
begin
  if S <> '' then
  begin
    L := Length(S);
    SetLength(Result, ALMimeEncodedSizeNoCRLF(L));
    ALMimeEncodeNoCRLF(PAnsiChar(S)^, L, PAnsiChar(Result)^);
  end
  else
    Result := '';
end;
{$ENDIF}

{***************}
{$IFNDEF NEXTGEN}
function ALMimeDecodeString(const S: AnsiString): AnsiString;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  L: ALSizeInt;
  P, R: PAnsiChar;
begin
  if S <> '' then
  begin
    L := Length(S);
    SetLength(Result, ALMimeDecodedSize(L));
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    P := PAnsiChar(S);
    R := PAnsiChar(Result);
    L := ALMimeDecodePartial(P^, L, R^, ByteBuffer, ByteBufferSpace);
    Inc(R, L);
    Inc(L, ALMimeDecodePartialEnd(R^, ByteBuffer, ByteBufferSpace));
    SetLength(Result, L);
  end
  else
    Result := '';
end;
{$ENDIF}

{**************************************************************************************}
function ALMimeEncodeStringU(const S: String; const AEncoding: TEncoding = nil): String;
var
  L: ALSizeInt;
  BufIn: TBytes;
  BufOut: TBytes;
begin
  if S <> '' then
  begin
    if assigned(AEncoding) then BufIn := AEncoding.GetBytes(S)
    else BufIn := TEncoding.unicode.GetBytes(S);
    L := Length(BufIn);
    SetLength(BufOut, ALMimeEncodedSize(L));
    ALMimeEncode(BufIn[0], L, BufOut[0]);
    result := TEncoding.UTF8.GetString(BufOut); // BufOut must contain only low ascii chars with are compatible with UTF8 (i could also take ASCII)
  end
  else
    Result := '';
end;

{********************************************************************************************}
function ALMimeEncodeStringNoCRLFU(const S: String; const AEncoding: TEncoding = nil): String;
var
  L: ALSizeInt;
  BufIn: TBytes;
  BufOut: TBytes;
begin
  if S <> '' then
  begin
    if assigned(AEncoding) then BufIn := AEncoding.GetBytes(S)
    else BufIn := TEncoding.unicode.GetBytes(S);
    L := Length(BufIn);
    SetLength(BufOut, ALMimeEncodedSizeNoCRLF(L));
    ALMimeEncodeNoCRLF(BufIn[0], L, BufOut[0]);
    result := TEncoding.UTF8.GetString(BufOut);  // BufOut must contain only low ascii chars with are compatible with UTF8 (i could also take ASCII)
  end
  else
    Result := '';
end;

{**************************************************************************************}
function ALMimeDecodeStringU(const S: String; const AEncoding: TEncoding = nil): String;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  L: ALSizeInt;
  BufIn: TBytes;
  BufOut: TBytes;
begin
  if S <> '' then
  begin
    BufIn := TEncoding.utf8.GetBytes(S); // BufIn must contain only low ascii chars with are compatible with UTF8 (i could also take ASCII)
    L := Length(BufIn);
    SetLength(BufOut, ALMimeDecodedSize(L));
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    L := ALMimeDecodePartial(BufIn[0], L, BufOut[0], ByteBuffer, ByteBufferSpace);
    Inc(L, ALMimeDecodePartialEnd(BufOut[L], ByteBuffer, ByteBufferSpace));
    SetLength(BufOut, L);
    if assigned(AEncoding) then result := AEncoding.GetString(BufOut)
    else result := TEncoding.unicode.GetString(BufOut);
  end
  else
    Result := '';
end;

{*******************************************************}
function ALMimeEncodeBytesU(const Bytes: Tbytes): String;
var
  L: ALSizeInt;
  BufOut: TBytes;
begin
  L := Length(Bytes);
  if L > 0 then
  begin
    SetLength(BufOut, ALMimeEncodedSize(L));
    ALMimeEncode(Bytes[0], L, BufOut[0]);
    result := TEncoding.UTF8.GetString(BufOut); // BufOut must contain only low ascii chars with are compatible with UTF8 (i could also take ASCII)
  end
  else
    Result := '';
end;

{*************************************************************}
function ALMimeEncodeBytesNoCRLFU(const Bytes: Tbytes): String;
var
  L: ALSizeInt;
  BufOut: TBytes;
begin
  L := Length(Bytes);
  if L > 0 then
  begin
    SetLength(BufOut, ALMimeEncodedSizeNoCRLF(L));
    ALMimeEncodeNoCRLF(Bytes[0], L, BufOut[0]);
    result := TEncoding.UTF8.GetString(BufOut);  // BufOut must contain only low ascii chars with are compatible with UTF8 (i could also take ASCII)
  end
  else
    Result := '';
end;

{***************************************************}
function ALMimeDecodeBytesU(const S: String): Tbytes;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  L: ALSizeInt;
  BufIn: TBytes;
begin
  if S <> '' then
  begin
    BufIn := TEncoding.utf8.GetBytes(S); // BufIn must contain only low ascii chars with are compatible with UTF8 (i could also take ASCII)
    L := Length(BufIn);
    SetLength(result, ALMimeDecodedSize(L));
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    L := ALMimeDecodePartial(BufIn[0], L, result[0], ByteBuffer, ByteBufferSpace);
    Inc(L, ALMimeDecodePartialEnd(result[L], ByteBuffer, ByteBufferSpace));
    SetLength(result, L);
  end
  else
    setlength(result, 0);
end;

{*****************}
// Helper functions
function ALMimeEncodedSize(const InputSize: ALSizeInt): ALSizeInt;
begin
  if InputSize > 0 then
    Result := (InputSize + 2) div 3 * 4 + (InputSize - 1) div cALMIME_DECODED_LINE_BREAK * 2
  else
    Result := InputSize;
end;

{**********************************************************************}
function ALMimeEncodedSizeNoCRLF(const InputSize: ALSizeInt): ALSizeInt;
begin
  Result := (InputSize + 2) div 3 * 4;
end;

{****************************************************************}
function ALMimeDecodedSize(const InputSize: ALSizeInt): ALSizeInt;
begin
  Result := (InputSize + 3) div 4 * 3;
end;

{*******************************}
// Primary functions & procedures
procedure ALMimeEncode(const InputBuffer: TALDynByteArray; InputOffset: ALSizeInt;
  const InputByteCount: ALSizeInt;
  out OutputBuffer: TALDynByteArray; OutputOffset: ALSizeInt);
var
  IDelta, ODelta: ALSizeInt;
begin
  ALMimeEncodeFullLines(InputBuffer, InputOffset, InputByteCount, OutputBuffer, OutputOffset);
  IDelta := InputByteCount div cALMIME_DECODED_LINE_BREAK; // Number of lines processed so far.
  ODelta := IDelta * (cALMIME_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * cALMIME_DECODED_LINE_BREAK;
  ALMimeEncodeNoCRLF(InputBuffer, InputOffset + IDelta, InputByteCount - IDelta, OutputBuffer, OutputOffset + ODelta);
end;

{*******************************}
// Primary functions & procedures
procedure ALMimeEncode(const InputBuffer; const InputByteCount: ALSizeInt; out OutputBuffer);
var
  IDelta, ODelta: ALSizeInt;
  I, O: PByte;
begin
  ALMimeEncodeFullLines(InputBuffer, InputByteCount, OutputBuffer);
  IDelta := InputByteCount div cALMIME_DECODED_LINE_BREAK; // Number of lines processed so far.
  ODelta := IDelta * (cALMIME_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * cALMIME_DECODED_LINE_BREAK;
  I := @InputBuffer;
  Inc(I, IDelta);
  O := @OutputBuffer;
  Inc(O, ODelta);
  ALMimeEncodeNoCRLF(I^, InputByteCount - IDelta, O^);
end;

{*****************************************************************************************}
procedure ALMimeEncodeFullLines(const InputBuffer: TALDynByteArray; InputOffset: ALSizeInt;
  const InputByteCount: ALSizeInt; out OutputBuffer: TALDynByteArray; OutputOffset: ALSizeInt);
var
  B, InnerLimit, OuterLimit: ALSizeInt;
  InIndex: ALSizeInt;
  OutIndex: ALSizeInt;
begin
  { Do we have enough input to encode a full line? }
  if InputByteCount < cALMIME_DECODED_LINE_BREAK then
    Exit;

  InIndex := InputOffset;
  OutIndex := OutputOffset;

  InnerLimit := InIndex;
  Inc(InnerLimit, cALMIME_DECODED_LINE_BREAK);

  OuterLimit := InIndex;
  Inc(OuterLimit, InputByteCount);

  { Multiple line loop. }
  repeat
    { Single line loop. }
    repeat
      { Read 3 bytes from InputBuffer. }
      B := InputBuffer[InIndex + 0];
      B := B shl 8;
      B := B or InputBuffer[InIndex + 1];
      B := B shl 8;
      B := B or InputBuffer[InIndex + 2];
      Inc(InIndex, 3);
      { Write 4 bytes to OutputBuffer (in reverse order). }
      OutputBuffer[OutIndex + 3] := cALMIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutputBuffer[OutIndex + 2] := cALMIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutputBuffer[OutIndex + 1] := cALMIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutputBuffer[OutIndex + 0] := cALMIME_ENCODE_TABLE[B];
      Inc(OutIndex, 3);
    until InIndex >= InnerLimit;

    { Write line break (CRLF). }
    OutputBuffer[OutIndex + 0] := 13;
    OutputBuffer[OutIndex + 1] := 10;
    Inc(OutIndex, 2);

    Inc(InnerLimit, cALMIME_DECODED_LINE_BREAK);
  until InnerLimit > OuterLimit;
end;

{****************************************************************************************************}
procedure ALMimeEncodeFullLines(const InputBuffer; const InputByteCount: ALSizeInt; out OutputBuffer);
var
  B: Cardinal;
  InnerLimit, OuterLimit: TALAddr;
  InPtr: PByte3;
  OutPtr: PByte4;
begin
  { Do we have enough input to encode a full line? }
  if InputByteCount < cALMIME_DECODED_LINE_BREAK then
    Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  InnerLimit := TALAddr(InPtr);
  Inc(InnerLimit, cALMIME_DECODED_LINE_BREAK);

  OuterLimit := TALAddr(InPtr);
  Inc(OuterLimit, InputByteCount);

  { Multiple line loop. }
  repeat
    { Single line loop. }
    repeat
      { Read 3 bytes from InputBuffer. }
      B := InPtr^.B1;
      B := B shl 8;
      B := B or InPtr^.B2;
      B := B shl 8;
      B := B or InPtr^.B3;
      Inc(InPtr);
      { Write 4 bytes to OutputBuffer (in reverse order). }
      OutPtr^.B4 := cALMIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.B3 := cALMIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.B2 := cALMIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.B1 := cALMIME_ENCODE_TABLE[B];
      Inc(OutPtr);
    until TALAddr(InPtr) >= InnerLimit;

    { Write line break (CRLF). }
    OutPtr^.B1 := 13;
    OutPtr^.B2 := 10;
    Inc(TALAddr(OutPtr), 2);

    Inc(InnerLimit, cALMIME_DECODED_LINE_BREAK);
  until InnerLimit > OuterLimit;
end;

{**************************************************************************************}
procedure ALMimeEncodeNoCRLF(const InputBuffer: TALDynByteArray; InputOffset: ALSizeInt;
  const InputByteCount: ALSizeInt; out OutputBuffer: TALDynByteArray; OutputOffset: ALSizeInt);
var
  B, InnerLimit, OuterLimit: ALSizeInt;
  InIndex: ALSizeInt;
  OutIndex: ALSizeInt;
begin
  if InputByteCount = 0 then
    Exit;

  InIndex := InputOffset;
  OutIndex := OutputOffset;

  OuterLimit := InputByteCount div 3 * 3;

  InnerLimit := InIndex;
  Inc(InnerLimit, OuterLimit);

  { Last line loop. }
  while InIndex < InnerLimit do
  begin
    { Read 3 bytes from InputBuffer. }
    B := InputBuffer[InIndex + 0];
    B := B shl 8;
    B := B or InputBuffer[InIndex + 1];
    B := B shl 8;
    B := B or InputBuffer[InIndex + 2];
    Inc(InIndex, 3);
    { Write 4 bytes to OutputBuffer (in reverse order). }
    OutputBuffer[OutIndex + 3] := cALMIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutputBuffer[OutIndex + 2] := cALMIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutputBuffer[OutIndex + 1] := cALMIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutputBuffer[OutIndex + 0] := cALMIME_ENCODE_TABLE[B];
    Inc(OutIndex, 3);
  end;

  { End of data & padding. }
  case InputByteCount - OuterLimit of
    1:
      begin
        B := InputBuffer[InIndex + 0];
        B := B shl 4;
        OutputBuffer[OutIndex + 1] := cALMIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutputBuffer[OutIndex + 0] := cALMIME_ENCODE_TABLE[B];
        OutputBuffer[OutIndex + 2] := cALMIME_PAD_CHAR; { Pad remaining 2 bytes. }
        OutputBuffer[OutIndex + 3] := cALMIME_PAD_CHAR;
      end;
    2:
      begin
        B := InputBuffer[InIndex + 0];
        B := B shl 8;
        B := B or InputBuffer[InIndex + 1];
        B := B shl 2;
        OutputBuffer[OutIndex + 2] := cALMIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutputBuffer[OutIndex + 1] := cALMIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutputBuffer[OutIndex + 0] := cALMIME_ENCODE_TABLE[B];
        OutputBuffer[OutIndex + 3] := cALMIME_PAD_CHAR; { Pad remaining byte. }
      end;
  end;
end;

{*************************************************************************************************}
procedure ALMimeEncodeNoCRLF(const InputBuffer; const InputByteCount: ALSizeInt; out OutputBuffer);
var
  B: Cardinal;
  InnerLimit, OuterLimit: ALSizeInt;
  InPtr: PByte3;
  OutPtr: PByte4;
begin
  if InputByteCount = 0 then
    Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  OuterLimit := InputByteCount div 3 * 3;

  InnerLimit := TALAddr(InPtr);
  Inc(InnerLimit, OuterLimit);

  { Last line loop. }
  while TALAddr(InPtr) < TALAddr(InnerLimit) do
  begin
    { Read 3 bytes from InputBuffer. }
    B := InPtr^.B1;
    B := B shl 8;
    B := B or InPtr^.B2;
    B := B shl 8;
    B := B or InPtr^.B3;
    Inc(InPtr);
    { Write 4 bytes to OutputBuffer (in reverse order). }
    OutPtr^.B4 := cALMIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.B3 := cALMIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.B2 := cALMIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.B1 := cALMIME_ENCODE_TABLE[B];
    Inc(OutPtr);
  end;

  { End of data & padding. }
  case InputByteCount - OuterLimit of
    1:
      begin
        B := InPtr^.B1;
        B := B shl 4;
        OutPtr.B2 := cALMIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B1 := cALMIME_ENCODE_TABLE[B];
        OutPtr.B3 := cALMIME_PAD_CHAR; { Pad remaining 2 bytes. }
        OutPtr.B4 := cALMIME_PAD_CHAR;
      end;
    2:
      begin
        B := InPtr^.B1;
        B := B shl 8;
        B := B or InPtr^.B2;
        B := B shl 2;
        OutPtr.B3 := cALMIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B2 := cALMIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B1 := cALMIME_ENCODE_TABLE[B];
        OutPtr.B4 := cALMIME_PAD_CHAR; { Pad remaining byte. }
      end;
  end;
end;

{**************}
// Decoding Core
function ALMimeDecode(const InputBuffer: TALDynByteArray; InputOffset: ALSizeInt;
  const InputByteCount: ALSizeInt; out OutputBuffer: TALDynByteArray; OutputOffset: ALSizeInt): ALSizeInt;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  Result := ALMimeDecodePartial(InputBuffer, InputOffset, InputByteCount, OutputBuffer, OutputOffset, ByteBuffer, ByteBufferSpace);
  Inc(Result, ALMimeDecodePartialEnd(OutputBuffer, OutputOffset + Result, ByteBuffer, ByteBufferSpace));
end;

{**************}
// Decoding Core
function ALMimeDecode(const InputBuffer; const InputByteCount: ALSizeInt; out OutputBuffer): ALSizeInt;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  O: PByte;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  Result := ALMimeDecodePartial(InputBuffer, InputByteCount, OutputBuffer, ByteBuffer, ByteBufferSpace);
  O := @OutputBuffer;
  Inc(O, Result);
  Inc(Result, ALMimeDecodePartialEnd(O^, ByteBuffer, ByteBufferSpace));
end;

{**************************************************************************************}
function ALMimeDecodePartial(const InputBuffer: TALDynByteArray; InputOffset: ALSizeInt;
  const InputByteCount: ALSizeInt; out OutputBuffer: TALDynByteArray; OutputOffset: ALSizeInt;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): ALSizeInt;
var
  LByteBuffer, LByteBufferSpace, C: Cardinal;
  InIndex, OuterLimit: ALSizeInt;
  OutIndex: ALSizeInt;
begin
  if InputByteCount > 0 then
    begin
      InIndex := InputOffset;
      OuterLimit := InIndex + InputByteCount;
      OutIndex := OutputOffset;
      LByteBuffer := ByteBuffer;
      LByteBufferSpace := ByteBufferSpace;
      while InIndex < OuterLimit do
      begin
        { Read from InputBuffer. }
        C := cALMIME_DECODE_TABLE[InputBuffer[InIndex]];
        Inc(InIndex);
        if C = $FF then
          Continue;
        LByteBuffer := LByteBuffer shl 6;
        LByteBuffer := LByteBuffer or C;
        Dec(LByteBufferSpace);
        { Have we read 4 bytes from InputBuffer? }
        if LByteBufferSpace <> 0 then
          Continue;

        { Write 3 bytes to OutputBuffer (in reverse order). }
        OutputBuffer[OutIndex + 2] := Byte(LByteBuffer);
        LByteBuffer := LByteBuffer shr 8;
        OutputBuffer[OutIndex + 1] := Byte(LByteBuffer);
        LByteBuffer := LByteBuffer shr 8;
        OutputBuffer[OutIndex + 0] := Byte(LByteBuffer);
        LByteBuffer := 0;
        Inc(OutIndex, 3);
        LByteBufferSpace := 4;
      end;
      ByteBuffer := LByteBuffer;
      ByteBufferSpace := LByteBufferSpace;
      Result := OutIndex - OutputOffset;
    end
  else
    Result := 0;
end;

{************************************************************************************************}
function ALMimeDecodePartial(const InputBuffer; const InputByteCount: ALSizeInt; out OutputBuffer;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): ALSizeInt;
var
  LByteBuffer, LByteBufferSpace, C: Cardinal;
  InPtr, OuterLimit: PByte;
  OutPtr: PByte3;
begin
  if InputByteCount > 0 then
  begin
    InPtr := @InputBuffer;
    OuterLimit := Pointer(TALAddr(InPtr) + TALAddr(InputByteCount));
    OutPtr := @OutputBuffer;
    LByteBuffer := ByteBuffer;
    LByteBufferSpace := ByteBufferSpace;
    while InPtr <> OuterLimit do
    begin
      { Read from InputBuffer. }
      C := cALMIME_DECODE_TABLE[InPtr^];
      Inc(InPtr);
      if C = $FF then
        Continue;
      LByteBuffer := LByteBuffer shl 6;
      LByteBuffer := LByteBuffer or C;
      Dec(LByteBufferSpace);
      { Have we read 4 bytes from InputBuffer? }
      if LByteBufferSpace <> 0 then
        Continue;

      { Write 3 bytes to OutputBuffer (in reverse order). }
      OutPtr^.B3 := Byte(LByteBuffer);
      LByteBuffer := LByteBuffer shr 8;
      OutPtr^.B2 := Byte(LByteBuffer);
      LByteBuffer := LByteBuffer shr 8;
      OutPtr^.B1 := Byte(LByteBuffer);
      LByteBuffer := 0;
      Inc(OutPtr);
      LByteBufferSpace := 4;
    end;
    ByteBuffer := LByteBuffer;
    ByteBufferSpace := LByteBufferSpace;
    Result := ALSizeInt(TALAddr(OutPtr) - TALAddr(@OutputBuffer));
  end
  else
    Result := 0;
end;

{*****************************************************************************************}
function ALMimeDecodePartialEnd(out OutputBuffer: TALDynByteArray; OutputOffset: ALSizeInt;
  const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): ALSizeInt;
var
  LByteBuffer: Cardinal;
begin
  case ByteBufferSpace of
    1:
      begin
        LByteBuffer := ByteBuffer shr 2;
        OutputBuffer[OutputOffset + 1] := Byte(LByteBuffer);
        LByteBuffer := LByteBuffer shr 8;
        OutputBuffer[OutputOffset + 0] := Byte(LByteBuffer);
        Result := 2;
      end;
    2:
      begin
        LByteBuffer := ByteBuffer shr 4;
        OutputBuffer[OutputOffset + 0] := Byte(LByteBuffer);
        Result := 1;
      end;
  else
    Result := 0;
  end;
end;

{***************************************************************************}
function ALMimeDecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): ALSizeInt;
var
  LByteBuffer: Cardinal;
begin
  case ByteBufferSpace of
    1:
      begin
        LByteBuffer := ByteBuffer shr 2;
        PByte3(@OutputBuffer)^.B2 := Byte(LByteBuffer);
        LByteBuffer := LByteBuffer shr 8;
        PByte3(@OutputBuffer)^.B1 := Byte(LByteBuffer);
        Result := 2;
      end;
    2:
      begin
        LByteBuffer := ByteBuffer shr 4;
        PByte3(@OutputBuffer)^.B1 := Byte(LByteBuffer);
        Result := 1;
      end;
  else
    Result := 0;
  end;
end;

{*************************}
// File Encoding & Decoding
procedure ALMimeEncodeFile(const InputFileName, OutputFileName: TFileName);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      ALMimeEncodeStream(InputStream, OutputStream);
    finally
      ALFreeAndNil(OutputStream);
    end;
  finally
    ALFreeAndNil(InputStream);
  end;
end;

{*******************************************************************************}
procedure ALMimeEncodeFileNoCRLF(const InputFileName, OutputFileName: TFileName);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      ALMimeEncodeStreamNoCRLF(InputStream, OutputStream);
    finally
      ALFreeAndNil(OutputStream);
    end;
  finally
    ALFreeAndNil(InputStream);
  end;
end;

{*************************************************************************}
procedure ALMimeDecodeFile(const InputFileName, OutputFileName: TFileName);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      ALMimeDecodeStream(InputStream, OutputStream);
    finally
      ALFreeAndNil(OutputStream);
    end;
  finally
    ALFreeAndNil(InputStream);
  end;
end;

{***************************}
// Stream Encoding & Decoding
procedure ALMimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array [0..cALMIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array [0..(cALMIME_BUFFER_SIZE + 2) div 3 * 4 + cALMIME_BUFFER_SIZE div cALMIME_DECODED_LINE_BREAK * 2 - 1] of Byte;
  BytesRead: ALSizeInt;
  IDelta, ODelta: ALSizeInt;
  I, O: PByte;
begin
  InputBuffer[0] := 0;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));

  while BytesRead = Length(InputBuffer) do
  begin
    ALMimeEncodeFullLines(InputBuffer, Length(InputBuffer), OutputBuffer);
    OutputStream.WriteBuffer(OutputBuffer, Length(OutputBuffer));
    BytesRead := InputStream.Read(InputBuffer, Length(InputBuffer));
  end;

  ALMimeEncodeFullLines(InputBuffer, BytesRead, OutputBuffer);

  IDelta := BytesRead div cALMIME_DECODED_LINE_BREAK; // Number of lines processed.
  ODelta := IDelta * (cALMIME_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * cALMIME_DECODED_LINE_BREAK;

  I := @InputBuffer;
  Inc(I, IDelta);
  O := @OutputBuffer;
  Inc(O, ODelta);

  ALMimeEncodeNoCRLF(I^, BytesRead - IDelta, O^);

  OutputStream.WriteBuffer(OutputBuffer, ALMimeEncodedSize(BytesRead));
end;

{******************************************************************************************}
procedure ALMimeEncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array [0..cALMIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array [0..((cALMIME_BUFFER_SIZE + 2) div 3) * 4 - 1] of Byte;
  BytesRead: ALSizeInt;
begin
  InputBuffer[0] := 0;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));

  while BytesRead = Length(InputBuffer) do
  begin
    ALMimeEncodeNoCRLF(InputBuffer, Length(InputBuffer), OutputBuffer);
    OutputStream.WriteBuffer(OutputBuffer, Length(OutputBuffer));
    BytesRead := InputStream.Read(InputBuffer, Length(InputBuffer));
  end;

  ALMimeEncodeNoCRLF(InputBuffer, BytesRead, OutputBuffer);
  OutputStream.WriteBuffer(OutputBuffer, ALMimeEncodedSizeNoCRLF(BytesRead));
end;

{************************************************************************************}
procedure ALMimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  InputBuffer: array [0..cALMIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array [0..(cALMIME_BUFFER_SIZE + 3) div 4 * 3 - 1] of Byte;
  BytesRead: ALSizeInt;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  InputBuffer[0] := 0;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));

  while BytesRead > 0 do
  begin
    OutputStream.WriteBuffer(OutputBuffer, ALMimeDecodePartial(InputBuffer, BytesRead, OutputBuffer, ByteBuffer, ByteBufferSpace));
    BytesRead := InputStream.Read(InputBuffer, Length(InputBuffer));
  end;
  OutputStream.WriteBuffer(OutputBuffer, ALMimeDecodePartialEnd(OutputBuffer, ByteBuffer, ByteBufferSpace));
end;

{$IFNDEF NEXTGEN}

{************************************************************************************}
Function ALGetDefaultFileExtFromMimeContentType(aContentType: AnsiString): AnsiString;
Var P: integer;
    Index : Integer;
Begin
  Result := '';

  aContentType := ALLowerCase(aContentType);
  P := AlPosEx(';',aContentType);
  if (P > 0) then delete(aContentType,P,MaxInt);
  aContentType := ALTrim(AContentType);

  Index := vAlExtbyMimeContentTypeList.IndexOfName(aContentType);
  if Index <> -1 then Result := vAlExtbyMimeContentTypeList.ValueFromIndex[Index];
end;

{******************************************************************************}
Function ALGetDefaultMIMEContentTypeFromExt(const aExt: AnsiString): AnsiString;
var Index : Integer;
    LExt: AnsiString;
begin
  LExt := AlLowerCase(aExt);
  If (LExt = '') or (LExt[low(AnsiString)] <> '.') then LExt := '.' + LExt;
  Index := vAlMimeContentTypeByExtList.IndexOfName(LExt);
  if Index <> -1 then Result := vAlMimeContentTypeByExtList.ValueFromIndex[Index]
  else Result := 'application/octet-stream';
end;

{************************}
procedure ALFillMimeTable;
var i: integer;
begin
  {NOTE:  All of these strings should never be translated
  because they are protocol specific and are important for some
  web-browsers}

  { Animation }
  vAlMimeContentTypeByExtList.Add('.nml=animation/narrative');    {Do not Localize}

  { Audio }
  vAlMimeContentTypeByExtList.Add('.aac=audio/mp4');
  vAlMimeContentTypeByExtList.Add('.aif=audio/x-aiff');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.aifc=audio/x-aiff');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.aiff=audio/x-aiff');    {Do not Localize}

  vAlMimeContentTypeByExtList.Add('.au=audio/basic');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.gsm=audio/x-gsm');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.kar=audio/midi');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.m3u=audio/mpegurl');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.m4a=audio/x-mpg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mid=audio/midi');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.midi=audio/midi');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mpega=audio/x-mpg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mp2=audio/x-mpg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mp3=audio/x-mpg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mpga=audio/x-mpg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.m3u=audio/x-mpegurl');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pls=audio/x-scpls');   {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.qcp=audio/vnd.qcelp');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ra=audio/x-realaudio');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ram=audio/x-pn-realaudio');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rm=audio/x-pn-realaudio');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sd2=audio/x-sd2');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sid=audio/prs.sid');   {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.snd=audio/basic');   {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wav=audio/x-wav');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wax=audio/x-ms-wax');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wma=audio/x-ms-wma');    {Do not Localize}

  vAlMimeContentTypeByExtList.Add('.mjf=audio/x-vnd.AudioExplosion.MjuiceMediaFile');    {Do not Localize}

  { Image }
  vAlMimeContentTypeByExtList.Add('.art=image/x-jg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.bmp=image/bmp');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.cdr=image/x-coreldraw');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.cdt=image/x-coreldrawtemplate');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.cpt=image/x-corelphotopaint');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.djv=image/vnd.djvu');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.djvu=image/vnd.djvu');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.gif=image/gif');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ief=image/ief');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ico=image/x-icon');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.jng=image/x-jng');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.jpg=image/jpeg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.jpeg=image/jpeg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.jpe=image/jpeg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pat=image/x-coreldrawpattern');   {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pcx=image/pcx');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pbm=image/x-portable-bitmap');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pgm=image/x-portable-graymap');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pict=image/x-pict');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.png=image/x-png');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pnm=image/x-portable-anymap');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pntg=image/x-macpaint');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ppm=image/x-portable-pixmap');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.psd=image/x-psd');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.qtif=image/x-quicktime');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ras=image/x-cmu-raster');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rf=image/vnd.rn-realflash');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rgb=image/x-rgb');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rp=image/vnd.rn-realpix');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sgi=image/x-sgi');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.svg=image/svg+xml');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.svgz=image/svg+xml');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.targa=image/x-targa');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.tif=image/x-tiff');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wbmp=image/vnd.wap.wbmp');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.webp=image/webp'); {Do not localize}
  vAlMimeContentTypeByExtList.Add('.xbm=image/xbm');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.xbm=image/x-xbitmap');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.xpm=image/x-xpixmap');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.xwd=image/x-xwindowdump');    {Do not Localize}

  { Text }
  vAlMimeContentTypeByExtList.Add('.323=text/h323');    {Do not Localize}

  vAlMimeContentTypeByExtList.Add('.xml=text/xml');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.uls=text/iuls');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.txt=text/plain');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rtx=text/richtext');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wsc=text/scriptlet');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rt=text/vnd.rn-realtext');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.htt=text/webviewhtml');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.htc=text/x-component');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.vcf=text/x-vcard');    {Do not Localize}

  { Video }
  vAlMimeContentTypeByExtList.Add('.asf=video/x-ms-asf');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.asx=video/x-ms-asf');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.avi=video/x-msvideo');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.dl=video/dl');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.dv=video/dv');  {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.flc=video/flc');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.fli=video/fli');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.gl=video/gl');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.lsf=video/x-la-asf');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.lsx=video/x-la-asf');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mng=video/x-mng');    {Do not Localize}

  vAlMimeContentTypeByExtList.Add('.mp2=video/mpeg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mp3=video/mpeg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mp4=video/mpeg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mpeg=video/x-mpeg2a');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mpa=video/mpeg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mpe=video/mpeg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mpg=video/mpeg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ogv=video/ogg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.moov=video/quicktime');     {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mov=video/quicktime');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mxu=video/vnd.mpegurl');   {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.qt=video/quicktime');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.qtc=video/x-qtc'); {Do not loccalize}
  vAlMimeContentTypeByExtList.Add('.rv=video/vnd.rn-realvideo');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ivf=video/x-ivf');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.webm=video/webm');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wm=video/x-ms-wm');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wmp=video/x-ms-wmp');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wmv=video/x-ms-wmv');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wmx=video/x-ms-wmx');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wvx=video/x-ms-wvx');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rms=video/vnd.rn-realvideo-secure');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.asx=video/x-ms-asf-plugin');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.movie=video/x-sgi-movie');    {Do not Localize}

  { Application }
  vAlMimeContentTypeByExtList.Add('.7z=application/x-7z-compressed');   {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.a=application/x-archive');   {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.aab=application/x-authorware-bin');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.aam=application/x-authorware-map');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.aas=application/x-authorware-seg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.abw=application/x-abiword');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ace=application/x-ace-compressed');  {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ai=application/postscript');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.alz=application/x-alz-compressed');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ani=application/x-navi-animation');   {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.arj=application/x-arj');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.asf=application/vnd.ms-asf');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.bat=application/x-msdos-program');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.bcpio=application/x-bcpio');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.boz=application/x-bzip2');     {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.bz=application/x-bzip');
  vAlMimeContentTypeByExtList.Add('.bz2=application/x-bzip2');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.cab=application/vnd.ms-cab-compressed');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.cat=application/vnd.ms-pki.seccat');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ccn=application/x-cnc');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.cco=application/x-cocoa');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.cdf=application/x-cdf');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.cer=application/x-x509-ca-cert');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.chm=application/vnd.ms-htmlhelp');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.chrt=application/vnd.kde.kchart');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.cil=application/vnd.ms-artgalry');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.class=application/java-vm');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.com=application/x-msdos-program');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.clp=application/x-msclip');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.cpio=application/x-cpio');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.cpt=application/mac-compactpro');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.cqk=application/x-calquick');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.crd=application/x-mscardfile');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.crl=application/pkix-crl');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.csh=application/x-csh');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.dar=application/x-dar');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.dbf=application/x-dbase');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.dcr=application/x-director');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.deb=application/x-debian-package');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.dir=application/x-director');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.dist=vnd.apple.installer+xml');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.distz=vnd.apple.installer+xml');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.dll=application/x-msdos-program');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.dmg=application/x-apple-diskimage');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.doc=application/msword');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.dot=application/msword');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.dvi=application/x-dvi');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.dxr=application/x-director');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ebk=application/x-expandedbook');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.eps=application/postscript');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.evy=application/envoy');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.exe=application/x-msdos-program');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.fdf=application/vnd.fdf');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.fif=application/fractals');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.flm=application/vnd.kde.kivio');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.fml=application/x-file-mirror-list');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.gzip=application/x-gzip');  {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.gnumeric=application/x-gnumeric');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.gtar=application/x-gtar');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.gz=application/x-gzip');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.hdf=application/x-hdf');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.hlp=application/winhlp');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.hpf=application/x-icq-hpf');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.hqx=application/mac-binhex40');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.hta=application/hta');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ims=application/vnd.ms-ims');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ins=application/x-internet-signup');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.iii=application/x-iphone');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.iso=application/x-iso9660-image');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.jar=application/java-archive');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.karbon=application/vnd.kde.karbon');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.kfo=application/vnd.kde.kformula');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.kon=application/vnd.kde.kontour');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.kpr=application/vnd.kde.kpresenter');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.kpt=application/vnd.kde.kpresenter');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.kwd=application/vnd.kde.kword');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.kwt=application/vnd.kde.kword');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.latex=application/x-latex');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.lha=application/x-lzh');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.lcc=application/fastman');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.lrm=application/vnd.ms-lrm');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.lz=application/x-lzip');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.lzh=application/x-lzh');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.lzma=application/x-lzma');  {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.lzo=application/x-lzop'); {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.lzx=application/x-lzx');
  vAlMimeContentTypeByExtList.Add('.m13=application/x-msmediaview');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.m14=application/x-msmediaview');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mpp=application/vnd.ms-project');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mvb=application/x-msmediaview');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.man=application/x-troff-man');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mdb=application/x-msaccess');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.me=application/x-troff-me');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ms=application/x-troff-ms');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.msi=application/x-msi');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mpkg=vnd.apple.installer+xml');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mny=application/x-msmoney');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.nix=application/x-mix-transfer');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.o=application/x-object');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.oda=application/oda');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.odb=application/vnd.oasis.opendocument.database');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.odc=application/vnd.oasis.opendocument.chart');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.odf=application/vnd.oasis.opendocument.formula');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.odg=application/vnd.oasis.opendocument.graphics');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.odi=application/vnd.oasis.opendocument.image');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.odm=application/vnd.oasis.opendocument.text-master');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.odp=application/vnd.oasis.opendocument.presentation');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ods=application/vnd.oasis.opendocument.spreadsheet');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ogg=application/ogg');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.odt=application/vnd.oasis.opendocument.text');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.otg=application/vnd.oasis.opendocument.graphics-template');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.oth=application/vnd.oasis.opendocument.text-web');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.otp=application/vnd.oasis.opendocument.presentation-template');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ots=application/vnd.oasis.opendocument.spreadsheet-template');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ott=application/vnd.oasis.opendocument.text-template');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.p10=application/pkcs10');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.p12=application/x-pkcs12');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.p7b=application/x-pkcs7-certificates');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.p7m=application/pkcs7-mime');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.p7r=application/x-pkcs7-certreqresp');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.p7s=application/pkcs7-signature');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.package=application/vnd.autopackage');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pfr=application/font-tdpfr');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pkg=vnd.apple.installer+xml');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pdf=application/pdf');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pko=application/vnd.ms-pki.pko');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pl=application/x-perl');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pnq=application/x-icq-pnq');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pot=application/mspowerpoint');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pps=application/mspowerpoint');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ppt=application/mspowerpoint');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ppz=application/mspowerpoint');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ps=application/postscript');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pub=application/x-mspublisher');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.qpw=application/x-quattropro');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.qtl=application/x-quicktimeplayer');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rar=application/rar');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rdf=application/rdf+xml');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rjs=application/vnd.rn-realsystem-rjs');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rm=application/vnd.rn-realmedia');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rmf=application/vnd.rmf');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rmp=application/vnd.rn-rn_music_package');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rmx=application/vnd.rn-realsystem-rmx');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rnx=application/vnd.rn-realplayer');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rpm=application/x-redhat-package-manager');
  vAlMimeContentTypeByExtList.Add('.rsml=application/vnd.rn-rsml');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rtsp=application/x-rtsp');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.rss=application/rss+xml');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.scm=application/x-icq-scm');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ser=application/java-serialized-object');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.scd=application/x-msschedule');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sda=application/vnd.stardivision.draw');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sdc=application/vnd.stardivision.calc');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sdd=application/vnd.stardivision.impress');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sdp=application/x-sdp');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.setpay=application/set-payment-initiation');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.setreg=application/set-registration-initiation');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sh=application/x-sh');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.shar=application/x-shar');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.shw=application/presentations');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sit=application/x-stuffit');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sitx=application/x-stuffitx');  {Do not localize}
  vAlMimeContentTypeByExtList.Add('.skd=application/x-koan');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.skm=application/x-koan');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.skp=application/x-koan');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.skt=application/x-koan');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.smf=application/vnd.stardivision.math');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.smi=application/smil');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.smil=application/smil');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.spl=application/futuresplash');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ssm=application/streamingmedia');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sst=application/vnd.ms-pki.certstore');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.stc=application/vnd.sun.xml.calc.template');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.std=application/vnd.sun.xml.draw.template');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sti=application/vnd.sun.xml.impress.template');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.stl=application/vnd.ms-pki.stl');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.stw=application/vnd.sun.xml.writer.template');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.svi=application/softvision');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sv4cpio=application/x-sv4cpio');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sv4crc=application/x-sv4crc');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.swf=application/x-shockwave-flash');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.swf1=application/x-shockwave-flash');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sxc=application/vnd.sun.xml.calc');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sxi=application/vnd.sun.xml.impress');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sxm=application/vnd.sun.xml.math');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sxw=application/vnd.sun.xml.writer');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sxg=application/vnd.sun.xml.writer.global');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.t=application/x-troff');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.tar=application/x-tar');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.tcl=application/x-tcl');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.tex=application/x-tex');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.texi=application/x-texinfo');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.texinfo=application/x-texinfo');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.tbz=application/x-bzip-compressed-tar');   {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.tbz2=application/x-bzip-compressed-tar');   {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.tgz=application/x-compressed-tar');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.tlz=application/x-lzma-compressed-tar');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.tr=application/x-troff');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.trm=application/x-msterminal');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.troff=application/x-troff');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.tsp=application/dsptype');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.torrent=application/x-bittorrent');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ttz=application/t-time');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.txz=application/x-xz-compressed-tar'); {Do not localize}
  vAlMimeContentTypeByExtList.Add('.udeb=application/x-debian-package');    {Do not Localize}

  vAlMimeContentTypeByExtList.Add('.uin=application/x-icq');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.urls=application/x-url-list');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.ustar=application/x-ustar');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.vcd=application/x-cdlink');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.vor=application/vnd.stardivision.writer');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.vsl=application/x-cnet-vsl');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wcm=application/vnd.ms-works');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wb1=application/x-quattropro');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wb2=application/x-quattropro');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wb3=application/x-quattropro');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wdb=application/vnd.ms-works');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wks=application/vnd.ms-works');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wmd=application/x-ms-wmd');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wms=application/x-ms-wms');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wmz=application/x-ms-wmz');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wp5=application/wordperfect5.1');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wpd=application/wordperfect');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wpl=application/vnd.ms-wpl');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wps=application/vnd.ms-works');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wri=application/x-mswrite');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.xfdf=application/vnd.adobe.xfdf');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.xls=application/x-msexcel');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.xlb=application/x-msexcel');     {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.xpi=application/x-xpinstall');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.xps=application/vnd.ms-xpsdocument');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.xsd=application/vnd.sun.xml.draw');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.xul=application/vnd.mozilla.xul+xml');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.z=application/x-compress');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.zoo=application/x-zoo');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.zip=application/x-zip-compressed');    {Do not Localize}

  { WAP }
  vAlMimeContentTypeByExtList.Add('.wbmp=image/vnd.wap.wbmp');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wml=text/vnd.wap.wml');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wmlc=application/vnd.wap.wmlc');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wmls=text/vnd.wap.wmlscript');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.wmlsc=application/vnd.wap.wmlscriptc');    {Do not Localize}

  { Non-web text}
  {
  IMPORTANT!!

  You should not use a text MIME type definition unless you are
  extremely certain that the file will NOT be a binary.  Some browsers
  will display the text instead of saving to disk and it looks ugly
  if a web-browser shows all of the 8bit charactors.
  }
  //of course, we have to add this :-).
  vAlMimeContentTypeByExtList.Add('.asm=text/x-asm');   {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.p=text/x-pascal');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.pas=text/x-pascal');    {Do not Localize}

  vAlMimeContentTypeByExtList.Add('.cs=text/x-csharp'); {Do not Localize}

  vAlMimeContentTypeByExtList.Add('.c=text/x-csrc');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.c++=text/x-c++src');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.cpp=text/x-c++src');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.cxx=text/x-c++src');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.cc=text/x-c++src');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.h=text/x-chdr'); {Do not localize}
  vAlMimeContentTypeByExtList.Add('.h++=text/x-c++hdr');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.hpp=text/x-c++hdr');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.hxx=text/x-c++hdr');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.hh=text/x-c++hdr');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.java=text/x-java');    {Do not Localize}

  { WEB }
  vAlMimeContentTypeByExtList.Add('.css=text/css');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.js=text/javascript');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.htm=text/html');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.html=text/html');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.xhtml=application/xhtml+xml'); {Do not localize}
  vAlMimeContentTypeByExtList.Add('.xht=application/xhtml+xml'); {Do not localize}
  vAlMimeContentTypeByExtList.Add('.rdf=application/rdf+xml'); {Do not localize}
  vAlMimeContentTypeByExtList.Add('.rss=application/rss+xml'); {Do not localize}

  vAlMimeContentTypeByExtList.Add('.ls=text/javascript');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.mocha=text/javascript');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.shtml=server-parsed-html');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.xml=text/xml');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sgm=text/sgml');    {Do not Localize}
  vAlMimeContentTypeByExtList.Add('.sgml=text/sgml');    {Do not Localize}

  { Message }
  vAlMimeContentTypeByExtList.Add('.mht=message/rfc822');    {Do not Localize}


  for I := 0 to vAlMimeContentTypeByExtList.Count - 1 do
    vAlExtbyMimeContentTypeList.Add(vAlMimeContentTypeByExtList.ValueFromIndex[i] + '=' + vAlMimeContentTypeByExtList.Names[i]);

end;

{$ENDIF}

Initialization
{$IFNDEF NEXTGEN}
  vAlMimeContentTypeByExtList := TALNVStringList.Create;
  vAlExtbyMimeContentTypeList := TALNVStringList.Create;
  ALFillMimeTable;
  TALNVStringList(vAlMimeContentTypeByExtList).Duplicates := dupAccept;
  TALNVStringList(vAlMimeContentTypeByExtList).Sorted := true;
  TALNVStringList(vAlExtbyMimeContentTypeList).Duplicates := dupAccept;
  TALNVStringList(vAlExtbyMimeContentTypeList).Sorted := true;
{$ENDIF}

finalization
{$IFNDEF NEXTGEN}
  vAlMimeContentTypeByExtList.Free;
  vAlExtbyMimeContentTypeList.Free;
{$ENDIF}

end.
