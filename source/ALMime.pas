{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Jedi Project - JCL
              Stephane Vander Clock (alcinoe@arkadia.com)              
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

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

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALMime;

interface

{$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     System.Classes,
     System.Types,
     System.sysutils,
     {$ELSE}
     Classes,
     Types,
     sysutils,
     {$IFEND}
     ALStringList;

{$if CompilerVersion<=18.5}
//http://stackoverflow.com/questions/7630781/delphi-2007-and-xe2-using-nativeint
type
  NativeInt = Integer;
  NativeUInt = Cardinal;
{$ifend}

//from JCL
function ALMimeBase64EncodeString(const S: AnsiString): AnsiString;
function ALMimeBase64EncodeStringNoCRLF(const S: AnsiString): AnsiString;
function ALMimeBase64DecodeString(const S: AnsiString): AnsiString;
function ALMimeBase64EncodedSize(const InputSize: NativeInt): NativeInt;
function ALMimeBase64EncodedSizeNoCRLF(const InputSize: NativeInt): NativeInt;
function ALMimeBase64DecodedSize(const InputSize: NativeInt): NativeInt;

procedure ALMimeBase64Encode(const InputBuffer: TByteDynArray; InputOffset: NativeInt;
  const InputByteCount: NativeInt; out OutputBuffer: TByteDynArray; OutputOffset: NativeInt = 0); overload;
procedure ALMimeBase64EncodeNoCRLF(const InputBuffer: TByteDynArray; InputOffset: NativeInt;
  const InputByteCount: NativeInt; out OutputBuffer: TByteDynArray; OutputOffset: NativeInt = 0); overload;
procedure ALMimeBase64EncodeFullLines(const InputBuffer: TByteDynArray; InputOffset: NativeInt;
  const InputByteCount: NativeInt; out OutputBuffer: TByteDynArray; OutputOffset: NativeInt = 0); overload;
function ALMimeBase64Decode(const InputBuffer: TByteDynArray; InputOffset: NativeInt;
  const InputByteCount: NativeInt; out OutputBuffer: TByteDynArray; OutputOffset: NativeInt = 0): NativeInt; overload;
function ALMimeBase64DecodePartial(const InputBuffer: TByteDynArray; InputOffset: NativeInt;
  const InputByteCount: NativeInt; out OutputBuffer: TByteDynArray; OutputOffset: NativeInt;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): NativeInt; overload;
function ALMimeBase64DecodePartialEnd(out OutputBuffer: TByteDynArray; OutputOffset: NativeInt;
  const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): NativeInt; overload;

procedure ALMimeBase64Encode(const InputBuffer: TByteDynArray; const InputByteCount: NativeInt;
  out OutputBuffer: TByteDynArray); overload;
procedure ALMimeBase64EncodeNoCRLF(const InputBuffer: TByteDynArray; const InputByteCount: NativeInt;
  out OutputBuffer: TByteDynArray); overload;
procedure ALMimeBase64EncodeFullLines(const InputBuffer: TByteDynArray; const InputByteCount: NativeInt;
  out OutputBuffer: TByteDynArray); overload;
function ALMimeBase64Decode(const InputBuffer: TByteDynArray; const InputByteCount: NativeInt;
  out OutputBuffer: TByteDynArray): NativeInt; overload;
function ALMimeBase64DecodePartial(const InputBuffer: TByteDynArray; const InputByteCount: NativeInt;
  out OutputBuffer: TByteDynArray; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): NativeInt; overload;
function ALMimeBase64DecodePartialEnd(out OutputBuffer: TByteDynArray; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): NativeInt; overload;

procedure ALMimeBase64Encode(const InputBuffer; const InputByteCount: NativeInt; out OutputBuffer); overload;
procedure ALMimeBase64EncodeNoCRLF(const InputBuffer; const InputByteCount: NativeInt; out OutputBuffer); overload;
procedure ALMimeBase64EncodeFullLines(const InputBuffer; const InputByteCount: NativeInt; out OutputBuffer); overload;
function ALMimeBase64Decode(const InputBuffer; const InputByteCount: NativeInt; out OutputBuffer): NativeInt; overload;
function ALMimeBase64DecodePartial(const InputBuffer; const InputByteCount: NativeInt; out OutputBuffer;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): NativeInt; overload;
function ALMimeBase64DecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): NativeInt; overload;

procedure ALMimeBase64EncodeFile(const InputFileName, OutputFileName: TFileName);
procedure ALMimeBase64EncodeFileNoCRLF(const InputFileName, OutputFileName: TFileName);
procedure ALMimeBase64DecodeFile(const InputFileName, OutputFileName: TFileName);
procedure ALMimeBase64EncodeStream(const InputStream: TStream; const OutputStream: TStream);
procedure ALMimeBase64EncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);
procedure ALMimeBase64DecodeStream(const InputStream: TStream; const OutputStream: TStream);

const
  cALMimeBase64_ENCODED_LINE_BREAK = 76;
  cALMimeBase64_DECODED_LINE_BREAK = cALMimeBase64_ENCODED_LINE_BREAK div 4 * 3;
  cALMimeBase64_BUFFER_SIZE = cALMimeBase64_DECODED_LINE_BREAK * 3 * 4 * 4;

//From indy
procedure ALFillMimeContentTypeByExtList(AMIMEList : TALStrings);
procedure ALFillExtByMimeContentTypeList(AMIMEList : TALStrings);
Function  ALGetDefaultFileExtFromMimeContentType(aContentType: AnsiString): AnsiString;
Function  ALGetDefaultMIMEContentTypeFromExt(aExt: AnsiString): AnsiString;

Var vAlMimeContentTypeByExtList: TALStrings; {.htm=text/html}
    vAlExtbyMimeContentTypeList: TALStrings; {text/html=.htm}

implementation

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     Winapi.Windows,
     System.win.Registry,
     {$ELSE}
     Windows,
     Registry,
     {$IFEND}
     ALString;

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
  cALMimeBase64_ENCODE_TABLE: array [0..63] of Byte = (
    065, 066, 067, 068, 069, 070, 071, 072, //  00 - 07
    073, 074, 075, 076, 077, 078, 079, 080, //  08 - 15
    081, 082, 083, 084, 085, 086, 087, 088, //  16 - 23
    089, 090, 097, 098, 099, 100, 101, 102, //  24 - 31
    103, 104, 105, 106, 107, 108, 109, 110, //  32 - 39
    111, 112, 113, 114, 115, 116, 117, 118, //  40 - 47
    119, 120, 121, 122, 048, 049, 050, 051, //  48 - 55
    052, 053, 054, 055, 056, 057, 043, 047); // 56 - 63

  cALMimeBase64_PAD_CHAR = Byte('=');

  cALMimeBase64_DECODE_TABLE: array [Byte] of Byte = (
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

{*********************************************************************************************}
procedure ALMimeBase64Encode(const InputBuffer: TByteDynArray; const InputByteCount: NativeInt;
  out OutputBuffer: TByteDynArray);
begin
  ALMimeBase64Encode(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

{***************************************************************************************************}
procedure ALMimeBase64EncodeNoCRLF(const InputBuffer: TByteDynArray; const InputByteCount: NativeInt;
  out OutputBuffer: TByteDynArray);
begin
  ALMimeBase64EncodeNoCRLF(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

{******************************************************************************************************}
procedure ALMimeBase64EncodeFullLines(const InputBuffer: TByteDynArray; const InputByteCount: NativeInt;
  out OutputBuffer: TByteDynArray);
begin
  ALMimeBase64EncodeFullLines(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

{********************************************************************************************}
function ALMimeBase64Decode(const InputBuffer: TByteDynArray; const InputByteCount: NativeInt;
  out OutputBuffer: TByteDynArray): NativeInt;
begin
  Result := ALMimeBase64Decode(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

{***************************************************************************************************}
function ALMimeBase64DecodePartial(const InputBuffer: TByteDynArray; const InputByteCount: NativeInt;
  out OutputBuffer: TByteDynArray; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): NativeInt;
begin
  Result := ALMimeBase64DecodePartial(InputBuffer, 0, InputByteCount, OutputBuffer, 0, ByteBuffer, ByteBufferSpace);
end;

{************************************************************************************************}
function ALMimeBase64DecodePartialEnd(out OutputBuffer: TByteDynArray; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): NativeInt;
begin
  Result := ALMimeBase64DecodePartialEnd(OutputBuffer, 0, ByteBuffer, ByteBufferSpace);
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
function ALMimeBase64EncodeString(const S: AnsiString): AnsiString;
var
  L: NativeInt;
begin
  if S <> '' then
  begin
    L := Length(S);
    SetLength(Result, ALMimeBase64EncodedSize(L));
    ALMimeBase64Encode(PAnsiChar(S)^, L, PAnsiChar(Result)^);
  end
  else
    Result := '';
end;

{***********************************************************************}
function ALMimeBase64EncodeStringNoCRLF(const S: AnsiString): AnsiString;
var
  L: NativeInt;
begin
  if S <> '' then
  begin
    L := Length(S);
    SetLength(Result, ALMimeBase64EncodedSizeNoCRLF(L));
    ALMimeBase64EncodeNoCRLF(PAnsiChar(S)^, L, PAnsiChar(Result)^);
  end
  else
    Result := '';
end;

{*****************************************************************}
function ALMimeBase64DecodeString(const S: AnsiString): AnsiString;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  L: NativeInt;
  P, R: PAnsiChar;
begin
  if S <> '' then
  begin
    L := Length(S);
    SetLength(Result, ALMimeBase64DecodedSize(L));
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    P := PAnsiChar(S);
    R := PAnsiChar(Result);
    L := ALMimeBase64DecodePartial(P^, L, R^, ByteBuffer, ByteBufferSpace);
    Inc(R, L);
    Inc(L, ALMimeBase64DecodePartialEnd(R^, ByteBuffer, ByteBufferSpace));
    SetLength(Result, L);
  end
  else
    Result := '';
end;

{************************************************************************************************************}
procedure DecodeHttpBasicAuthentication(const BasicCredentials: AnsiString; out UserId, PassWord: AnsiString);
const
  LBasic = 6; { Length ('Basic ') }
var
  DecodedPtr, P: PAnsiChar;
  I, L: NativeInt;
begin
  UserId := '';
  PassWord := '';

  P := PAnsiChar(BasicCredentials);
  if P = nil then
    Exit;

  L := Length(BasicCredentials);
  if L <= LBasic then
    Exit;

  Dec(L, LBasic);
  Inc(P, LBasic);

  GetMem(DecodedPtr, ALMimeBase64DecodedSize(L));
  L := ALMimeBase64Decode(P^, L, DecodedPtr^);

  { Look for colon (':'). }
  I := 0;
  P := DecodedPtr;
  while (L > 0) and (P[I] <> ':') do
  begin
    Inc(I);
    Dec(L);
  end;

  { Store UserId and Password. }
  SetString(UserId, DecodedPtr, I);
  if L > 1 then
    SetString(PassWord, DecodedPtr + I + 1, L - 1)
  else
    PassWord := '';

  FreeMem(DecodedPtr);
end;

{*****************}
// Helper functions
function ALMimeBase64EncodedSize(const InputSize: NativeInt): NativeInt;
begin
  if InputSize > 0 then
    Result := (InputSize + 2) div 3 * 4 + (InputSize - 1) div cALMimeBase64_DECODED_LINE_BREAK * 2
  else
    Result := InputSize;
end;

{****************************************************************************}
function ALMimeBase64EncodedSizeNoCRLF(const InputSize: NativeInt): NativeInt;
begin
  Result := (InputSize + 2) div 3 * 4;
end;

{**********************************************************************}
function ALMimeBase64DecodedSize(const InputSize: NativeInt): NativeInt;
begin
  Result := (InputSize + 3) div 4 * 3;
end;

{*******************************}
// Primary functions & procedures
procedure ALMimeBase64Encode(const InputBuffer: TByteDynArray; InputOffset: NativeInt;
  const InputByteCount: NativeInt;
  out OutputBuffer: TByteDynArray; OutputOffset: NativeInt);
var
  IDelta, ODelta: NativeInt;
begin
  ALMimeBase64EncodeFullLines(InputBuffer, InputOffset, InputByteCount, OutputBuffer, OutputOffset);
  IDelta := InputByteCount div cALMimeBase64_DECODED_LINE_BREAK; // Number of lines processed so far.
  ODelta := IDelta * (cALMimeBase64_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * cALMimeBase64_DECODED_LINE_BREAK;
  ALMimeBase64EncodeNoCRLF(InputBuffer, InputOffset + IDelta, InputByteCount - IDelta, OutputBuffer, OutputOffset + ODelta);
end;

{*******************************}
// Primary functions & procedures
procedure ALMimeBase64Encode(const InputBuffer; const InputByteCount: NativeInt; out OutputBuffer);
var
  IDelta, ODelta: NativeInt;
  I, O: PByte;
begin
  ALMimeBase64EncodeFullLines(InputBuffer, InputByteCount, OutputBuffer);
  IDelta := InputByteCount div cALMimeBase64_DECODED_LINE_BREAK; // Number of lines processed so far.
  ODelta := IDelta * (cALMimeBase64_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * cALMimeBase64_DECODED_LINE_BREAK;
  I := @InputBuffer;
  Inc(I, IDelta);
  O := @OutputBuffer;
  Inc(O, ODelta);
  ALMimeBase64EncodeNoCRLF(I^, InputByteCount - IDelta, O^);
end;

{*********************************************************************************************}
procedure ALMimeBase64EncodeFullLines(const InputBuffer: TByteDynArray; InputOffset: NativeInt;
  const InputByteCount: NativeInt; out OutputBuffer: TByteDynArray; OutputOffset: NativeInt);
var
  B, InnerLimit, OuterLimit: NativeInt;
  InIndex: NativeInt;
  OutIndex: NativeInt;
begin
  { Do we have enough input to encode a full line? }
  if InputByteCount < cALMimeBase64_DECODED_LINE_BREAK then
    Exit;

  InIndex := InputOffset;
  OutIndex := OutputOffset;

  InnerLimit := InIndex;
  Inc(InnerLimit, cALMimeBase64_DECODED_LINE_BREAK);

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
      OutputBuffer[OutIndex + 3] := cALMimeBase64_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutputBuffer[OutIndex + 2] := cALMimeBase64_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutputBuffer[OutIndex + 1] := cALMimeBase64_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutputBuffer[OutIndex + 0] := cALMimeBase64_ENCODE_TABLE[B];
      Inc(OutIndex, 3);
    until InIndex >= InnerLimit;

    { Write line break (CRLF). }
    OutputBuffer[OutIndex + 0] := 13;
    OutputBuffer[OutIndex + 1] := 10;
    Inc(OutIndex, 2);

    Inc(InnerLimit, cALMimeBase64_DECODED_LINE_BREAK);
  until InnerLimit > OuterLimit;
end;

{**********************************************************************************************************}
procedure ALMimeBase64EncodeFullLines(const InputBuffer; const InputByteCount: NativeInt; out OutputBuffer);
var
  B: Cardinal;
  InnerLimit, OuterLimit: NativeUint;
  InPtr: PByte3;
  OutPtr: PByte4;
begin
  { Do we have enough input to encode a full line? }
  if InputByteCount < cALMimeBase64_DECODED_LINE_BREAK then
    Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  InnerLimit := NativeUint(InPtr);
  Inc(InnerLimit, cALMimeBase64_DECODED_LINE_BREAK);

  OuterLimit := NativeUint(InPtr);
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
      OutPtr^.B4 := cALMimeBase64_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.B3 := cALMimeBase64_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.B2 := cALMimeBase64_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.B1 := cALMimeBase64_ENCODE_TABLE[B];
      Inc(OutPtr);
    until NativeUint(InPtr) >= InnerLimit;

    { Write line break (CRLF). }
    OutPtr^.B1 := 13;
    OutPtr^.B2 := 10;
    Inc(NativeUint(OutPtr), 2);

    Inc(InnerLimit, cALMimeBase64_DECODED_LINE_BREAK);
  until InnerLimit > OuterLimit;
end;

{******************************************************************************************}
procedure ALMimeBase64EncodeNoCRLF(const InputBuffer: TByteDynArray; InputOffset: NativeInt;
  const InputByteCount: NativeInt; out OutputBuffer: TByteDynArray; OutputOffset: NativeInt);
var
  B, InnerLimit, OuterLimit: NativeInt;
  InIndex: NativeInt;
  OutIndex: NativeInt;
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
    OutputBuffer[OutIndex + 3] := cALMimeBase64_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutputBuffer[OutIndex + 2] := cALMimeBase64_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutputBuffer[OutIndex + 1] := cALMimeBase64_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutputBuffer[OutIndex + 0] := cALMimeBase64_ENCODE_TABLE[B];
    Inc(OutIndex, 3);
  end;

  { End of data & padding. }
  case InputByteCount - OuterLimit of
    1:
      begin
        B := InputBuffer[InIndex + 0];
        B := B shl 4;
        OutputBuffer[OutIndex + 1] := cALMimeBase64_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutputBuffer[OutIndex + 0] := cALMimeBase64_ENCODE_TABLE[B];
        OutputBuffer[OutIndex + 2] := cALMimeBase64_PAD_CHAR; { Pad remaining 2 bytes. }
        OutputBuffer[OutIndex + 3] := cALMimeBase64_PAD_CHAR;
      end;
    2:
      begin
        B := InputBuffer[InIndex + 0];
        B := B shl 8;
        B := B or InputBuffer[InIndex + 1];
        B := B shl 2;
        OutputBuffer[OutIndex + 2] := cALMimeBase64_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutputBuffer[OutIndex + 1] := cALMimeBase64_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutputBuffer[OutIndex + 0] := cALMimeBase64_ENCODE_TABLE[B];
        OutputBuffer[OutIndex + 3] := cALMimeBase64_PAD_CHAR; { Pad remaining byte. }
      end;
  end;
end;

{*******************************************************************************************************}
procedure ALMimeBase64EncodeNoCRLF(const InputBuffer; const InputByteCount: NativeInt; out OutputBuffer);
var
  B: Cardinal;
  InnerLimit, OuterLimit: NativeInt;
  InPtr: PByte3;
  OutPtr: PByte4;
begin
  if InputByteCount = 0 then
    Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  OuterLimit := InputByteCount div 3 * 3;

  InnerLimit := NativeUint(InPtr);
  Inc(InnerLimit, OuterLimit);

  { Last line loop. }
  while NativeUint(InPtr) < NativeUint(InnerLimit) do
  begin
    { Read 3 bytes from InputBuffer. }
    B := InPtr^.B1;
    B := B shl 8;
    B := B or InPtr^.B2;
    B := B shl 8;
    B := B or InPtr^.B3;
    Inc(InPtr);
    { Write 4 bytes to OutputBuffer (in reverse order). }
    OutPtr^.B4 := cALMimeBase64_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.B3 := cALMimeBase64_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.B2 := cALMimeBase64_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.B1 := cALMimeBase64_ENCODE_TABLE[B];
    Inc(OutPtr);
  end;

  { End of data & padding. }
  case InputByteCount - OuterLimit of
    1:
      begin
        B := InPtr^.B1;
        B := B shl 4;
        OutPtr.B2 := cALMimeBase64_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B1 := cALMimeBase64_ENCODE_TABLE[B];
        OutPtr.B3 := cALMimeBase64_PAD_CHAR; { Pad remaining 2 bytes. }
        OutPtr.B4 := cALMimeBase64_PAD_CHAR;
      end;
    2:
      begin
        B := InPtr^.B1;
        B := B shl 8;
        B := B or InPtr^.B2;
        B := B shl 2;
        OutPtr.B3 := cALMimeBase64_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B2 := cALMimeBase64_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B1 := cALMimeBase64_ENCODE_TABLE[B];
        OutPtr.B4 := cALMimeBase64_PAD_CHAR; { Pad remaining byte. }
      end;
  end;
end;

{**************}
// Decoding Core
function ALMimeBase64Decode(const InputBuffer: TByteDynArray; InputOffset: NativeInt;
  const InputByteCount: NativeInt; out OutputBuffer: TByteDynArray; OutputOffset: NativeInt): NativeInt;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  Result := ALMimeBase64DecodePartial(InputBuffer, InputOffset, InputByteCount, OutputBuffer, OutputOffset, ByteBuffer, ByteBufferSpace);
  Inc(Result, ALMimeBase64DecodePartialEnd(OutputBuffer, OutputOffset + Result, ByteBuffer, ByteBufferSpace));
end;

{**************}
// Decoding Core
function ALMimeBase64Decode(const InputBuffer; const InputByteCount: NativeInt; out OutputBuffer): NativeInt;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  O: PByte;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  Result := ALMimeBase64DecodePartial(InputBuffer, InputByteCount, OutputBuffer, ByteBuffer, ByteBufferSpace);
  O := @OutputBuffer;
  Inc(O, Result);
  Inc(Result, ALMimeBase64DecodePartialEnd(O^, ByteBuffer, ByteBufferSpace));
end;

{******************************************************************************************}
function ALMimeBase64DecodePartial(const InputBuffer: TByteDynArray; InputOffset: NativeInt;
  const InputByteCount: NativeInt; out OutputBuffer: TByteDynArray; OutputOffset: NativeInt;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): NativeInt;
var
  LByteBuffer, LByteBufferSpace, C: Cardinal;
  InIndex, OuterLimit: NativeInt;
  OutIndex: NativeInt;
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
        C := cALMimeBase64_DECODE_TABLE[InputBuffer[InIndex]];
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

{******************************************************************************************************}
function ALMimeBase64DecodePartial(const InputBuffer; const InputByteCount: NativeInt; out OutputBuffer;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): NativeInt;
var
  LByteBuffer, LByteBufferSpace, C: Cardinal;
  InPtr, OuterLimit: PByte;
  OutPtr: PByte3;
begin
  if InputByteCount > 0 then
  begin
    InPtr := @InputBuffer;
    OuterLimit := Pointer(NativeUint(InPtr) + NativeUint(InputByteCount));
    OutPtr := @OutputBuffer;
    LByteBuffer := ByteBuffer;
    LByteBufferSpace := ByteBufferSpace;
    while InPtr <> OuterLimit do
    begin
      { Read from InputBuffer. }
      C := cALMimeBase64_DECODE_TABLE[InPtr^];
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
    Result := NativeInt(NativeUint(OutPtr) - NativeUint(@OutputBuffer));
  end
  else
    Result := 0;
end;

{*********************************************************************************************}
function ALMimeBase64DecodePartialEnd(out OutputBuffer: TByteDynArray; OutputOffset: NativeInt;
  const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): NativeInt;
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

{*********************************************************************************}
function ALMimeBase64DecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): NativeInt;
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
procedure ALMimeBase64EncodeFile(const InputFileName, OutputFileName: TFileName);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      ALMimeBase64EncodeStream(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

{*************************************************************************************}
procedure ALMimeBase64EncodeFileNoCRLF(const InputFileName, OutputFileName: TFileName);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      ALMimeBase64EncodeStreamNoCRLF(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

{*******************************************************************************}
procedure ALMimeBase64DecodeFile(const InputFileName, OutputFileName: TFileName);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      ALMimeBase64DecodeStream(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

{***************************}
// Stream Encoding & Decoding
procedure ALMimeBase64EncodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array [0..cALMimeBase64_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array [0..(cALMimeBase64_BUFFER_SIZE + 2) div 3 * 4 + cALMimeBase64_BUFFER_SIZE div cALMimeBase64_DECODED_LINE_BREAK * 2 - 1] of Byte;
  BytesRead: NativeInt;
  IDelta, ODelta: NativeInt;
  I, O: PByte;
begin
  InputBuffer[0] := 0;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));

  while BytesRead = Length(InputBuffer) do
  begin
    ALMimeBase64EncodeFullLines(InputBuffer, Length(InputBuffer), OutputBuffer);
    OutputStream.Write(OutputBuffer, Length(OutputBuffer));
    BytesRead := InputStream.Read(InputBuffer, Length(InputBuffer));
  end;

  ALMimeBase64EncodeFullLines(InputBuffer, BytesRead, OutputBuffer);

  IDelta := BytesRead div cALMimeBase64_DECODED_LINE_BREAK; // Number of lines processed.
  ODelta := IDelta * (cALMimeBase64_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * cALMimeBase64_DECODED_LINE_BREAK;

  I := @InputBuffer;
  Inc(I, IDelta);
  O := @OutputBuffer;
  Inc(O, ODelta);

  ALMimeBase64EncodeNoCRLF(I^, BytesRead - IDelta, O^);

  OutputStream.Write(OutputBuffer, ALMimeBase64EncodedSize(BytesRead));
end;

{************************************************************************************************}
procedure ALMimeBase64EncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array [0..cALMimeBase64_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array [0..((cALMimeBase64_BUFFER_SIZE + 2) div 3) * 4 - 1] of Byte;
  BytesRead: NativeInt;
begin
  InputBuffer[0] := 0;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));

  while BytesRead = Length(InputBuffer) do
  begin
    ALMimeBase64EncodeNoCRLF(InputBuffer, Length(InputBuffer), OutputBuffer);
    OutputStream.Write(OutputBuffer, Length(OutputBuffer));
    BytesRead := InputStream.Read(InputBuffer, Length(InputBuffer));
  end;

  ALMimeBase64EncodeNoCRLF(InputBuffer, BytesRead, OutputBuffer);
  OutputStream.Write(OutputBuffer, ALMimeBase64EncodedSizeNoCRLF(BytesRead));
end;

{******************************************************************************************}
procedure ALMimeBase64DecodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  InputBuffer: array [0..cALMimeBase64_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array [0..(cALMimeBase64_BUFFER_SIZE + 3) div 4 * 3 - 1] of Byte;
  BytesRead: NativeInt;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  InputBuffer[0] := 0;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));

  while BytesRead > 0 do
  begin
    OutputStream.Write(OutputBuffer, ALMimeBase64DecodePartial(InputBuffer, BytesRead, OutputBuffer, ByteBuffer, ByteBufferSpace));
    BytesRead := InputStream.Read(InputBuffer, Length(InputBuffer));
  end;
  OutputStream.Write(OutputBuffer, ALMimeBase64DecodePartialEnd(OutputBuffer, ByteBuffer, ByteBufferSpace));
end;

{***************************************************************}
procedure ALFillMimeContentTypeByExtList(AMIMEList : TALStrings);
var reg: TRegistry;
    KeyList: TStrings;
    i: Integer;
    aExt, aContentType: String;
begin
  AMIMEList.Clear;

  Reg := TRegistry.Create(KEY_READ);
  try

    KeyList := TStringList.create;
    try

      Reg.RootKey := HKEY_CLASSES_ROOT;
      if Reg.OpenKeyReadOnly('\') then begin
        Reg.GetKeyNames(KeyList);
        Reg.CloseKey;
        for i := 0 to KeyList.Count - 1 do begin
          aExt := KeyList[i];
          if (length(aExt) > 1) and (aExt[1] = '.') then begin
            if reg.OpenKeyReadOnly('\' + aExt) then begin
              aExt := Trim(aExt);
              If (length(aExt) > 1) then begin
                aContentType := Trim(Reg.ReadString('Content Type'));
                if aContentType <> '' then AMIMEList.Values[alLowerCase(ansiString(aExt))] := AlLowerCase(ansistring(aContentType));
              end;
              Reg.CloseKey;
            end;
          end;
        end;
      end;

    finally
      KeyList.Free;
    end;

  finally
    reg.free;
  end;
end;

{***************************************************************}
procedure ALFillExtByMimeContentTypeList(AMIMEList : TALStrings);
var reg: TRegistry;
    KeyList: TStrings;
    i: Integer;
    aExt, aContentType: String;
begin
  AMIMEList.Clear;

  Reg := TRegistry.Create(KEY_READ);
  try

    KeyList := TStringList.create;
    try

      Reg.RootKey := HKEY_CLASSES_ROOT;
      if Reg.OpenKeyreadOnly('\MIME\Database\Content Type') then begin
        Reg.GetKeyNames(KeyList);
        Reg.CloseKey;
        for i := 0 to KeyList.Count - 1 do begin
          aContentType := KeyList[i];
          If aContentType <> '' then begin
            if Reg.OpenKeyreadOnly('\MIME\Database\Content Type\' + aContentType) then begin
              aContenttype := Trim(aContentType);
              if aContentType <> '' then begin
                aExt := reg.ReadString('Extension');
                if aExt <> '' then begin
                  If (aExt[1] <> '.') then aExt := '.' + aExt;
                  AMIMEList.Values[alLowerCase(ansiString(aContentType))] := AlLowerCase(ansistring(aExt))
                end;
              end;
              Reg.CloseKey;
            end;
          end;
        end;
      end;

    finally
      KeyList.Free;
    end;

  finally
    reg.free;
  end;
end;

{****************************************************************************}
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

{****************************************************************}
Function ALGetDefaultMIMEContentTypeFromExt(aExt: AnsiString): AnsiString;
var Index : Integer;
    LExt: AnsiString;
begin
  LExt := AlLowerCase(ALTrim(aExt));
  If (LExt = '') or (LExt[1] <> '.') then LExt := '.' + LExt;
  Index := vAlMimeContentTypeByExtList.IndexOfName(LExt);
  if Index <> -1 then Result := vAlMimeContentTypeByExtList.ValueFromIndex[Index]
  else Result := 'application/octet-stream';
end;

Initialization
 vAlMimeContentTypeByExtList := TALStringList.Create;
 vAlExtbyMimeContentTypeList := TALStringList.Create;
 ALFillMimeContentTypeByExtList(vAlMimeContentTypeByExtList);
 ALFillExtByMimeContentTypeList(vAlExtbyMimeContentTypeList);
 TALStringList(vAlMimeContentTypeByExtList).Duplicates := dupIgnore;
 TALStringList(vAlMimeContentTypeByExtList).Sorted := true;
 TALStringList(vAlExtbyMimeContentTypeList).Duplicates := dupIgnore;
 TALStringList(vAlExtbyMimeContentTypeList).Sorted := true;
 
finalization
 vAlMimeContentTypeByExtList.Free;
 vAlExtbyMimeContentTypeList.Free;

end.
