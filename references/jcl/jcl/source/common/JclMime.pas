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
{ The Original Code is JclMime.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Ralf Junker.                                       }
{ Portions created by Ralf Junker are Copyright (C) Ralf Junker. All rights reserved.              }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Ralf Junker                                                                                    }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   edbored                                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Lightning fast Mime (Base64) Encoding and Decoding routines. Coded by Ralf Junker                }
{ (ralfjunker att gmx dott de).                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{ Migration Guide from JCL 1.90 and older:                                                         }
{                                                                                                  }
{ These new functions now support line breaks (CRLF) as required by RFC 2045.                      }
{ Inserting line breaks is the default behaviour in RFC 2045 therefor the encoding functions now   }
{ encode with line breaks.                                                                         }
{                                                                                                  }
{ This may require changes to your code:                                                           }
{ Encoding without inserting line breaks is possible using the corresponding NoCRLF procedures:    }
{                                                                                                  }
{ MimeEncode => MimeEncodeNoCRLF                                                                   }
{ MimeEncodeString => MimeEncodeStringNoCRLF                                                       }
{ MimeEncodeStream => MimeEncodeStreamNoCRLF                                                       }
{ MimeEncodedSize => MimeEncodedSizeNoCRLF                                                         }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclMime;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils, System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils, Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase;

function MimeEncodeString(const S: AnsiString): AnsiString;
function MimeEncodeStringNoCRLF(const S: AnsiString): AnsiString;
function MimeDecodeString(const S: AnsiString): AnsiString;
function MimeEncodedSize(const InputSize: SizeInt): SizeInt;
function MimeEncodedSizeNoCRLF(const InputSize: SizeInt): SizeInt;
function MimeDecodedSize(const InputSize: SizeInt): SizeInt;
procedure DecodeHttpBasicAuthentication(const BasicCredentials: AnsiString;
  out UserId, PassWord: AnsiString);

procedure MimeEncode(const InputBuffer: TDynByteArray; InputOffset: SizeInt;
  const InputByteCount: SizeInt; out OutputBuffer: TDynByteArray; OutputOffset: SizeInt = 0); overload;
procedure MimeEncodeNoCRLF(const InputBuffer: TDynByteArray; InputOffset: SizeInt;
  const InputByteCount: SizeInt; out OutputBuffer: TDynByteArray; OutputOffset: SizeInt = 0); overload;
procedure MimeEncodeFullLines(const InputBuffer: TDynByteArray; InputOffset: SizeInt;
  const InputByteCount: SizeInt; out OutputBuffer: TDynByteArray; OutputOffset: SizeInt = 0); overload;
function MimeDecode(const InputBuffer: TDynByteArray; InputOffset: SizeInt;
  const InputByteCount: SizeInt; out OutputBuffer: TDynByteArray; OutputOffset: SizeInt = 0): SizeInt; overload;
function MimeDecodePartial(const InputBuffer: TDynByteArray; InputOffset: SizeInt;
  const InputByteCount: SizeInt; out OutputBuffer: TDynByteArray; OutputOffset: SizeInt;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): SizeInt; overload;
function MimeDecodePartialEnd(out OutputBuffer: TDynByteArray; OutputOffset: SizeInt;
  const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): SizeInt; overload;

procedure MimeEncode(const InputBuffer: TDynByteArray; const InputByteCount: SizeInt;
  out OutputBuffer: TDynByteArray); overload;
procedure MimeEncodeNoCRLF(const InputBuffer: TDynByteArray; const InputByteCount: SizeInt;
  out OutputBuffer: TDynByteArray); overload;
procedure MimeEncodeFullLines(const InputBuffer: TDynByteArray; const InputByteCount: SizeInt;
  out OutputBuffer: TDynByteArray); overload;
function MimeDecode(const InputBuffer: TDynByteArray; const InputByteCount: SizeInt;
  out OutputBuffer: TDynByteArray): SizeInt; overload;
function MimeDecodePartial(const InputBuffer: TDynByteArray; const InputByteCount: SizeInt;
  out OutputBuffer: TDynByteArray; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): SizeInt; overload;
function MimeDecodePartialEnd(out OutputBuffer: TDynByteArray; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): SizeInt; overload;

procedure MimeEncode(const InputBuffer; const InputByteCount: SizeInt; out OutputBuffer); overload;
procedure MimeEncodeNoCRLF(const InputBuffer; const InputByteCount: SizeInt; out OutputBuffer); overload;
procedure MimeEncodeFullLines(const InputBuffer; const InputByteCount: SizeInt; out OutputBuffer); overload;
function MimeDecode(const InputBuffer; const InputByteCount: SizeInt; out OutputBuffer): SizeInt; overload;
function MimeDecodePartial(const InputBuffer; const InputByteCount: SizeInt; out OutputBuffer;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): SizeInt; overload;
function MimeDecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): SizeInt; overload;

procedure MimeEncodeFile(const InputFileName, OutputFileName: TFileName);
procedure MimeEncodeFileNoCRLF(const InputFileName, OutputFileName: TFileName);
procedure MimeDecodeFile(const InputFileName, OutputFileName: TFileName);
procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
procedure MimeEncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);
procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);

const
  MIME_ENCODED_LINE_BREAK = 76;
  MIME_DECODED_LINE_BREAK = MIME_ENCODED_LINE_BREAK div 4 * 3;
  MIME_BUFFER_SIZE = MIME_DECODED_LINE_BREAK * 3 * 4 * 4;

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

// Caution: For MimeEncodeStream and all other kinds of multi-buffered
// Mime encodings (i.e. Files etc.), BufferSize must be set to a multiple of 3.
// Even though the implementation of the Mime decoding routines below
// do not require a particular buffer size, they work fastest with sizes of
// multiples of four. The chosen size is a multiple of 3 and of 4 as well.
// The following numbers are, in addition, also divisible by 1024:
// $2400, $3000, $3C00, $4800, $5400, $6000, $6C00.

const
  { The mime encoding table. Do not alter. }
  MIME_ENCODE_TABLE: array [0..63] of Byte = (
    065, 066, 067, 068, 069, 070, 071, 072, //  00 - 07
    073, 074, 075, 076, 077, 078, 079, 080, //  08 - 15
    081, 082, 083, 084, 085, 086, 087, 088, //  16 - 23
    089, 090, 097, 098, 099, 100, 101, 102, //  24 - 31
    103, 104, 105, 106, 107, 108, 109, 110, //  32 - 39
    111, 112, 113, 114, 115, 116, 117, 118, //  40 - 47
    119, 120, 121, 122, 048, 049, 050, 051, //  48 - 55
    052, 053, 054, 055, 056, 057, 043, 047); // 56 - 63

  MIME_PAD_CHAR = Byte('=');

  MIME_DECODE_TABLE: array [Byte] of Byte = (
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

procedure MimeEncode(const InputBuffer: TDynByteArray; const InputByteCount: SizeInt;
  out OutputBuffer: TDynByteArray);
begin
  MimeEncode(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

procedure MimeEncodeNoCRLF(const InputBuffer: TDynByteArray; const InputByteCount: SizeInt;
  out OutputBuffer: TDynByteArray);
begin
  MimeEncodeNoCRLF(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

procedure MimeEncodeFullLines(const InputBuffer: TDynByteArray; const InputByteCount: SizeInt;
  out OutputBuffer: TDynByteArray);
begin
  MimeEncodeFullLines(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

function MimeDecode(const InputBuffer: TDynByteArray; const InputByteCount: SizeInt;
  out OutputBuffer: TDynByteArray): SizeInt;
begin
  Result := MimeDecode(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

function MimeDecodePartial(const InputBuffer: TDynByteArray; const InputByteCount: SizeInt;
  out OutputBuffer: TDynByteArray; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): SizeInt;
begin
  Result := MimeDecodePartial(InputBuffer, 0, InputByteCount, OutputBuffer, 0, ByteBuffer, ByteBufferSpace);
end;

function MimeDecodePartialEnd(out OutputBuffer: TDynByteArray; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): SizeInt;
begin
  Result := MimeDecodePartialEnd(OutputBuffer, 0, ByteBuffer, ByteBufferSpace);
end;

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

// Wrapper functions & procedures
function MimeEncodeString(const S: AnsiString): AnsiString;
var
  L: SizeInt;
begin
  if S <> '' then
  begin
    L := Length(S);
    SetLength(Result, MimeEncodedSize(L));
    MimeEncode(PAnsiChar(S)^, L, PAnsiChar(Result)^);
  end
  else
    Result := '';
end;

function MimeEncodeStringNoCRLF(const S: AnsiString): AnsiString;
var
  L: SizeInt;
begin
  if S <> '' then
  begin
    L := Length(S);
    SetLength(Result, MimeEncodedSizeNoCRLF(L));
    MimeEncodeNoCRLF(PAnsiChar(S)^, L, PAnsiChar(Result)^);
  end
  else
    Result := '';
end;

function MimeDecodeString(const S: AnsiString): AnsiString;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  L: SizeInt;
  P, R: PAnsiChar;
begin
  if S <> '' then
  begin
    L := Length(S);
    SetLength(Result, MimeDecodedSize(L));
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    P := PAnsiChar(S);
    R := PAnsiChar(Result);
    L := MimeDecodePartial(P^, L, R^, ByteBuffer, ByteBufferSpace);
    Inc(R, L);
    Inc(L, MimeDecodePartialEnd(R^, ByteBuffer, ByteBufferSpace));
    SetLength(Result, L);
  end
  else
    Result := '';
end;

procedure DecodeHttpBasicAuthentication(const BasicCredentials: AnsiString; out UserId, PassWord: AnsiString);
const
  LBasic = 6; { Length ('Basic ') }
var
  DecodedPtr, P: PAnsiChar;
  I, L: SizeInt;
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

  GetMem(DecodedPtr, MimeDecodedSize(L));
  L := MimeDecode(P^, L, DecodedPtr^);

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

// Helper functions
function MimeEncodedSize(const InputSize: SizeInt): SizeInt;
begin
  if InputSize > 0 then
    Result := (InputSize + 2) div 3 * 4 + (InputSize - 1) div MIME_DECODED_LINE_BREAK * 2
  else
    Result := InputSize;
end;

function MimeEncodedSizeNoCRLF(const InputSize: SizeInt): SizeInt;
begin
  Result := (InputSize + 2) div 3 * 4;
end;

function MimeDecodedSize(const InputSize: SizeInt): SizeInt;
begin
  Result := (InputSize + 3) div 4 * 3;
end;


// Primary functions & procedures
procedure MimeEncode(const InputBuffer: TDynByteArray; InputOffset: SizeInt;
  const InputByteCount: SizeInt;
  out OutputBuffer: TDynByteArray; OutputOffset: SizeInt);
var
  IDelta, ODelta: SizeInt;
begin
  MimeEncodeFullLines(InputBuffer, InputOffset, InputByteCount, OutputBuffer, OutputOffset);
  IDelta := InputByteCount div MIME_DECODED_LINE_BREAK; // Number of lines processed so far.
  ODelta := IDelta * (MIME_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * MIME_DECODED_LINE_BREAK;
  MimeEncodeNoCRLF(InputBuffer, InputOffset + IDelta, InputByteCount - IDelta, OutputBuffer, OutputOffset + ODelta);
end;

// Primary functions & procedures
procedure MimeEncode(const InputBuffer; const InputByteCount: SizeInt; out OutputBuffer);
var
  IDelta, ODelta: SizeInt;
  I, O: PByte;
begin
  MimeEncodeFullLines(InputBuffer, InputByteCount, OutputBuffer);
  IDelta := InputByteCount div MIME_DECODED_LINE_BREAK; // Number of lines processed so far.
  ODelta := IDelta * (MIME_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * MIME_DECODED_LINE_BREAK;
  I := @InputBuffer;
  Inc(I, IDelta);
  O := @OutputBuffer;
  Inc(O, ODelta);
  MimeEncodeNoCRLF(I^, InputByteCount - IDelta, O^);
end;

procedure MimeEncodeFullLines(const InputBuffer: TDynByteArray; InputOffset: SizeInt;
  const InputByteCount: SizeInt; out OutputBuffer: TDynByteArray; OutputOffset: SizeInt);
var
  B: SizeInt;
  InnerLimit, OuterLimit: TJclAddr;
  InIndex: TJclAddr;
  OutIndex: TJclAddr;
begin
  { Do we have enough input to encode a full line? }
  if InputByteCount < MIME_DECODED_LINE_BREAK then
    Exit;

  InIndex := InputOffset;
  OutIndex := OutputOffset;

  InnerLimit := InIndex;
  Inc(InnerLimit, MIME_DECODED_LINE_BREAK);

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
      OutputBuffer[OutIndex + 3] := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutputBuffer[OutIndex + 2] := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutputBuffer[OutIndex + 1] := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutputBuffer[OutIndex + 0] := MIME_ENCODE_TABLE[B];
      Inc(OutIndex, 3);
    until InIndex >= InnerLimit;

    { Write line break (CRLF). }
    OutputBuffer[OutIndex + 0] := 13;
    OutputBuffer[OutIndex + 1] := 10;
    Inc(OutIndex, 2);

    Inc(InnerLimit, MIME_DECODED_LINE_BREAK);
  until InnerLimit > OuterLimit;
end;

procedure MimeEncodeFullLines(const InputBuffer; const InputByteCount: SizeInt; out OutputBuffer);
var
  B: Cardinal;
  InnerLimit, OuterLimit: TJclAddr;
  InPtr: PByte3;
  OutPtr: PByte4;
begin
  { Do we have enough input to encode a full line? }
  if InputByteCount < MIME_DECODED_LINE_BREAK then
    Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  InnerLimit := TJclAddr(InPtr);
  Inc(InnerLimit, MIME_DECODED_LINE_BREAK);

  OuterLimit := TJclAddr(InPtr);
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
      OutPtr^.B4 := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.B3 := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.B2 := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.B1 := MIME_ENCODE_TABLE[B];
      Inc(OutPtr);
    until TJclAddr(InPtr) >= InnerLimit;

    { Write line break (CRLF). }
    OutPtr^.B1 := 13;
    OutPtr^.B2 := 10;
    Inc(TJclAddr(OutPtr), 2);

    Inc(InnerLimit, MIME_DECODED_LINE_BREAK);
  until InnerLimit > OuterLimit;
end;

procedure MimeEncodeNoCRLF(const InputBuffer: TDynByteArray; InputOffset: SizeInt;
  const InputByteCount: SizeInt; out OutputBuffer: TDynByteArray; OutputOffset: SizeInt);
var
  B: SizeInt;
  InnerLimit, OuterLimit: TJclAddr;
  InIndex: TJclAddr;
  OutIndex: TJclAddr;
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
    OutputBuffer[OutIndex + 3] := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutputBuffer[OutIndex + 2] := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutputBuffer[OutIndex + 1] := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutputBuffer[OutIndex + 0] := MIME_ENCODE_TABLE[B];
    Inc(OutIndex, 3);
  end;

  { End of data & padding. }
  case TJclAddr(InputByteCount) - OuterLimit of
    1:
      begin
        B := InputBuffer[InIndex + 0];
        B := B shl 4;
        OutputBuffer[OutIndex + 1] := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutputBuffer[OutIndex + 0] := MIME_ENCODE_TABLE[B];
        OutputBuffer[OutIndex + 2] := MIME_PAD_CHAR; { Pad remaining 2 bytes. }
        OutputBuffer[OutIndex + 3] := MIME_PAD_CHAR;
      end;
    2:
      begin
        B := InputBuffer[InIndex + 0];
        B := B shl 8;
        B := B or InputBuffer[InIndex + 1];
        B := B shl 2;
        OutputBuffer[OutIndex + 2] := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutputBuffer[OutIndex + 1] := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutputBuffer[OutIndex + 0] := MIME_ENCODE_TABLE[B];
        OutputBuffer[OutIndex + 3] := MIME_PAD_CHAR; { Pad remaining byte. }
      end;
  end;
end;

procedure MimeEncodeNoCRLF(const InputBuffer; const InputByteCount: SizeInt; out OutputBuffer);
var
  B: Cardinal;
  InnerLimit, OuterLimit: TJclAddr;
  InPtr: PByte3;
  OutPtr: PByte4;
begin
  if InputByteCount = 0 then
    Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  OuterLimit := InputByteCount div 3 * 3;

  InnerLimit := TJclAddr(InPtr);
  Inc(InnerLimit, OuterLimit);

  { Last line loop. }
  while TJclAddr(InPtr) < TJclAddr(InnerLimit) do
  begin
    { Read 3 bytes from InputBuffer. }
    B := InPtr^.B1;
    B := B shl 8;
    B := B or InPtr^.B2;
    B := B shl 8;
    B := B or InPtr^.B3;
    Inc(InPtr);
    { Write 4 bytes to OutputBuffer (in reverse order). }
    OutPtr^.B4 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.B3 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.B2 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.B1 := MIME_ENCODE_TABLE[B];
    Inc(OutPtr);
  end;

  { End of data & padding. }
  case TJclAddr(InputByteCount) - OuterLimit of
    1:
      begin
        B := InPtr^.B1;
        B := B shl 4;
        OutPtr.B2 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B1 := MIME_ENCODE_TABLE[B];
        OutPtr.B3 := MIME_PAD_CHAR; { Pad remaining 2 bytes. }
        OutPtr.B4 := MIME_PAD_CHAR;
      end;
    2:
      begin
        B := InPtr^.B1;
        B := B shl 8;
        B := B or InPtr^.B2;
        B := B shl 2;
        OutPtr.B3 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B2 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B1 := MIME_ENCODE_TABLE[B];
        OutPtr.B4 := MIME_PAD_CHAR; { Pad remaining byte. }
      end;
  end;
end;

// Decoding Core
function MimeDecode(const InputBuffer: TDynByteArray; InputOffset: SizeInt;
  const InputByteCount: SizeInt; out OutputBuffer: TDynByteArray; OutputOffset: SizeInt): SizeInt;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  Result := MimeDecodePartial(InputBuffer, InputOffset, InputByteCount, OutputBuffer, OutputOffset, ByteBuffer, ByteBufferSpace);
  Inc(Result, MimeDecodePartialEnd(OutputBuffer, OutputOffset + Result, ByteBuffer, ByteBufferSpace));
end;

// Decoding Core
function MimeDecode(const InputBuffer; const InputByteCount: SizeInt; out OutputBuffer): SizeInt;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  O: PByte;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  Result := MimeDecodePartial(InputBuffer, InputByteCount, OutputBuffer, ByteBuffer, ByteBufferSpace);
  O := @OutputBuffer;
  Inc(O, Result);
  Inc(Result, MimeDecodePartialEnd(O^, ByteBuffer, ByteBufferSpace));
end;

function MimeDecodePartial(const InputBuffer: TDynByteArray; InputOffset: SizeInt;
  const InputByteCount: SizeInt; out OutputBuffer: TDynByteArray; OutputOffset: SizeInt;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): SizeInt;
var
  LByteBuffer, LByteBufferSpace, C: Cardinal;
  InIndex, OuterLimit: TJclAddr;
  OutIndex: TJclAddr;
begin
  if InputByteCount > 0 then
    begin
      InIndex := InputOffset;
      OuterLimit := InIndex + TJclAddr(InputByteCount);
      OutIndex := OutputOffset;
      LByteBuffer := ByteBuffer;
      LByteBufferSpace := ByteBufferSpace;
      while InIndex < OuterLimit do
      begin
        { Read from InputBuffer. }
        C := MIME_DECODE_TABLE[InputBuffer[InIndex]];
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
      Result := OutIndex - TJclAddr(OutputOffset);
    end
  else
    Result := 0;
end;

function MimeDecodePartial(const InputBuffer; const InputByteCount: SizeInt; out OutputBuffer;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): SizeInt;
var
  LByteBuffer, LByteBufferSpace, C: Cardinal;
  InPtr, OuterLimit: PByte;
  OutPtr: PByte3;
begin
  if InputByteCount > 0 then
  begin
    InPtr := @InputBuffer;
    OuterLimit := Pointer(TJclAddr(InPtr) + TJclAddr(InputByteCount));
    OutPtr := @OutputBuffer;
    LByteBuffer := ByteBuffer;
    LByteBufferSpace := ByteBufferSpace;
    while InPtr <> OuterLimit do
    begin
      { Read from InputBuffer. }
      C := MIME_DECODE_TABLE[InPtr^];
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
    Result := SizeInt(TJclAddr(OutPtr) - TJclAddr(@OutputBuffer));
  end
  else
    Result := 0;
end;

function MimeDecodePartialEnd(out OutputBuffer: TDynByteArray; OutputOffset: SizeInt;
  const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): SizeInt;
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

function MimeDecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): SizeInt;
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

// File Encoding & Decoding
procedure MimeEncodeFile(const InputFileName, OutputFileName: TFileName);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      MimeEncodeStream(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

procedure MimeEncodeFileNoCRLF(const InputFileName, OutputFileName: TFileName);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      MimeEncodeStreamNoCRLF(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

procedure MimeDecodeFile(const InputFileName, OutputFileName: TFileName);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      MimeDecodeStream(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

// Stream Encoding & Decoding
procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array [0..MIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array [0..(MIME_BUFFER_SIZE + 2) div 3 * 4 + MIME_BUFFER_SIZE div MIME_DECODED_LINE_BREAK * 2 - 1] of Byte;
  BytesRead: SizeInt;
  IDelta, ODelta: SizeInt;
  I, O: PByte;
begin
  InputBuffer[0] := 0;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));

  while BytesRead = Length(InputBuffer) do
  begin
    MimeEncodeFullLines(InputBuffer, Length(InputBuffer), OutputBuffer);
    OutputStream.Write(OutputBuffer, Length(OutputBuffer));
    BytesRead := InputStream.Read(InputBuffer, Length(InputBuffer));
  end;

  MimeEncodeFullLines(InputBuffer, BytesRead, OutputBuffer);

  IDelta := BytesRead div MIME_DECODED_LINE_BREAK; // Number of lines processed.
  ODelta := IDelta * (MIME_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * MIME_DECODED_LINE_BREAK;

  I := @InputBuffer;
  Inc(I, IDelta);
  O := @OutputBuffer;
  Inc(O, ODelta);

  MimeEncodeNoCRLF(I^, BytesRead - IDelta, O^);

  OutputStream.Write(OutputBuffer, MimeEncodedSize(BytesRead));
end;

procedure MimeEncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array [0..MIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array [0..((MIME_BUFFER_SIZE + 2) div 3) * 4 - 1] of Byte;
  BytesRead: SizeInt;
begin
  InputBuffer[0] := 0;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));

  while BytesRead = Length(InputBuffer) do
  begin
    MimeEncodeNoCRLF(InputBuffer, Length(InputBuffer), OutputBuffer);
    OutputStream.Write(OutputBuffer, Length(OutputBuffer));
    BytesRead := InputStream.Read(InputBuffer, Length(InputBuffer));
  end;

  MimeEncodeNoCRLF(InputBuffer, BytesRead, OutputBuffer);
  OutputStream.Write(OutputBuffer, MimeEncodedSizeNoCRLF(BytesRead));
end;

procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  InputBuffer: array [0..MIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array [0..(MIME_BUFFER_SIZE + 3) div 4 * 3 - 1] of Byte;
  BytesRead: SizeInt;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  InputBuffer[0] := 0;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));

  while BytesRead > 0 do
  begin
    OutputStream.Write(OutputBuffer, MimeDecodePartial(InputBuffer, BytesRead, OutputBuffer, ByteBuffer, ByteBufferSpace));
    BytesRead := InputStream.Read(InputBuffer, Length(InputBuffer));
  end;
  OutputStream.Write(OutputBuffer, MimeDecodePartialEnd(OutputBuffer, ByteBuffer, ByteBufferSpace));
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
