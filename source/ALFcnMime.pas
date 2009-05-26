{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Jedi Project - JCL
							Stephane Vander Clock (svanderclock@arkadia.com)              
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      Alcinoe Mime Functions
Version:      3.50

Description:  Function mime encode and decode from JCL and function
              to get default Mime content type from file extension

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

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

History :

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALFcnMime;

interface

uses Windows,
     Classes,
     sysutils;

{from JCL}
function  ALMimeBase64EncodeString(const S: AnsiString): AnsiString;
function  ALMimeBase64EncodeStringNoCRLF(const S: AnsiString): AnsiString;
function  ALMimeBase64DecodeString(const S: AnsiString): AnsiString;
function  ALMimeBase64EncodedSize(const InputSize: Cardinal): Cardinal;
function  ALMimeBase64EncodedSizeNoCRLF(const InputSize: Cardinal): Cardinal;
function  ALMimeBase64DecodedSize(const InputSize: Cardinal): Cardinal;
procedure ALMimeBase64Encode(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
procedure ALMimeBase64EncodeNoCRLF(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
procedure ALMimeBase64EncodeFullLines(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
function  ALMimeBase64Decode(const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer): Cardinal;
function  ALMimeBase64DecodePartial(const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
function  ALMimeBase64DecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Cardinal;

{From indy}
procedure ALFillMimeContentTypeByExtList(AMIMEList : TStrings);
procedure ALFillExtByMimeContentTypeList(AMIMEList : TStrings);
Function  ALGetDefaultFileExtFromMimeContentType(aContentType: String): String;
Function  ALGetDefaultMIMEContentTypeFromExt(aExt: String): String;

{Mime content type list variable}
Var vAlMimeContentTypeByExtList: Tstrings; {.htm=text/html}
    vAlExtbyMimeContentTypeList: Tstrings; {text/html=.htm}

implementation

Uses Registry,
     inifiles,
     alFcnString;

{--------------------------------------------}
type PALMimeBase64Byte4 = ^TALMimeBase64Byte4;
     TALMimeBase64Byte4 = packed record
       b1: Byte;
       b2: Byte;
       b3: Byte;
       b4: Byte;
     end;
     PALMimeBase64Byte3 = ^TALMimeBase64Byte3;
     TALMimeBase64Byte3 = packed record
       b1: Byte;
       b2: Byte;
       b3: Byte;
     end;

{-------------------------------------------------------------------------------}
{*  Caution: For MimeEncodeStream and all other kinds of multi-buffered         }
{*  Mime encodings (i.e. Files etc.), BufferSize must be set to a multiple of 3 }
{*  Even though the implementation of the Mime decoding routines below          }
{*  do not require a particular buffer size, they work fastest with sizes of    }
{*  multiples of four. The chosen size is a multiple of 3 and of 4 as well.     }
{*  The following numbers are, in addition, also divisible by 1024:             }
{*  $2400, $3000, $3C00, $4800, $5400, $6000, $6C00.                            }
const cALMime_Base64_Buffer_Size = $3000;
      cALMime_Base64_Encoded_Line_Break = 76;
      cALMime_Base64_Decoded_Line_Break = cALMime_Base64_Encoded_Line_Break div 4 * 3;
      cALMime_Base64_Encode_Table: array[0..63] of Byte = (
                                                           065, 066, 067, 068, 069, 070, 071, 072, //  00 - 07
                                                           073, 074, 075, 076, 077, 078, 079, 080, //  08 - 15
                                                           081, 082, 083, 084, 085, 086, 087, 088, //  16 - 23
                                                           089, 090, 097, 098, 099, 100, 101, 102, //  24 - 31
                                                           103, 104, 105, 106, 107, 108, 109, 110, //  32 - 39
                                                           111, 112, 113, 114, 115, 116, 117, 118, //  40 - 47
                                                           119, 120, 121, 122, 048, 049, 050, 051, //  48 - 55
                                                           052, 053, 054, 055, 056, 057, 043, 047  // 56 - 63
                                                          );
      cALMime_Base64_Pad_Char = Byte('=');
      cAlMime_Base64_Decode_Table: array[Byte] of Cardinal = (
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
                                                              255, 255, 255, 255, 255, 255, 255, 255
                                                             );

{*****************************************************************}
function ALMimeBase64EncodeString(const S: AnsiString): AnsiString;
var L: Cardinal;
begin
  if Pointer(S) <> nil then begin
    L := PCardinal(Cardinal(S) - 4)^;
    SetLength(Result, ALMimeBase64EncodedSize(L));
    ALMimeBase64Encode(Pointer(S)^, L, Pointer(Result)^);
  end
  else Result := '';
end;

{***********************************************************************}
function ALMimeBase64EncodeStringNoCRLF(const S: AnsiString): AnsiString;
var L: Cardinal;
begin
  if Pointer(S) <> nil then begin
    L := PCardinal(Cardinal(S) - 4)^;
    SetLength(Result, ALMimeBase64EncodedSizeNoCRLF(L));
    ALMimeBase64EncodeNoCRLF(Pointer(S)^, L, Pointer(Result)^);
  end
  else Result := '';
end;

{*****************************************************************}
function ALMimeBase64DecodeString(const S: AnsiString): AnsiString;
var ByteBuffer, ByteBufferSpace: Cardinal;
    L: Cardinal;
begin
  if Pointer(S) <> nil then begin
    L := PCardinal(Cardinal(S) - 4)^;
    SetLength(Result, ALMimeBase64DecodedSize(L));
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    L := ALMimeBase64DecodePartial(Pointer(S)^, L, Pointer(Result)^, ByteBuffer, ByteBufferSpace);
    Inc(L, ALMimeBase64DecodePartialEnd(Pointer(Cardinal(Result) + L)^, ByteBuffer, ByteBufferSpace));
    SetLength(Result, L);
  end
  else Result := '';
end;

{******************************************************************************************}
procedure ALMimeBase64EncodeStream(const InputStream: TStream; const OutputStream: TStream);
var InputBuffer: array [0..CALMIME_Base64_BUFFER_SIZE - 1] of Byte;
    OutputBuffer: array [0..((CALMIME_Base64_BUFFER_SIZE + 2) div 3) * 4 - 1] of Byte;
    BytesRead: Integer;
begin
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  while BytesRead > 0 do begin
    ALMimeBase64Encode(InputBuffer, BytesRead, OutputBuffer);
    OutputStream.Write(OutputBuffer, ALMimeBase64EncodedSize(BytesRead));
    BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  end;
end;

{******************************************************************************************}
procedure ALMimeBase64DecodeStream(const InputStream: TStream; const OutputStream: TStream);
var ByteBuffer, ByteBufferSpace: Cardinal;
    InputBuffer: array [0..(CALMIME_Base64_BUFFER_SIZE + 3) div 4 * 3 - 1] of Byte;
    OutputBuffer: array [0..CALMIME_Base64_BUFFER_SIZE - 1] of Byte;
    BytesRead: Integer;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  while BytesRead > 0 do begin
    OutputStream.Write(OutputBuffer, ALMimeBase64DecodePartial(InputBuffer, BytesRead, OutputBuffer, ByteBuffer, ByteBufferSpace));
    BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  end;
  OutputStream.Write(OutputBuffer, ALMimeBase64DecodePartialEnd(OutputBuffer, ByteBuffer, ByteBufferSpace));
end;

{********************************************************************}
function ALMimeBase64EncodedSize(const InputSize: Cardinal): Cardinal;
begin
  if InputSize > 0 then Result := (InputSize + 2) div 3 * 4 + (InputSize - 1) div cALMIME_Base64_DECODED_LINE_BREAK * 2
  else Result := InputSize;
end;

{**************************************************************************}
function ALMimeBase64EncodedSizeNoCRLF(const InputSize: Cardinal): Cardinal;
begin
  Result := (InputSize + 2) div 3 * 4;
end;

{********************************************************************}
function ALMimeBase64DecodedSize(const InputSize: Cardinal): Cardinal;
begin
  Result := (InputSize + 3) div 4 * 3;
end;

{************************************************************************************************}
procedure ALMimeBase64Encode(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
var IDelta, ODelta: Cardinal;
begin
  ALMimeBase64EncodeFullLines(InputBuffer, InputByteCount, OutputBuffer);
  IDelta := InputByteCount div CALMIME_Base64_DECODED_LINE_BREAK; // Number of lines processed so far.
  ODelta := IDelta * (CALMIME_Base64_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * CALMIME_Base64_DECODED_LINE_BREAK;
  ALMimeBase64EncodeNoCRLF(Pointer(Cardinal(@InputBuffer) + IDelta)^, InputByteCount - IDelta, Pointer(Cardinal(@OutputBuffer) + ODelta)^);
end;

{*********************************************************************************************************}
procedure ALMimeBase64EncodeFullLines(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
var B, InnerLimit, OuterLimit: Cardinal;
    InPtr: PALMimeBase64Byte3;
    OutPtr: PALMimeBase64Byte4;
begin
  { Do we have enough input to encode a full line? }
  if InputByteCount < CALMIME_Base64_DECODED_LINE_BREAK then Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  InnerLimit := Cardinal(InPtr);
  Inc(InnerLimit, CALMIME_Base64_DECODED_LINE_BREAK);

  OuterLimit := Cardinal(InPtr);
  Inc(OuterLimit, InputByteCount);

  { Multiple line loop. }
  repeat

    { Single line loop. }
    repeat
      { Read 3 bytes from InputBuffer. }
      B := InPtr^.b1;
      B := B shl 8;
      B := B or InPtr^.b2;
      B := B shl 8;
      B := B or InPtr^.b3;
      Inc(InPtr);
      { Write 4 bytes to OutputBuffer (in reverse order). }
      OutPtr^.b4 := CALMIME_Base64_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.b3 := CALMIME_Base64_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.b2 := CALMIME_Base64_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.b1 := CALMIME_Base64_ENCODE_TABLE[B];
      Inc(OutPtr);
    until Cardinal(InPtr) >= InnerLimit;

    { Write line break (CRLF). }
    OutPtr^.b1 := 13;
    OutPtr^.b2 := 10;
    Inc(Cardinal(OutPtr), 2);

    Inc(InnerLimit, CALMIME_Base64_DECODED_LINE_BREAK);
  until InnerLimit > OuterLimit;
end;

{******************************************************************************************************}
procedure ALMimeBase64EncodeNoCRLF(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
var B, InnerLimit, OuterLimit: Cardinal;
    InPtr: PALMimeBase64Byte3;
    OutPtr: PALMimeBase64Byte4;
begin
  if InputByteCount = 0 then Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  OuterLimit := InputByteCount div 3 * 3;

  InnerLimit := Cardinal(InPtr);
  Inc(InnerLimit, OuterLimit);

  { Last line loop. }
  while Cardinal(InPtr) < InnerLimit do begin
    { Read 3 bytes from InputBuffer. }
    B := InPtr^.b1;
    B := B shl 8;
    B := B or InPtr^.b2;
    B := B shl 8;
    B := B or InPtr^.b3;
    Inc(InPtr);
    { Write 4 bytes to OutputBuffer (in reverse order). }
    OutPtr^.b4 := CALMIME_Base64_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.b3 := CALMIME_Base64_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.b2 := CALMIME_Base64_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.b1 := CALMIME_Base64_ENCODE_TABLE[B];
    Inc(OutPtr);
  end;

  { End of data & padding. }
  case InputByteCount - OuterLimit of
    1:
      begin
        B := InPtr^.b1;
        B := B shl 4;
        OutPtr.b2 := CALMIME_Base64_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.b1 := CALMIME_Base64_ENCODE_TABLE[B];
        OutPtr.b3 := CALMIME_Base64_PAD_CHAR; { Pad remaining 2 bytes. }
        OutPtr.b4 := CALMIME_Base64_PAD_CHAR;
      end;
    2:
      begin
        B := InPtr^.b1;
        B := B shl 8;
        B := B or InPtr^.b2;
        B := B shl 2;
        OutPtr.b3 := CALMIME_Base64_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.b2 := CALMIME_Base64_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.b1 := CALMIME_Base64_ENCODE_TABLE[B];
        OutPtr.b4 := CALMIME_Base64_PAD_CHAR; { Pad remaining byte. }
      end;
  end;
end;

{**********************************************************************************************************}
function ALMimeBase64Decode(const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer): Cardinal;
var ByteBuffer, ByteBufferSpace: Cardinal;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  Result := alMimeBase64DecodePartial(InputBuffer, InputBytesCount, OutputBuffer, ByteBuffer, ByteBufferSpace);
  Inc(Result, alMimeBase64DecodePartialEnd(Pointer(Cardinal(@OutputBuffer) + Result)^, ByteBuffer, ByteBufferSpace));
end;

{**************************************************************************************************************************************************************************}
function ALMimeBase64DecodePartial(const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
var lByteBuffer, lByteBufferSpace, C: Cardinal;
    InPtr, OuterLimit: ^Byte;
    OutPtr: PALMimeBase64Byte3;
begin
  if InputBytesCount > 0 then begin
    InPtr := @InputBuffer;
    Cardinal(OuterLimit) := Cardinal(InPtr) + InputBytesCount;
    OutPtr := @OutputBuffer;
    lByteBuffer := ByteBuffer;
    lByteBufferSpace := ByteBufferSpace;
    while InPtr <> OuterLimit do begin
      { Read from InputBuffer. }
      C := CALMIME_Base64_DECODE_TABLE[InPtr^];
      Inc(InPtr);
      if C = $FF then Continue;
      lByteBuffer := lByteBuffer shl 6;
      lByteBuffer := lByteBuffer or C;
      Dec(lByteBufferSpace);
      { Have we read 4 bytes from InputBuffer? }
      if lByteBufferSpace <> 0 then Continue;

      { Write 3 bytes to OutputBuffer (in reverse order). }
      OutPtr^.b3 := Byte(lByteBuffer);
      lByteBuffer := lByteBuffer shr 8;
      OutPtr^.b2 := Byte(lByteBuffer);
      lByteBuffer := lByteBuffer shr 8;
      OutPtr^.b1 := Byte(lByteBuffer);
      lByteBuffer := 0;
      Inc(OutPtr);
      lByteBufferSpace := 4;
    end;
    ByteBuffer := lByteBuffer;
    ByteBufferSpace := lByteBufferSpace;
    Result := Cardinal(OutPtr) - Cardinal(@OutputBuffer);
  end
  else Result := 0;
end;

{*****************************************************************************************************************************}
function ALMimeBase64DecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Cardinal;
var lByteBuffer: Cardinal;
begin
  case ByteBufferSpace of
    1:
      begin
        lByteBuffer := ByteBuffer shr 2;
        PALMimeBase64Byte3(@OutputBuffer)^.b2 := Byte(lByteBuffer);
        lByteBuffer := lByteBuffer shr 8;
        PALMimeBase64Byte3(@OutputBuffer)^.b1 := Byte(lByteBuffer);
        Result := 2;
      end;
    2:
      begin
        lByteBuffer := ByteBuffer shr 4;
        PALMimeBase64Byte3(@OutputBuffer)^.b1 := Byte(lByteBuffer);
        Result := 1;
      end;
  else
    Result := 0;
  end;
end;

{*************************************************************}
procedure ALFillMimeContentTypeByExtList(AMIMEList : TStrings);
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
                aContentType := trim(Reg.ReadString('Content Type'));
                if aContentType <> '' then AMIMEList.Values[alLowerCase(aExt)] := AlLowerCase(aContentType);
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

{*************************************************************}
procedure ALFillExtByMimeContentTypeList(AMIMEList : TStrings);
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
              aContenttype := trim(aContentType);
              if aContentType <> '' then begin
                aExt := reg.ReadString('Extension');
                if aExt <> '' then begin
                  If (aExt[1] <> '.') then aExt := '.' + aExt;
                  AMIMEList.Values[alLowerCase(aContentType)] := AlLowerCase(aExt)
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
Function ALGetDefaultFileExtFromMimeContentType(aContentType: String): String;
Var P: integer;
    Index : Integer;
Begin
  Result := '';

  aContentType := ALLowerCase(aContentType);
  P := AlPosEx(';',aContentType);
  if (P > 0) then delete(aContentType,P,MaxInt);
  aContentType := Trim(AContentType);

  Index := vAlExtbyMimeContentTypeList.IndexOfName(aContentType);
  if Index <> -1 then Result := vAlExtbyMimeContentTypeList.ValueFromIndex[Index];
end;

{****************************************************************}
Function ALGetDefaultMIMEContentTypeFromExt(aExt: String): String;
var Index : Integer;
    LExt: string;
begin
  LExt := AlLowerCase(trim(aExt));
  If (LExt = '') or (LExt[1] <> '.') then LExt := '.' + LExt;
  Index := vAlMimeContentTypeByExtList.IndexOfName(LExt);
  if Index <> -1 then Result := vAlMimeContentTypeByExtList.ValueFromIndex[Index]
  else Result := 'application/octet-stream';
end;




Initialization
 vAlMimeContentTypeByExtList := ThashedStringList.Create;
 vAlExtbyMimeContentTypeList := ThashedStringList.Create;
 ALFillMimeContentTypeByExtList(vAlMimeContentTypeByExtList);
 ALFillExtByMimeContentTypeList(vAlExtbyMimeContentTypeList);

finalization
 vAlMimeContentTypeByExtList.Free;
 vAlExtbyMimeContentTypeList.Free;

end.
