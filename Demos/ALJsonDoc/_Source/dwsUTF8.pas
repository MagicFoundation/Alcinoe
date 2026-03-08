{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsUTF8;

{$I dws.inc}

interface

// This unit holds low-level UTF8 implementations or redirections to implementations
// by other frameworks, you should not be using it directly

function StringToUTF8(const unicodeString : String) : RawByteString;
function UTF8ToString(const utf8String : RawByteString) : String;

function IsValidUTF8(const buf : RawByteString) : Boolean; overload; inline;
function IsValidUTF8(p : PByte; byteSize : Integer) : Boolean; overload;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R-}

// StringToUTF8
//
function StringToUTF8(const unicodeString : String) : RawByteString;
type
   TBytes4 = array [0..3] of Byte;
   PBytes4 = ^TBytes4;
begin
   if unicodeString = '' then Exit('');

   var srcLen := Length(unicodeString);
   SetLength(Result, srcLen * 3);

   var pSrc := PWord(unicodeString);
   var pDest := PByte(Result);

   repeat

      // quick scan over two ASCII chars at once
      while (srcLen >= 2) and ((PCardinal(pSrc)^ and $ff80ff80) = 0) do begin
         var pair := PCardinal(pSrc)^;
         PWord(pDest)^ := Word((pair shr 8) or pair);
         Inc(pSrc, 2);
         Inc(pDest, 2);
         Dec(srcLen, 2);
      end;

      if srcLen = 0 then Break;

      // isolated ASCII char ?
      if pSrc^ < $80 then begin
         pDest^ := pSrc^;
         Inc(pSrc);
         Inc(pDest);
         Dec(srcLen);
         if srcLen = 0 then Break;
      end;

      // decode to ucs4
      var ucs4 : Cardinal;
      if (pSrc^ >= $d800) and (pSrc^ < $e000) then begin

         // surrogate pair
         if srcLen = 1 then begin
            // error, use replacement character
            pDest^ := $ef; Inc(pDest);
            pDest^ := $bf; Inc(pDest);
            pDest^ := $bd; Inc(pDest);
            Break;
         end;
         ucs4 := (pSrc^ - $d800) * $400;
         Inc(pSrc);
         var surrogateLow := pSrc^ - $dc00;
         ucs4 := ucs4 + Cardinal(surrogateLow) + $10000;
         Inc(pSrc);
         Dec(srcLen, 2);

      end else begin

         ucs4 := pSrc^;
         Inc(pSrc);
         Dec(srcLen);

      end;

      // output ucs4 as utf-8
      case ucs4 of
         0..$7ff: begin
            pDest^ := %1100_0000 or (ucs4 shr 6); Inc(pDest);
            pDest^ := %1000_0000 or (ucs4 and %11_1111); Inc(pDest);
         end;
         $800..$ffff : begin
            pDest^ := %1110_0000 or (ucs4 shr 12); Inc(pDest);
            pDest^ := %1000_0000 or ((ucs4 shr 6) and %11_1111); Inc(pDest);
            pDest^ := %1000_0000 or (ucs4 and %11_1111); Inc(pDest);
         end;
         $10000..$10FFFF: begin
            pDest^ := %1111_0000 or (ucs4 shr 18); Inc(pDest);
            pDest^ := %1000_0000 or ((ucs4 shr 12) and %11_1111); Inc(pDest);
            pDest^ := %1000_0000 or ((ucs4 shr 6) and %11_1111); Inc(pDest);
            pDest^ := %1000_0000 or (ucs4 and %11_1111); Inc(pDest);
         end;
      else
         // error since RFC 3629, use replacement character
         pDest^ := $ef; Inc(pDest);
         pDest^ := $bf; Inc(pDest);
         pDest^ := $bd; Inc(pDest);
      end;

   until srcLen <= 0;

   SetLength(Result, UIntPtr(pDest) - UIntPtr(Pointer(Result)));
end;

// UTF8ToString
//
function UTF8ToString(const utf8String : RawByteString) : String;
begin
   if utf8String = '' then Exit('');

   var srcLen := Length(utf8String);
   SetLength(Result, srcLen);

   var pSrc := PByte(utf8String);
   var pDest := PWord(Result);

   repeat

      // quick scan over four bytes at once to ASCII chars
      while (srcLen >= 4) and ((PCardinal(pSrc)^ and $80808080) = 0) do begin
         var quad := PCardinal(pSrc)^;
         Inc(pSrc, 4);

         PCardinal(pDest)^ := (quad shl 8 or (quad and $FF)) and $00ff00ff;
         quad := quad shr 16;
         Inc(pDest, 2);
         PCardinal(pDest)^ := (quad shl 8 or quad) and $00ff00ff;
         Inc(pDest, 2);

         Dec(srcLen, 4);
      end;

      if srcLen = 0 then Break;

      // isolated ASCII char ?
      if pSrc^ < $80 then begin
         pDest^ := pSrc^;
         Inc(pDest);
         Inc(pSrc);
         Dec(srcLen);
         continue;
      end;

      if (pSrc^ and %1110_0000) = %1100_0000 then begin
         // two bytes
         if srcLen < 2 then begin
            // EOF, use replacement character & break
            pDest^ := $FFFD;
            Inc(pDest);
            Break;
         end;
         pDest^ := ((pSrc[0] and  %1_1111) shl 6)
                 +  (pSrc[1] and %11_1111);
         Inc(pDest);
         Inc(pSrc, 2);
         Dec(srcLen, 2);
      end else begin
         var ucs4 : Cardinal;
         if (pSrc^ and %1111_0000) = %1110_0000 then begin
            // three bytes
            if srcLen < 3 then begin
               // EOF, use replacement character & break
               pDest^ := $fffd;
               Inc(pDest);
               Break;
            end;
            ucs4 := ((pSrc[0] and    %1111) shl 12)
                  + ((pSrc[1] and %11_1111) shl 6)
                  +  (pSrc[2] and %11_1111);
            Inc(pSrc, 3);
            Dec(srcLen, 3);
         end else begin
            // four bytes
            if srcLen < 4 then begin
               // EOF, use replacement character & break
               pDest^ := $fffd;
               Inc(pDest);
               Break;
            end;
            ucs4 := ((pSrc[0] and     %111) shl 18)
                  + ((pSrc[1] and %11_1111) shl 12)
                  + ((pSrc[2] and %11_1111) shl 6)
                  +  (pSrc[3] and %11_1111);
            Inc(pSrc, 4);
            Dec(srcLen, 4);
         end;

         if ucs4 <= $ffff then begin
            pDest^ := ucs4;
            Inc(pDest);
         end else begin
            Dec(ucs4, $10000);
            pDest^ := (ucs4 shr 10) + $d800;
            Inc(pDest);
            pDest^ := (ucs4 and $3ff) + $dc00;
            Inc(pDest);
         end;
      end;
   until srcLen <= 0;

   SetLength(Result, (UIntPtr(pDest) - UIntPtr(Pointer(Result))) div 2);
end;

// IsValidUTF8
//
function IsValidUTF8(const buf : RawByteString) : Boolean; overload;
begin
   Result := IsValidUTF8(Pointer(buf), Length(buf));
end;

// IsValidUTF8
//
function IsValidUTF8(p : PByte; byteSize : Integer) : Boolean;
begin
   var n := byteSize;
   while n > 0 do begin
      // gallop over ASCII
      while (n > 4) and ((PUInt32(p)^ and $80808080) = 0) do begin
         Inc(p, 4);
         Dec(n, 4);
      end;
      // non-ASCII
      if p^ >= $80 then begin
         if (p^ and %1110_0000) = %1100_0000 then begin
            // 2 bytes encoding
            if n < 2 then Exit(False);
            Dec(n);
            Inc(p);
            if (p^ and %1100_0000) <> %1000_0000 then
               Exit(False);
         end else if (p^ and %1111_0000) = %1110_0000 then begin
            // 3 bytes encoding
            if n < 3 then Exit(False);
            Dec(n, 2);
            Inc(p);
            if (p^ and %1100_0000) <> %1000_0000 then
               Exit(False);
            Inc(p);
            if (p^ and %1100_0000) <> %1000_0000 then
               Exit(False);
         end else if (p^ and %1111_1000) = %1111_0000 then begin
            // 4 bytes encoding
            if n < 4 then Exit(False);
            Dec(n, 3);
            Inc(p);
            if (p^ and %1100_0000) <> %1000_0000 then
               Exit(False);
            Inc(p);
            if (p^ and %1100_0000) <> %1000_0000 then
               Exit(False);
            Inc(p);
            if (p^ and %1100_0000) <> %1000_0000 then
               Exit(False);
         end else Exit(False);
      end;
      Inc(p);
      Dec(n);
   end;
   Result := True;
end;

end.
