/// DO NOT USE !!! CloudFlare's fork of ZLib for SSE3+SSE4.2 Intel/AMD CPUs
// - only supports FPC+Win64 by now, due to Delphi linker issues
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynZLibSSE;

// DEPRECATED UNIT !!!
//  just use plain SynZip.pas unit, defining USEZLIBSSE conditional on FPC+Win64

interface

// About CloudFlare's fork of ZLib for SSE3+SSE4.2 Intel/AMD CPUs:
//  https://github.com/cloudflare/zlib
//  https://blog.cloudflare.com/cloudflare-fights-cancer


implementation

end. // put as reference all deprecated code below
(*

    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****
*)

{$I Synopse.inc} // define HASINLINE CPU32 CPU64

interface

uses
  SysUtils,
  Classes,
  SynCommons;

/// in-memory ZLib DEFLATE compression using CloudFlare's fork
// - returns Z_VERSION_ERROR (-6) if the CPU doesn't support SSE4.2 instructions
// - by default, will use the deflate/.zip header-less format, but you may set
// ZlibFormat=true to add an header, as expected by zlib (and pdf)
function CompressMemSSE42(src, dst: pointer; srcLen, dstLen: integer;
  CompressionLevel: integer=6; ZlibFormat: Boolean=false) : integer;

/// in-memory to stream ZLib DEFLATE compression using CloudFlare's fork
// - returns Z_VERSION_ERROR (-6) if the CPU doesn't support SSE4.2 instructions
// - by default, will use the deflate/.zip header-less format, but you may set
// ZlibFormat=true to add an header, as expected by zlib (and pdf)
function CompressStreamSSE42(src: pointer; srcLen: integer;
  aStream: TStream; tmpbuf: pointer; tmpsize: integer;
  CompressionLevel:integer=6; ZlibFormat: Boolean=false): integer;

/// in-memory ZLib INFLATE decompression using CloudFlare's fork
// - returns Z_VERSION_ERROR (-6) if the CPU doesn't support SSE3 instructions
// - by default, will use the deflate/.zip header-less format, but you may set
// ZlibFormat=true to add an header, as expected by zlib (and pdf)
function UnCompressMemSSE3(src, dst: pointer; srcLen, dstLen: integer; ZlibFormat: Boolean=false) : integer;

/// in-memory to stream ZLib INFLATE decompression using CloudFlare's fork
// - returns Z_VERSION_ERROR (-6) if the CPU doesn't support SSE3 instructions
// - return the number of bytes written into the stream
// - if checkCRC if not nil, it will contain the crc32; if aStream is nil, it
// will only calculate the crc of the the uncompressed memory block
// - by default, will use the deflate/.zip header-less format, but you may set
// ZlibFormat=true to add an header, as expected by zlib (and pdf)
function UnCompressStreamSSE3(src: pointer; srcLen: integer; aStream: TStream;
  checkCRC: PCardinal; tmpbuf: pointer; tmpsize: integer;
  ZlibFormat: Boolean=false): integer;


implementation

type
  ESynZLibSSE42 = class(ESynException);

  { CloudFlare zip uses uLong for total_in, total_out, adler and reserved }
  TZCRC = Int64;
  TZStream = record
    next_in: PAnsiChar;
    avail_in: cardinal;
    total_in: Int64;
    next_out: PAnsiChar;
    avail_out: cardinal;
    total_out: Int64;
    msg: PAnsiChar;
    state: pointer;
    zalloc: pointer;
    zfree: pointer;
    opaque: pointer;
    data_type: integer;
    adler: TZCRC;
    reserved: Int64;
  end;

function adler32(adler: TZCRC; buf: PAnsiChar; len: cardinal): TZCRC; cdecl; external;
function crc32(crc: TZCRC; buf: PAnsiChar; len: cardinal): TZCRC; cdecl; external;
function deflateInit2_(var strm: TZStream;
  level, method, windowBits, memLevel, strategy: integer;
  version: PAnsiChar; stream_size: integer): integer; cdecl; external;
function inflateInit2_(var strm: TZStream; windowBits: integer;
  version: PAnsiChar; stream_size: integer): integer; cdecl; external;
function zlibVersion: PAnsiChar; cdecl; external;
function deflate(var strm: TZStream; flush: integer): integer; cdecl; external;
function deflateEnd(var strm: TZStream): integer; cdecl; external;
function inflate(var strm: TZStream; flush: integer): integer; cdecl; external;
function inflateEnd(var strm: TZStream): integer; cdecl; external;

function malloc(size: cardinal): Pointer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'malloc'{$else}'_malloc'{$endif};{$endif}
begin
  GetMem(Result, size);
end;

procedure free(P: Pointer); cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'free'{$else}'_free'{$endif};{$endif}
begin
  FreeMem(P);
end;

procedure memcpy(dest, src: Pointer; count: integer); cdecl;
  {$ifdef FPC}public name{$ifdef CPU64}'memcpy'{$else}'memcpy'{$endif};{$endif}
begin // will use fastcode if compiled within
  MoveFast(src^, dest^, count);
end;

procedure memset(dest: Pointer; val: Integer; count: integer); cdecl;
  {$ifdef FPC}public name{$ifdef CPU64}'memset'{$else}'memset'{$endif};{$endif}
begin // will use fastcode if compiled within
  FillCharFast(dest^, count, val);
end;

{$ifdef FPC}

{$ifdef WIN32}
  {.$linklib static\i386-win32\libkernel32.a}
  {.$linklib static\i386-win32\libgcc.a}
  {$L static\i386-win32\trees.o}
  {$L static\i386-win32\adler32.o}
  {$L static\i386-win32\crc32.o}
  {$L static\i386-win32\deflate.o}
  {$L static\i386-win32\zutil.o}
  {$L static\i386-win32\inffast.o}
  {$L static\i386-win32\inftrees.o}
  {$L static\i386-win32\inflate.o}
{$endif}

{$ifdef WIN64}
  {$L static\x86_64-win64\sse\trees.o}
  {$L static\x86_64-win64\sse\adler32.o}
  {$L static\x86_64-win64\sse\crc32.o}
  {$L static\x86_64-win64\sse\deflate.o}
  {$L static\x86_64-win64\sse\zutil.o}
  {$L static\x86_64-win64\sse\inffast.o}
  {$L static\x86_64-win64\sse\inftrees.o}
  {$L static\x86_64-win64\sse\inflate.o}
{$endif}

{$endif FPC}

const
  ZLIB_VERSION = '1.2.8';

  Z_NO_FLUSH = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH = 2;
  Z_FULL_FLUSH = 3;
  Z_FINISH = 4;

  Z_OK = 0;
  Z_STREAM_END = 1;
  Z_NEED_DICT = 2;
  Z_ERRNO = -1;
  Z_STREAM_ERROR = -2;
  Z_DATA_ERROR = -3;
  Z_MEM_ERROR = -4;
  Z_BUF_ERROR = -5;
  Z_VERSION_ERROR = -6;

  Z_STORED = 0;
  Z_DEFLATED = 8;
  MAX_WBITS = 15; // 32K LZ77 window
  DEF_MEM_LEVEL = 8;
  Z_DEFAULT_STRATEGY = 0;
  Z_HUFFMAN_ONLY = 2;

function DeflateInit(var Stream: TZStream;
  CompressionLevel: integer; ZlibFormat: Boolean): Boolean;
var Bits: integer;
begin
  if ZlibFormat then
    Bits := MAX_WBITS else
    Bits := -MAX_WBITS;
  result := deflateInit2_(Stream, CompressionLevel, Z_DEFLATED, Bits, DEF_MEM_LEVEL,
    Z_DEFAULT_STRATEGY, ZLIB_VERSION, sizeof(Stream))>=0
end;

function Check(const Code: Integer; const ValidCodes: array of Integer): integer;
var i: Integer;
begin
  if Code=Z_MEM_ERROR then
    OutOfMemoryError;
  result := code;
  for i := Low(ValidCodes) to High(ValidCodes) do
    if ValidCodes[i]=Code then
      exit;
  raise ESynZLibSSE42.CreateUTF8('Error % during zip/deflate process',[Code]);
end;


function CompressMemSSE42(src, dst: pointer; srcLen, dstLen: integer;
  CompressionLevel: integer=6; ZlibFormat: Boolean=false) : integer;
var strm: TZStream;
begin
  if not (cfSSE42 in CpuFeatures) then begin
    result := Z_VERSION_ERROR;
    exit;
  end;
  FillCharFast(strm,sizeof(strm),0);
  strm.next_in := src;
  strm.avail_in := srcLen;
  strm.next_out := dst;
  strm.avail_out := dstLen;
  if DeflateInit(strm,CompressionLevel,ZlibFormat) then
  try
    Check(deflate(strm,Z_FINISH),[Z_STREAM_END,Z_OK]);
  finally
    deflateEnd(strm);
  end;
  result := strm.total_out;
end;

function CompressStreamSSE42(src: pointer; srcLen: integer;
  aStream: TStream; tmpbuf: pointer; tmpsize: integer;
  CompressionLevel: integer=6; ZlibFormat: Boolean=false): integer;
var strm: TZStream;
    code: integer;
  procedure FlushBuf;
  var Count: integer;
  begin
    Count := tmpsize - integer(strm.avail_out);
    if Count=0 then exit;
    aStream.WriteBuffer(tmpbuf^,Count);
    strm.next_out := tmpbuf;
    strm.avail_out := tmpsize;
  end;
begin
  if not (cfSSE42 in CpuFeatures) then begin
    result := Z_VERSION_ERROR;
    exit;
  end;
  FillCharFast(strm,sizeof(strm),0);
  strm.next_in := src;
  strm.avail_in := srcLen;
  strm.next_out := tmpbuf;
  strm.avail_out := tmpsize;
  if DeflateInit(strm,CompressionLevel,ZlibFormat) then
  try
    repeat
      code := Check(deflate(strm, Z_FINISH),[Z_OK,Z_STREAM_END,Z_BUF_ERROR]);
      FlushBuf;
    until code=Z_STREAM_END;
    FlushBuf;
  finally
    deflateEnd(strm);
  end;
  result := strm.total_out;
end;

function UnCompressMemSSE3(src, dst: pointer; srcLen, dstLen: integer;
  ZlibFormat: Boolean) : integer;
var strm: TZStream;
    Bits: integer;
begin
  if not (cfSSE3 in CpuFeatures) then begin
    result := Z_VERSION_ERROR;
    exit;
  end;
  FillCharFast(strm,sizeof(strm),0);
  strm.next_in := src;
  strm.avail_in := srcLen;
  strm.next_out := dst;
  strm.avail_out := dstLen;
  if ZlibFormat then
    Bits := MAX_WBITS else
    Bits := -MAX_WBITS; // -MAX_WBITS -> no zLib header => .zip compatible !
  if inflateInit2_(strm, Bits, ZLIB_VERSION, sizeof(strm))>=0 then
  try
    Check(inflate(strm, Z_FINISH),[Z_OK,Z_STREAM_END]);
  finally
    inflateEnd(strm);
  end;
  result := strm.total_out;
end;

function UnCompressStreamSSE3(src: pointer; srcLen: integer; aStream: TStream;
  checkCRC: PCardinal; tmpbuf: pointer; tmpsize: integer; ZlibFormat: Boolean): integer;
var strm: TZStream;
    code: integer;
    Bits: integer;
  procedure FlushBuf;
  var Count: integer;
  begin
    Count := tmpsize - integer(strm.avail_out);
    if Count=0 then exit;
    if checkCRC<>nil then
      checkCRC^ := crc32(checkCRC^,tmpbuf,Count);
    if aStream<>nil then
      aStream.WriteBuffer(tmpbuf^,Count);
    strm.next_out := tmpbuf;
    strm.avail_out := tmpsize;
  end;
begin
  if not (cfSSE3 in CpuFeatures) then begin
    result := Z_VERSION_ERROR;
    exit;
  end;
  FillCharFast(strm,sizeof(strm),0);
  strm.next_in := src;
  strm.avail_in := srcLen;
  strm.next_out := tmpbuf;
  strm.avail_out := tmpsize;
  if checkCRC<>nil then
    checkCRC^ := 0;
  if ZlibFormat then
    Bits := MAX_WBITS else
    Bits := -MAX_WBITS; // -MAX_WBITS -> no zLib header => .zip compatible !
  if inflateInit2_(strm, Bits, ZLIB_VERSION, sizeof(strm))>=0 then
  try
    repeat
      code := Check(inflate(strm, Z_FINISH),[Z_OK,Z_STREAM_END,Z_BUF_ERROR]);
      FlushBuf;
    until code=Z_STREAM_END;
    FlushBuf;
  finally
    inflateEnd(strm);
  end;
  result := strm.total_out;
end;

{
  Some Numbers with FPC+Win64, without SynZLibSSE:
  - In memory compression: 12 assertions passed  168.06ms
  - GZIP format: 19 assertions passed  322.12ms
  - ZIP format: 76 assertions passed  1.34s
  - SynLZO: 3,006 assertions passed  67.82ms
  - SynLZ: 23,210 assertions passed  400.09ms
  Total failed: 0 / 26,323  - Compression PASSED  2.30s

  When enabling SynZLibSSE:
  - In memory compression: 12 assertions passed  75.69ms
  - GZIP format: 19 assertions passed  258.61ms
  - ZIP format: 76 assertions passed  1.02s
  - SynLZO: 3,006 assertions passed  68.70ms
  - SynLZ: 23,210 assertions passed  403.96ms
  Total failed: 0 / 26,323  - Compression PASSED  1.83s

  Win32, without SynZLibSSE:
  - In memory compression: 12 assertions passed  250.97ms
  - GZIP format: 19 assertions passed  481.25ms
  - ZIP format: 76 assertions passed  2.10s
  - SynLZO: 3,006 assertions passed  67.30ms
  - SynLZ: 30,217 assertions passed  483.62ms
  Total failed: 0 / 33,330  - Compression PASSED  3.38s

}

end.
