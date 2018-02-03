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
{ The Original Code is bzip2.pas.                                                                  }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All rights reserved.     }
{ Portions created by Julian Seward are Copyright (C) 1996-2006 Julian Seward <jseward@bzip.org>   }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{ The latest release of BZIP2 is available from http://www.bzip.org/                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Header conversion of bzlib.h                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit bzip2;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase,
  JclSysUtils;

//DOM-IGNORE-BEGIN

{
/*-------------------------------------------------------------*/
/*--- Public header file for the library.                   ---*/
/*---                                               bzlib.h ---*/
/*-------------------------------------------------------------*/

/* ------------------------------------------------------------------
   This file is part of bzip2/libbzip2, a program and library for
   lossless, block-sorting data compression.

   bzip2/libbzip2 version 1.0.4 of 20 December 2006
   Copyright (C) 1996-2006 Julian Seward <jseward@bzip.org>

   Please read the WARNING, DISCLAIMER and PATENTS sections in the
   README file.

   This program is released under the terms of the license contained
   in the file LICENSE.
   ------------------------------------------------------------------ */
}

const
  BZ_RUN              = 0;
  BZ_FLUSH            = 1;
  BZ_FINISH           = 2;

  BZ_OK               = 0;
  BZ_RUN_OK           = 1;
  BZ_FLUSH_OK         = 2;
  BZ_FINISH_OK        = 3;
  BZ_STREAM_END       = 4;
  BZ_SEQUENCE_ERROR   = -1;
  BZ_PARAM_ERROR      = -2;
  BZ_MEM_ERROR        = -3;
  BZ_DATA_ERROR       = -4;
  BZ_DATA_ERROR_MAGIC = -5;
  BZ_IO_ERROR         = -6;
  BZ_UNEXPECTED_EOF   = -7;
  BZ_OUTBUFF_FULL     = -8;
  BZ_CONFIG_ERROR     = -9;

type
   bz_stream = record
      next_in: PByte;
      avail_in: Cardinal;
      total_in_lo32: Cardinal;
      total_in_hi32: Cardinal;

      next_out: PByte;
      avail_out: Cardinal;
      total_out_lo32: Cardinal;
      total_out_hi32: Cardinal;

      state: Pointer;

      bzalloc: function (opaque: Pointer; n, m: Integer): Pointer; cdecl; // returns n*m bytes
      bzfree: procedure (opaque, p: Pointer); cdecl; // free p
      opaque: Pointer;
   end;

{$IFNDEF BZIP2_LINKONREQUEST}
//-- Core (low-level) library functions --

function BZ2_bzCompressInit(var strm: bz_stream;
  blockSize100k, verbosity, workFactor: Integer): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

function BZ2_bzCompress(var strm: bz_stream; action: Integer): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

function BZ2_bzCompressEnd(var strm: bz_stream): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

function BZ2_bzDecompressInit(var strm: bz_stream;
  verbosity, small: Integer): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

function BZ2_bzDecompress(var strm: bz_stream): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

function BZ2_bzDecompressEnd(var strm: bz_stream): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

//-- High(er) level library functions --

type
  BZFILE = Pointer;

// TODO: no stdio for static link (problems while linking stdin/stdout/stderr)

{#ifndef BZ_NO_STDIO
#define BZ_MAX_UNUSED 5000

typedef void BZFILE;

BZ_EXTERN BZFILE* BZ_API(BZ2_bzReadOpen) ( 
      int*  bzerror,   
      FILE* f, 
      int   verbosity, 
      int   small,
      void* unused,    
      int   nUnused 
   );

BZ_EXTERN void BZ_API(BZ2_bzReadClose) ( 
      int*    bzerror, 
      BZFILE* b 
   );

BZ_EXTERN void BZ_API(BZ2_bzReadGetUnused) ( 
      int*    bzerror, 
      BZFILE* b, 
      void**  unused,  
      int*    nUnused 
   );

BZ_EXTERN int BZ_API(BZ2_bzRead) ( 
      int*    bzerror, 
      BZFILE* b, 
      void*   buf, 
      int     len 
   );

BZ_EXTERN BZFILE* BZ_API(BZ2_bzWriteOpen) ( 
      int*  bzerror,      
      FILE* f, 
      int   blockSize100k, 
      int   verbosity, 
      int   workFactor 
   );

BZ_EXTERN void BZ_API(BZ2_bzWrite) ( 
      int*    bzerror, 
      BZFILE* b, 
      void*   buf, 
      int     len 
   );

BZ_EXTERN void BZ_API(BZ2_bzWriteClose) ( 
      int*          bzerror, 
      BZFILE*       b, 
      int           abandon, 
      unsigned int* nbytes_in, 
      unsigned int* nbytes_out 
   );

BZ_EXTERN void BZ_API(BZ2_bzWriteClose64) ( 
      int*          bzerror, 
      BZFILE*       b, 
      int           abandon, 
      unsigned int* nbytes_in_lo32, 
      unsigned int* nbytes_in_hi32, 
      unsigned int* nbytes_out_lo32, 
      unsigned int* nbytes_out_hi32
   );
#endif}


//- Utility functions --

function BZ2_bzBuffToBuffCompress(dest: PByte; destLen: PCardinal; source: PByte;
  sourceLen: Cardinal; blockSize100k, verbosity, workFactor: Integer): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

function BZ2_bzBuffToBuffDecompress(dest: PByte; destLen: PCardinal; source: PByte;
  sourceLen: Cardinal; small, verbosity: Integer): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

{
/*--
   Code contributed by Yoshioka Tsuneo (tsuneo@rr.iij4u.or.jp)
   to support better zlib compatibility.
   This code is not _officially_ part of libbzip2 (yet);
   I haven't tested it, documented it, or considered the
   threading-safeness of it.
   If this code breaks, please contact both Yoshioka and me.
--*/
}

function BZ2_bzlibVersion: PAnsiChar;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

// no STDIO (see above)
{
function BZ2_bzopen(path, mode: PChar): BZFILE;

function BZ2_bzdopen(fd: Integer; mode: PChar): BZFILE;

function BZ2_bzread(b: BZFILE; buf: Pointer; len: Integer): Integer;

function BZ2_bzwrite(b: BZFILE; buf: Pointer; len: Integer): Integer;

function BZ2_bzflush(b: BZFILE): Integer;

procedure BZ2_bzclose(b: BZFILE);

function BZ2_bzerror(b: BZFILE; errnum: PInteger): PChar;
}

{$ELSE BZIP2_LINKONREQUEST}
type
  BZ2_bzCompressInit_func = function(var strm: bz_stream;
    blockSize100k, verbosity, workFactor: Integer): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzCompress_func = function(var strm: bz_stream;
    action: Integer): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzCompressEnd_func = function(var strm: bz_stream): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzDecompressInit_func = function(var strm: bz_stream;
    verbosity, small: Integer): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzDecompress_func = function(var strm: bz_stream): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzDecompressEnd_func = function(var strm: bz_stream): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzBuffToBuffCompress_func = function(dest: PByte; destLen: PCardinal;
    source: PByte; sourceLen: Cardinal;
    blockSize100k, verbosity, workFactor: Integer): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzBuffToBuffDecompress_func = function(dest: PByte; destLen: PCardinal;
    source: PByte; sourceLen: Cardinal; small, verbosity: Integer): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzlibVersion_func = function: PAnsiChar;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

var
  BZ2_bzCompressInit: BZ2_bzCompressInit_func = nil;
  BZ2_bzCompress: BZ2_bzCompress_func = nil;
  BZ2_bzCompressEnd: BZ2_bzCompressEnd_func = nil;
  BZ2_bzDecompressInit: BZ2_bzDecompressInit_func = nil;
  BZ2_bzDecompress: BZ2_bzDecompress_func = nil;
  BZ2_bzDecompressEnd: BZ2_bzDecompressEnd_func = nil;
  BZ2_bzBuffToBuffCompress: BZ2_bzBuffToBuffCompress_func = nil;
  BZ2_bzBuffToBuffDecompress: BZ2_bzBuffToBuffDecompress_func = nil;
  BZ2_bzlibVersion: BZ2_bzlibVersion_func = nil;
{$ENDIF BZIP2_LINKONREQUEST}

var
  bz2_internal_error_event: procedure(errcode: Integer) of object = nil;

//DOM-IGNORE-END

const
  {$IFDEF MSWINDOWS}
  BZip2DefaultLibraryName = 'bzip2.dll'; // from http://gnuwin32.sourceforge.net/
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  BZip2DefaultLibraryName = 'libbz2.so.1';
  {$ENDIF UNIX}
  BZip2CompressInitDefaultExportName = 'BZ2_bzCompressInit';
  BZip2CompressDefaultExportName = 'BZ2_bzCompress';
  BZip2CompressEndDefaultExportName = 'BZ2_bzCompressEnd';
  BZip2DecompressInitDefaultExportName = 'BZ2_bzDecompressInit';
  BZip2DecompressDefaultExportName = 'BZ2_bzDecompress';
  BZip2DecompressEndDefaultExportName = 'BZ2_bzDecompressEnd';
  BZip2BuffToBuffCompressDefaultExportName = 'BZ2_bzBuffToBuffCompress';
  BZip2BuffToBuffDecompressDefaultExportName = 'BZ2_bzBuffToBuffDecompress';
  BZip2LibVersionDefaultExportName = 'BZ2_bzlibVersion';

{$IFDEF BZIP2_LINKONREQUEST}
var
  BZip2LibraryName: string = BZip2DefaultLibraryName;
  BZip2CompressInitExportName: string = BZip2CompressInitDefaultExportName;
  BZip2CompressExportName: string = BZip2CompressDefaultExportName;
  BZip2CompressEndExportName: string = BZip2CompressEndDefaultExportName;
  BZip2DecompressInitExportName: string = BZip2DecompressInitDefaultExportName;
  BZip2DecompressExportName: string = BZip2DecompressDefaultExportName;
  BZip2DecompressEndExportName: string = BZip2DecompressEndDefaultExportName;
  BZip2BuffToBuffCompressExportName: string = BZip2BuffToBuffCompressDefaultExportName;
  BZip2BuffToBuffDecompressExportName: string = BZip2BuffToBuffDecompressDefaultExportName;
  BZip2LibVersionExportName: string = BZip2LibVersionDefaultExportName;
  BZip2LibraryHandle: TModuleHandle = INVALID_MODULEHANDLE_VALUE;
{$ENDIF BZIP2_LINKONREQUEST}

function LoadBZip2: Boolean;
function IsBZip2Loaded: Boolean;
procedure UnloadBZip2;

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
  {$IFDEF BZIP2_LINKONREQUEST}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  {$ENDIF BZIP2_LINKONREQUEST}
  System.Types,
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  System.SysUtils;
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF BZIP2_LINKONREQUEST}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$ENDIF BZIP2_LINKONREQUEST}
  Types,
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  SysUtils;
  {$ENDIF ~HAS_UNITSCOPE}

{$IFDEF BZIP2_STATICLINK}
function BZ2_bzCompressInit; external;
function BZ2_bzCompress; external;
function BZ2_bzCompressEnd; external;
function BZ2_bzDecompressInit; external;
function BZ2_bzDecompress; external;
function BZ2_bzDecompressEnd; external;
function BZ2_bzBuffToBuffCompress; external;
function BZ2_bzBuffToBuffDecompress; external;
function BZ2_bzlibVersion; external;
// workaround to make the compiler aware of _BZ2_indexIntoF
// an external must be declared for this function in order to make the compiler considering
// the corresponding PUBDEF in bzlib.obj
// source: CodeGear QA team
{$IFDEF CPU32}
function _BZ2_indexIntoF: Pointer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL} external;
{$ENDIF CPU32}
{$IFDEF CPU64}
function BZ2_indexIntoF: Pointer; external;
{$ENDIF CPU64}

{$IFDEF CPU32}
{$LINK ..\windows\obj\bzip2\win32\bzlib.obj}
{$LINK ..\windows\obj\bzip2\win32\randtable.obj}
{$LINK ..\windows\obj\bzip2\win32\crctable.obj}
{$LINK ..\windows\obj\bzip2\win32\compress.obj}
{$LINK ..\windows\obj\bzip2\win32\decompress.obj}
{$LINK ..\windows\obj\bzip2\win32\huffman.obj}
{$LINK ..\windows\obj\bzip2\win32\blocksort.obj}
{$ENDIF CPU32}
{$IFDEF CPU64}
{$LINK ..\windows\obj\bzip2\win64\bzlib.obj}
{$LINK ..\windows\obj\bzip2\win64\randtable.obj}
{$LINK ..\windows\obj\bzip2\win64\crctable.obj}
{$LINK ..\windows\obj\bzip2\win64\compress.obj}
{$LINK ..\windows\obj\bzip2\win64\decompress.obj}
{$LINK ..\windows\obj\bzip2\win64\huffman.obj}
{$LINK ..\windows\obj\bzip2\win64\blocksort.obj}
{$ENDIF CPU64}

{$IFDEF CPU32}
function _malloc(size: Longint): Pointer; cdecl;
{$ENDIF CPU32}
{$IFDEF CPU64}
function malloc(size: SizeInt): Pointer;
{$ENDIF CPU64}
begin
  GetMem(Result, Size);
end;

{$IFDEF CPU32}
procedure _free(pBlock: Pointer); cdecl;
{$ENDIF CPU32}
{$IFDEF CPU64}
procedure free(pBlock: Pointer);
{$ENDIF CPU64}
begin
  FreeMem(pBlock);
end;

{$IFDEF CPU32}
procedure _bz_internal_error(errcode: Integer); cdecl;
{$ENDIF CPU32}
{$IFDEF CPU64}
procedure bz_internal_error(errcode: Integer);
{$ENDIF CPU64}
begin
  if Assigned(bz2_internal_error_event) then
    bz2_internal_error_event(errcode);
end;

{$ENDIF BZIP2_STATICLINK}

{$IFDEF BZIP2_LINKDLL}
function BZ2_bzCompressInit; external BZip2DefaultLibraryName name BZip2CompressInitDefaultExportName;
function BZ2_bzCompress; external BZip2DefaultLibraryName name BZip2CompressDefaultExportName;
function BZ2_bzCompressEnd; external BZip2DefaultLibraryName name BZip2CompressEndDefaultExportName;
function BZ2_bzDecompressInit; external BZip2DefaultLibraryName name BZip2DecompressInitDefaultExportName;
function BZ2_bzDecompress; external BZip2DefaultLibraryName name BZip2DecompressDefaultExportName;
function BZ2_bzDecompressEnd; external BZip2DefaultLibraryName name BZip2DecompressEndDefaultExportName;
function BZ2_bzBuffToBuffCompress; external BZip2DefaultLibraryName name BZip2BuffToBuffCompressDefaultExportName;
function BZ2_bzBuffToBuffDecompress; external BZip2DefaultLibraryName name BZip2BuffToBuffDecompressDefaultExportName;
function BZ2_bzlibVersion; external BZip2DefaultLibraryName name BZip2LibVersionDefaultExportName;
{$ENDIF BZIP2_LINKDLL}

function LoadBZip2: Boolean;
{$IFDEF BZIP2_LINKONREQUEST}
begin
  Result := BZip2LibraryHandle <> INVALID_MODULEHANDLE_VALUE;
  if Result then
    Exit;

  Result := JclSysUtils.LoadModule(BZip2LibraryHandle, BZip2LibraryName);
  if Result then
  begin
    @BZ2_bzCompressInit := GetModuleSymbol(BZip2LibraryHandle, BZip2CompressInitExportName);
    @BZ2_bzCompress := GetModuleSymbol(BZip2LibraryHandle, BZip2CompressExportName);
    @BZ2_bzCompressEnd := GetModuleSymbol(BZip2LibraryHandle, BZip2CompressEndExportName);
    @BZ2_bzDecompressInit := GetModuleSymbol(BZip2LibraryHandle, BZip2DecompressInitExportName);
    @BZ2_bzDecompress := GetModuleSymbol(BZip2LibraryHandle, BZip2DecompressExportName);
    @BZ2_bzDecompressEnd := GetModuleSymbol(BZip2LibraryHandle, BZip2DecompressEndExportName);
    @BZ2_bzBuffToBuffCompress := GetModuleSymbol(BZip2LibraryHandle, BZip2BuffToBuffCompressExportName);
    @BZ2_bzBuffToBuffDecompress := GetModuleSymbol(BZip2LibraryHandle, BZip2BuffToBuffDecompressExportName);
    @BZ2_bzlibVersion := GetModuleSymbol(BZip2LibraryHandle, BZip2LibVersionExportName);
  end;
end;
{$ELSE ~BZIP2_LINKONREQUEST}
begin
  Result := True;
end;
{$ENDIF ~BZIP2_LINKONREQUEST}

function IsBZip2Loaded: Boolean;
begin
  {$IFDEF BZIP2_LINKONREQUEST}
  Result := BZip2LibraryHandle <> INVALID_MODULEHANDLE_VALUE;
  {$ELSE ~BZIP2_LINKONREQUEST}
  Result := True;
  {$ENDIF ~BZIP2_LINKONREQUEST}
end;

procedure UnloadBZip2;
begin
  {$IFDEF BZIP2_LINKONREQUEST}
  JclSysUtils.UnloadModule(BZip2LibraryHandle);
  {$ENDIF BZIP2_LINKONREQUEST}
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
