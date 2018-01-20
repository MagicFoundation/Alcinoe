{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ zlib.h -- interface of the 'zlib' general purpose compression library                            }
{ version 1.2.1, November 17th, 2003                                                               }
{                                                                                                  }
{ Copyright (C) 1995-2003 Jean-loup Gailly and Mark Adler                                          }
{                                                                                                  }
{ This software is provided 'as-is', without any express or implied warranty.  In no event will    }
{ the authors be held liable for any damages arising from the use of this software.                }
{                                                                                                  }
{ Permission is granted to anyone to use this software for any purpose, including commercial       }
{ applications, and to alter it and redistribute it freely, subject to the following restrictions: }
{                                                                                                  }
{ 1. The origin of this software must not be misrepresented; you must not claim that you wrote the }
{    original software. If you use this software in a product, an acknowledgment in the product    }
{    documentation would be appreciated but is not required.                                       }
{ 2. Altered source versions must be plainly marked as such, and must not be misrepresented as     }
{    being the original software.                                                                  }
{ 3. This notice may not be removed or altered from any source distribution.                       }
{                                                                                                  }
{     Jean-loup Gailly        Mark Adler                                                           }
{     jloup@gzip.org          madler@alumni.caltech.edu                                            }
{                                                                                                  }
{ The data format used by the zlib library is described by RFCs (Request for                       }
{ Comments) 1950 to 1952 in the files http://www.ietf.org/rfc/rfc1950.txt                          }
{ (zlib format), rfc1951.txt (deflate format) and rfc1952.txt (gzip format).                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit zlibh;

{$I jcl.inc}

{$IFDEF ZLIB_LINKDLL}
{$HPPEMIT '#define ZLIB_DLL'}
{$ELSE ~ZLIB_LINKDLL}
{$HPPEMIT '#define ZEXPORT __fastcall'}
{$ENDIF ~ZLIB_LINKDLL}

{$IFDEF ZEXPORT_CDECL}
{$HPPEMIT '#define ZEXPORT __cdecl'}
{$ENDIF ZEXPORT_CDECL}

{$HPPEMIT '#define ZEXPORTVA __cdecl'}

{$HPPEMIT '#define __MACTYPES__'}
{$IFDEF COMPILER10_UP}
{$IFDEF RTL310_UP}
{$HPPEMIT '#include <System.ZLib.hpp>'}
{$ELSE ~RTL310_UP}
{$HPPEMIT '#include <ZLib.hpp>'}
{$ENDIF ~RTL310_UP}
{$ELSE ~COMPILER10_UP}
{$HPPEMIT '#include <zlib.h>'}
{$ENDIF ~COMPILER10_UP}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase,
  JclSysUtils;

//DOM-IGNORE-BEGIN

{$IFNDEF ZLIB_RTL}

{$IFNDEF FPC}
type
  {$IFDEF UNIX}
  uLong = LongWord;
  {$EXTERNALSYM uLong}
  uInt = Cardinal;
  {$EXTERNALSYM uInt}
  {$ENDIF UNIX}
  uShort = Word;
  {$EXTERNALSYM uShort}
  size_t = Longint;
  {$EXTERNALSYM size_t}
{$ENDIF ~FPC}

//-----------------------------------------------------------------------------
// START of the contents of the converted ZCONF.H
//-----------------------------------------------------------------------------
{* zconf.h -- configuration of the zlib compression library
 * Copyright (C) 1995-2003 Jean-loup Gailly.
 * For conditions of distribution and use, see copyright notice in zlib.h

 * If you *really* need a unique prefix for all types and library functions,
 * compile with -DZ_PREFIX. The "standard" zlib should be compiled without it.
 *}

type
  {$EXTERNALSYM Bytef}
  Bytef  = Byte;
  {$EXTERNALSYM PBytef}
  PBytef = ^Bytef;
  {$EXTERNALSYM UnsignedInt}
  UnsignedInt = LongWord;
  {$EXTERNALSYM uLongf}
  uLongf = ULONG;
  {$EXTERNALSYM PuLongf}
  PuLongf = ^uLongf;

{* Maximum value for windowBits in deflateInit2 and inflateInit2.
 * WARNING: reducing MAX_WBITS makes minigzip unable to extract .gz files
 * created by gzip. (Files created by minigzip can still be extracted by
 * gzip.)
 *}

const
  {$EXTERNALSYM MAX_WBITS}
  MAX_WBITS   = 15; // 32K LZ77 window

{* The memory requirements for deflate are (in bytes):
            (1 << (windowBits+2)) +  (1 << (memLevel+9))
 that is: 128K for windowBits=15  +  128K for memLevel = 8  (default values)
 plus a few kilobytes for small objects. For example, if you want to reduce
 the default memory requirements from 256K to 128K, compile with
     make CFLAGS="-O -DMAX_WBITS=14 -DMAX_MEM_LEVEL=7"
 Of course this will generally degrade compression (there's no free lunch).

   The memory requirements for inflate are (in bytes) 1 << windowBits
 that is, 32K for windowBits=15 (default value) plus a few kilobytes
 for small objects.
*}

                        {* Type declarations *}

{* The following definitions for FAR are needed only for MSDOS mixed
 * model programming (small or medium model with some far allocations).
 * This was tested only with MSC; for other MSDOS compilers you may have
 * to define NO_MEMCPY in zutil.h.  If you don't need the mixed model,
 * just define FAR to be empty.
 *}

{* If building or using zlib with the WINAPI/WINAPIV calling convention,
 * define ZLIB_WINAPI.
 * Caution: the standard ZLIB1.DLL is NOT compiled using ZLIB_WINAPI.
 *}

{ $HPPEMIT '#define ZEXPORT   __stdcall'} // OS: CHECKTHIS
{ $HPPEMIT '#define ZEXPORTVA __cdecl'} // OS: CHECKTHIS

// type
//   uInt = UINT; --> already defined in Windows.pas /* 16 bits or more
//   uLong = ULONG; --> already defined in Windows.pas /* 32 bits or more

type
  {$EXTERNALSYM voidpc}
   voidpc = Pointer;
  {$EXTERNALSYM voidpf}
   voidpf = Pointer;
  {$EXTERNALSYM voidp}
   voidp = Pointer;
  {$EXTERNALSYM z_off_t}
   z_off_t = LongInt;

const
  {$EXTERNALSYM SEEK_SET}
  SEEK_SET = 0;       // Seek from beginning of file.
  {$EXTERNALSYM SEEK_CUR}
  SEEK_CUR = 1;       // Seek from current position.
  {$EXTERNALSYM SEEK_END}
  SEEK_END = 2;       // Set file pointer to EOF plus "offset" 

//-----------------------------------------------------------------------------
// END of the contents of the converted ZCONF.H
//-----------------------------------------------------------------------------

const
  {$EXTERNALSYM ZLIB_VERSION}
  ZLIB_VERSION = '1.2.7';
  {$EXTERNALSYM ZLIB_VERNUM}
  ZLIB_VERNUM = $1250;
  {$EXTERNALSYM ZLIB_VER_MAJOR}
  ZLIB_VER_MAJOR = 1;
  {$EXTERNALSYM ZLIB_VER_MINOR}
  ZLIB_VER_MINOR = 2;
  {$EXTERNALSYM ZLIB_VER_REVISION}
  ZLIB_VER_REVISION = 7;
  {$EXTERNALSYM ZLIB_VER_SUBREVISION}
  ZLIB_VER_SUBREVISION = 0;

{*
     The 'zlib' compression library provides in-memory compression and
  decompression functions, including integrity checks of the uncompressed
  data.  This version of the library supports only one compression method
  (deflation) but other algorithms will be added later and will have the same
  stream interface.

     Compression can be done in a single step if the buffers are large
  enough (for example if an input file is mmap'ed), or can be done by
  repeated calls of the compression function.  In the latter case, the
  application must provide more input and/or consume the output
  (providing more output space) before each call.

     The compressed data format used by the in-memory functions is the zlib
  format, which is a zlib wrapper documented in RFC 1950, wrapped around a
  deflate stream, which is itself documented in RFC 1951.

     The library also supports reading and writing files in gzip (.gz) format
  with an interface similar to that of stdio using the functions that start
  with "gz".  The gzip format is different from the zlib format.  gzip is a
  gzip wrapper, documented in RFC 1952, wrapped around a deflate stream.

     The zlib format was designed to be compact and fast for use in memory
  and on communications channels.  The gzip format was designed for single-
  file compression on file systems, has a larger header than zlib to maintain
  directory information, and uses a different, slower check method than zlib.

     This library does not provide any functions to write gzip files in memory.
  However such functions could be easily written using zlib's deflate function,
  the documentation in the gzip RFC, and the examples in gzio.c.

     The library does not install any signal handler. The decoder checks
  the consistency of the compressed data, so the library should never
  crash even in case of corrupted input.
*}

type
  {$EXTERNALSYM alloc_func}
  alloc_func = function(opaque:voidpf; items:uInt; size:uInt):voidpf;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
  {$EXTERNALSYM free_func}
  free_func = procedure(opaque:voidpf; address:voidpf);
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
  {$EXTERNALSYM TFNAllocFunc}
  TFNAllocFunc = alloc_func;
  {$EXTERNALSYM TFNFreeFunc}
  TFNFreeFunc = free_func;

type
  {$EXTERNALSYM internal_state}
  internal_state = packed record end;
  {$EXTERNALSYM TInternalState}
  TInternalState = internal_state; // backward compatibility
  {$EXTERNALSYM PInternalState}
  PInternalState = ^internal_state; // backward compatibility

type
  {$EXTERNALSYM z_stream_s}
  z_stream_s = record
      next_in: PBytef;       // next input byte
      avail_in: uInt;        // number of bytes available at next_in
      total_in: uLong;       // total nb of input bytes read so far

      next_out: PBytef;      // next output byte should be put there
      avail_out:uInt;        // remaining free space at next_out
      total_out:uLong;       // total nb of bytes output so far

      msg:     PAnsiChar;    // last error message, NULL if no error
      state:PInternalState;  // not visible by applications

      zalloc:   TFNAllocFunc;// used to allocate the internal state
      zfree:    TFNFreeFunc; // used to free the internal state
      opaque:   voidpf;      // private data object passed to zalloc and zfree

      data_type: Integer;     // best guess about the data type: ascii or binary
      adler:    uLong;       // adler32 value of the uncompressed data
      reserved: uLong;       // reserved for future use
  end;
  {$IFDEF COMPILER10_UP}
  (*$HPPEMIT 'namespace Zlibh {'*)
  (*$HPPEMIT 'typedef Zlib::TZStreamRec z_stream_s;'*)
  (*$HPPEMIT '}'*)
  {$ENDIF COMPILER10_UP}

  {$EXTERNALSYM z_stream}
  z_stream = z_stream_s;
  {$EXTERNALSYM z_streamp}
  z_streamp = ^z_stream_s;

  {$EXTERNALSYM TZStreamRec}
  TZStreamRec = z_stream_s;
  {$EXTERNALSYM PZStreamRec}
  PZStreamRec = ^z_stream_s;

{*
   The application must update next_in and avail_in when avail_in has
   dropped to zero. It must update next_out and avail_out when avail_out
   has dropped to zero. The application must initialize zalloc, zfree and
   opaque before calling the init function. All other fields are set by the
   compression library and must not be updated by the application.

   The opaque value provided by the application will be passed as the first
   parameter for calls of zalloc and zfree. This can be useful for custom
   memory management. The compression library attaches no meaning to the
   opaque value.

   zalloc must return Z_NULL if there is not enough memory for the object.
   If zlib is used in a multi-threaded application, zalloc and zfree must be
   thread safe.

   On 16-bit systems, the functions zalloc and zfree must be able to allocate
   exactly 65536 bytes, but will not be required to allocate more than this
   if the symbol MAXSEG_64K is defined (see zconf.h). WARNING: On MSDOS,
   pointers returned by zalloc for objects of exactly 65536 bytes *must*
   have their offset normalized to zero. The default allocation function
   provided by this library ensures this (see zutil.c). To reduce memory
   requirements and avoid any allocation of 64K objects, at the expense of
   compression ratio, compile the library with -DMAX_WBITS=14 (see zconf.h).

   The fields total_in and total_out can be used for statistics or
   progress reports. After compression, total_in holds the total size of
   the uncompressed data and may be saved for use in the decompressor
   (particularly if the decompressor wants to decompress everything in
   a single step).
*}

                        {* constants *}

const
  {$EXTERNALSYM Z_NO_FLUSH}
  Z_NO_FLUSH      = 0;
  {$EXTERNALSYM Z_PARTIAL_FLUSH}
  Z_PARTIAL_FLUSH = 1; // will be removed, use Z_SYNC_FLUSH instead
  {$EXTERNALSYM Z_SYNC_FLUSH}
  Z_SYNC_FLUSH    = 2;
  {$EXTERNALSYM Z_FULL_FLUSH}
  Z_FULL_FLUSH    = 3;
  {$EXTERNALSYM Z_FINISH}
  Z_FINISH        = 4;
  {$EXTERNALSYM Z_BLOCK}
  Z_BLOCK         = 5;
  {$EXTERNALSYM Z_TREES}
  Z_TREES         = 6;

{* Allowed flush values; see deflate() and inflate() below for details *}

  {$EXTERNALSYM Z_OK}
  Z_OK            = 0;
  {$EXTERNALSYM Z_STREAM_END}
  Z_STREAM_END    = 1;
  {$EXTERNALSYM Z_NEED_DICT}
  Z_NEED_DICT     = 2;
  {$EXTERNALSYM Z_ERRNO}
  Z_ERRNO        = -1;
  {$EXTERNALSYM Z_STREAM_ERROR}
  Z_STREAM_ERROR = -2;
  {$EXTERNALSYM Z_DATA_ERROR}
  Z_DATA_ERROR   = -3;
  {$EXTERNALSYM Z_MEM_ERROR}
  Z_MEM_ERROR    = -4;
  {$EXTERNALSYM Z_BUF_ERROR}
  Z_BUF_ERROR    = -5;
  {$EXTERNALSYM Z_VERSION_ERROR}
  Z_VERSION_ERROR = -6;
{* Return codes for the compression/decompression functions. Negative
 * values are errors, positive values are used for special but normal events.
 *}

  {$EXTERNALSYM Z_NO_COMPRESSION}
  Z_NO_COMPRESSION       = 0;
  {$EXTERNALSYM Z_BEST_SPEED}
  Z_BEST_SPEED           = 1;
  {$EXTERNALSYM Z_BEST_COMPRESSION}
  Z_BEST_COMPRESSION     = 9;
  {$EXTERNALSYM Z_DEFAULT_COMPRESSION}
  Z_DEFAULT_COMPRESSION = -1;

{* compression levels *}

  {$EXTERNALSYM Z_FILTERED}
  Z_FILTERED           = 1;
  {$EXTERNALSYM Z_HUFFMAN_ONLY}
  Z_HUFFMAN_ONLY       = 2;
  {$EXTERNALSYM Z_RLE}
  Z_RLE                = 3;
  {$EXTERNALSYM Z_FIXED}
  Z_FIXED              = 4;
  {$EXTERNALSYM Z_DEFAULT_STRATEGY}
  Z_DEFAULT_STRATEGY   = 0;
{* compression strategy; see deflateInit2() below for details *}

  {$EXTERNALSYM Z_BINARY}
  Z_BINARY  = 0;
  {$EXTERNALSYM Z_TEXT}
  Z_TEXT    = 1;
  {$EXTERNALSYM Z_ASCII}
  Z_ASCII   = Z_TEXT;
  {$EXTERNALSYM Z_UNKNOWN}
  Z_UNKNOWN = 2;
{* Possible values of the data_type field (though see inflate()) *}

  {$EXTERNALSYM Z_DEFLATED}
  Z_DEFLATED  = 8;
{* The deflate compression method (the only one supported in this version) *}

  {$EXTERNALSYM Z_NULL}
  Z_NULL  = 0;  {* for initializing zalloc, zfree, opaque *}

                        {* basic functions *}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TzlibVersion}
  TzlibVersion = function (): PAnsiChar;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM zlibVersion}
  zlibVersion: TzlibVersion = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM zlibVersion}
function zlibVersion(): PAnsiChar;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{* The application can compare zlibVersion and ZLIB_VERSION for consistency.
   If the first character differs, the library code actually used is
   not compatible with the zlib.h header file used by the application.
   This check is automatically made by deflateInit and inflateInit.
 *}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TdeflateInit_}
  TdeflateInit_ = function (var strm:z_stream;
                            level: Integer;
                            {const} version: PAnsiChar;
                            stream_size: Integer): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM deflateInit_}
  deflateInit_: TdeflateInit_ = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM deflateInit_}
function deflateInit_(var strm:z_stream;
                      level: Integer;
                      {const} version: PAnsiChar;
                      stream_size: Integer): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM deflateInit}
function deflateInit(var strm: TZStreamRec; level: Integer): Integer; // macro
{*
     Initializes the internal stream state for compression. The fields
   zalloc, zfree and opaque must be initialized before by the caller.
   If zalloc and zfree are set to Z_NULL, deflateInit updates them to
   use default allocation functions.

     The compression level must be Z_DEFAULT_COMPRESSION, or between 0 and 9:
   1 gives best speed, 9 gives best compression, 0 gives no compression at
   all (the input data is simply copied a block at a time).
   Z_DEFAULT_COMPRESSION requests a default compromise between speed and
   compression (currently equivalent to level 6).

     deflateInit returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_STREAM_ERROR if level is not a valid compression level,
   Z_VERSION_ERROR if the zlib library version (zlib_version) is incompatible
   with the version assumed by the caller (ZLIB_VERSION).
   msg is set to null if there is no error message.  deflateInit does not
   perform any compression: this will be done by deflate().
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM Tdeflate}
  Tdeflate = function (var strm: TZStreamRec; flush: Integer): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM deflate}
  deflate: Tdeflate = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM deflate}
function deflate(var strm: TZStreamRec; flush: Integer): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
    deflate compresses as much data as possible, and stops when the input
  buffer becomes empty or the output buffer becomes full. It may introduce some
  output latency (reading input without producing any output) except when
  forced to flush.

    The detailed semantics are as follows. deflate performs one or both of the
  following actions:

  - Compress more input starting at next_in and update next_in and avail_in
    accordingly. If not all input can be processed (because there is not
    enough room in the output buffer), next_in and avail_in are updated and
    processing will resume at this point for the next call of deflate().

  - Provide more output starting at next_out and update next_out and avail_out
    accordingly. This action is forced if the parameter flush is non zero.
    Forcing flush frequently degrades the compression ratio, so this parameter
    should be set only when necessary (in interactive applications).
    Some output may be provided even if flush is not set.

  Before the call of deflate(), the application should ensure that at least
  one of the actions is possible, by providing more input and/or consuming
  more output, and updating avail_in or avail_out accordingly; avail_out
  should never be zero before the call. The application can consume the
  compressed output when it wants, for example when the output buffer is full
  (avail_out == 0), or after each call of deflate(). If deflate returns Z_OK
  and with zero avail_out, it must be called again after making room in the
  output buffer because there might be more output pending.

    If the parameter flush is set to Z_SYNC_FLUSH, all pending output is
  flushed to the output buffer and the output is aligned on a byte boundary, so
  that the decompressor can get all input data available so far. (In particular
  avail_in is zero after the call if enough output space has been provided
  before the call.)  Flushing may degrade compression for some compression
  algorithms and so it should be used only when necessary.

    If flush is set to Z_FULL_FLUSH, all output is flushed as with
  Z_SYNC_FLUSH, and the compression state is reset so that decompression can
  restart from this point if previous compressed data has been damaged or if
  random access is desired. Using Z_FULL_FLUSH too often can seriously degrade
  the compression.

    If deflate returns with avail_out == 0, this function must be called again
  with the same value of the flush parameter and more output space (updated
  avail_out), until the flush is complete (deflate returns with non-zero
  avail_out). In the case of a Z_FULL_FLUSH or Z_SYNC_FLUSH, make sure that
  avail_out is greater than six to avoid repeated flush markers due to
  avail_out == 0 on return.

    If the parameter flush is set to Z_FINISH, pending input is processed,
  pending output is flushed and deflate returns with Z_STREAM_END if there
  was enough output space; if deflate returns with Z_OK, this function must be
  called again with Z_FINISH and more output space (updated avail_out) but no
  more input data, until it returns with Z_STREAM_END or an error. After
  deflate has returned Z_STREAM_END, the only possible operations on the
  stream are deflateReset or deflateEnd.

    Z_FINISH can be used immediately after deflateInit if all the compression
  is to be done in a single step. In this case, avail_out must be at least
  the value returned by deflateBound (see below). If deflate does not return
  Z_STREAM_END, then it must be called again as described above.

    deflate() sets strm->adler to the adler32 checksum of all input read
  so far (that is, total_in bytes).

    deflate() may update data_type if it can make a good guess about
  the input data type (Z_ASCII or Z_BINARY). In doubt, the data is considered
  binary. This field is only for information purposes and does not affect
  the compression algorithm in any manner.

    deflate() returns Z_OK if some progress has been made (more input
  processed or more output produced), Z_STREAM_END if all input has been
  consumed and all output has been produced (only when flush is set to
  Z_FINISH), Z_STREAM_ERROR if the stream state was inconsistent (for example
  if next_in or next_out was NULL), Z_BUF_ERROR if no progress is possible
  (for example avail_in or avail_out was zero). Note that Z_BUF_ERROR is not
  fatal, and deflate() can be called again with more input and more output
  space to continue compressing.
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TdeflateEnd}
  TdeflateEnd = function (var strm: TZStreamRec): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM deflateEnd}
  deflateEnd: TdeflateEnd = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM deflateEnd}
function deflateEnd(var strm: TZStreamRec): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     All dynamically allocated data structures for this stream are freed.
   This function discards any unprocessed input and does not flush any
   pending output.

     deflateEnd returns Z_OK if success, Z_STREAM_ERROR if the
   stream state was inconsistent, Z_DATA_ERROR if the stream was freed
   prematurely (some input or output was discarded). In the error case,
   msg may be set but then points to a static string (which must not be
   deallocated).
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TinflateInit_}
  TinflateInit_ = function (var strm:z_stream;
                            {const} version: PAnsiChar;
                            stream_size: Integer): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM inflateInit_}
  inflateInit_: TinflateInit_ = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM inflateInit_}
function inflateInit_(var strm:z_stream;
                      {const} version: PAnsiChar;
                      stream_size: Integer): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM inflateInit}
function inflateInit(var strm: TZStreamRec): Integer; // macro
{*

     Initializes the internal stream state for decompression. The fields
   next_in, avail_in, zalloc, zfree and opaque must be initialized before by
   the caller. If next_in is not Z_NULL and avail_in is large enough (the exact
   value depends on the compression method), inflateInit determines the
   compression method from the zlib header and allocates all data structures
   accordingly; otherwise the allocation will be deferred to the first call of
   inflate.  If zalloc and zfree are set to Z_NULL, inflateInit updates them to
   use default allocation functions.

     inflateInit returns Z_OK if success, Z_MEM_ERROR if there was not enough
   memory, Z_VERSION_ERROR if the zlib library version is incompatible with the
   version assumed by the caller.  msg is set to null if there is no error
   message. inflateInit does not perform any decompression apart from reading
   the zlib header if present: this will be done by inflate().  (So next_in and
   avail_in may be modified, but next_out and avail_out are unchanged.)
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM Tinflate}
  Tinflate = function (var strm: TZStreamRec; flush: Integer): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM inflate}
  inflate: Tinflate = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM inflate}
function inflate(var strm: TZStreamRec; flush: Integer): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
    inflate decompresses as much data as possible, and stops when the input
  buffer becomes empty or the output buffer becomes full. It may introduce
  some output latency (reading input without producing any output) except when
  forced to flush.

  The detailed semantics are as follows. inflate performs one or both of the
  following actions:

  - Decompress more input starting at next_in and update next_in and avail_in
    accordingly. If not all input can be processed (because there is not
    enough room in the output buffer), next_in is updated and processing
    will resume at this point for the next call of inflate().

  - Provide more output starting at next_out and update next_out and avail_out
    accordingly.  inflate() provides as much output as possible, until there
    is no more input data or no more space in the output buffer (see below
    about the flush parameter).

  Before the call of inflate(), the application should ensure that at least
  one of the actions is possible, by providing more input and/or consuming
  more output, and updating the next_* and avail_* values accordingly.
  The application can consume the uncompressed output when it wants, for
  example when the output buffer is full (avail_out == 0), or after each
  call of inflate(). If inflate returns Z_OK and with zero avail_out, it
  must be called again after making room in the output buffer because there
  might be more output pending.

    The flush parameter of inflate() can be Z_NO_FLUSH, Z_SYNC_FLUSH,
  Z_FINISH, or Z_BLOCK. Z_SYNC_FLUSH requests that inflate() flush as much
  output as possible to the output buffer. Z_BLOCK requests that inflate() stop
  if and when it get to the next deflate block boundary. When decoding the zlib
  or gzip format, this will cause inflate() to return immediately after the
  header and before the first block. When doing a raw inflate, inflate() will
  go ahead and process the first block, and will return when it gets to the end
  of that block, or when it runs out of data.

    The Z_BLOCK option assists in appending to or combining deflate streams.
  Also to assist in this, on return inflate() will set strm->data_type to the
  number of unused bits in the last byte taken from strm->next_in, plus 64
  if inflate() is currently decoding the last block in the deflate stream,
  plus 128 if inflate() returned immediately after decoding an end-of-block
  code or decoding the complete header up to just before the first byte of the
  deflate stream. The end-of-block will not be indicated until all of the
  uncompressed data from that block has been written to strm->next_out.  The
  number of unused bits may in general be greater than seven, except when
  bit 7 of data_type is set, in which case the number of unused bits will be
  less than eight.

    inflate() should normally be called until it returns Z_STREAM_END or an
  error. However if all decompression is to be performed in a single step
  (a single call of inflate), the parameter flush should be set to
  Z_FINISH. In this case all pending input is processed and all pending
  output is flushed; avail_out must be large enough to hold all the
  uncompressed data. (The size of the uncompressed data may have been saved
  by the compressor for this purpose.) The next operation on this stream must
  be inflateEnd to deallocate the decompression state. The use of Z_FINISH
  is never required, but can be used to inform inflate that a faster approach
  may be used for the single inflate() call.

     In this implementation, inflate() always flushes as much output as
  possible to the output buffer, and always uses the faster approach on the
  first call. So the only effect of the flush parameter in this implementation
  is on the return value of inflate(), as noted below, or when it returns early
  because Z_BLOCK is used.

     If a preset dictionary is needed after this call (see inflateSetDictionary
  below), inflate sets strm-adler to the adler32 checksum of the dictionary
  chosen by the compressor and returns Z_NEED_DICT; otherwise it sets
  strm->adler to the adler32 checksum of all output produced so far (that is,
  total_out bytes) and returns Z_OK, Z_STREAM_END or an error code as described
  below. At the end of the stream, inflate() checks that its computed adler32
  checksum is equal to that saved by the compressor and returns Z_STREAM_END
  only if the checksum is correct.

    inflate() will decompress and check either zlib-wrapped or gzip-wrapped
  deflate data.  The header type is detected automatically.  Any information
  contained in the gzip header is not retained, so applications that need that
  information should instead use raw inflate, see inflateInit2() below, or
  inflateBack() and perform their own processing of the gzip header and
  trailer.

    inflate() returns Z_OK if some progress has been made (more input processed
  or more output produced), Z_STREAM_END if the end of the compressed data has
  been reached and all uncompressed output has been produced, Z_NEED_DICT if a
  preset dictionary is needed at this point, Z_DATA_ERROR if the input data was
  corrupted (input stream not conforming to the zlib format or incorrect check
  value), Z_STREAM_ERROR if the stream structure was inconsistent (for example
  if next_in or next_out was NULL), Z_MEM_ERROR if there was not enough memory,
  Z_BUF_ERROR if no progress is possible or if there was not enough room in the
  output buffer when Z_FINISH is used. Note that Z_BUF_ERROR is not fatal, and
  inflate() can be called again with more input and more output space to
  continue decompressing. If Z_DATA_ERROR is returned, the application may then
  call inflateSync() to look for a good compression block if a partial recovery
  of the data is desired.
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TinflateEnd}
  TinflateEnd = function (var strm: TZStreamRec): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM inflateEnd}
  inflateEnd: TinflateEnd = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM inflateEnd}
function inflateEnd(var strm: TZStreamRec): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     All dynamically allocated data structures for this stream are freed.
   This function discards any unprocessed input and does not flush any
   pending output.

     inflateEnd returns Z_OK if success, Z_STREAM_ERROR if the stream state
   was inconsistent. In the error case, msg may be set but then points to a
   static string (which must not be deallocated).
*}

                        {* Advanced functions *}

{*
    The following functions are needed only in some special applications.
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TdeflateInit2_}
  TdeflateInit2_ = function (var strm:z_stream;
                             level: Integer;
                             method: Integer;
                             windowBits: Integer;
                             memLevel: Integer;
                             strategy: Integer;
                             {const} version: PAnsiChar;
                             stream_size: Integer): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM deflateInit2_}
  deflateInit2_: TdeflateInit2_ = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM deflateInit2_}
function deflateInit2_(var strm:z_stream;
                       level: Integer;
                       method: Integer;
                       windowBits: Integer;
                       memLevel: Integer;
                       strategy: Integer;
                       {const} version: PAnsiChar;
                       stream_size: Integer): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM deflateInit2}
function deflateInit2(var strm: TZStreamRec;
                      level: Integer;
                      method: Integer;
                      windowBits: Integer;
                      memLevel: Integer;
                      strategy: Integer): Integer; // macro
{*
     This is another version of deflateInit with more compression options. The
   fields next_in, zalloc, zfree and opaque must be initialized before by
   the caller.

     The method parameter is the compression method. It must be Z_DEFLATED in
   this version of the library.

     The windowBits parameter is the base two logarithm of the window size
   (the size of the history buffer). It should be in the range 8..15 for this
   version of the library. Larger values of this parameter result in better
   compression at the expense of memory usage. The default value is 15 if
   deflateInit is used instead.

     windowBits can also be -8..-15 for raw deflate. In this case, -windowBits
   determines the window size. deflate() will then generate raw deflate data
   with no zlib header or trailer, and will not compute an adler32 check value.

     windowBits can also be greater than 15 for optional gzip encoding. Add
   16 to windowBits to write a simple gzip header and trailer around the
   compressed data instead of a zlib wrapper. The gzip header will have no
   file name, no extra data, no comment, no modification time (set to zero),
   no header crc, and the operating system will be set to 255 (unknown).

     The memLevel parameter specifies how much memory should be allocated
   for the internal compression state. memLevel=1 uses minimum memory but
   is slow and reduces compression ratio; memLevel=9 uses maximum memory
   for optimal speed. The default value is 8. See zconf.h for total memory
   usage as a function of windowBits and memLevel.

     The strategy parameter is used to tune the compression algorithm. Use the
   value Z_DEFAULT_STRATEGY for normal data, Z_FILTERED for data produced by a
   filter (or predictor), Z_HUFFMAN_ONLY to force Huffman encoding only (no
   string match), or Z_RLE to limit match distances to one (run-length
   encoding). Filtered data consists mostly of small values with a somewhat
   random distribution. In this case, the compression algorithm is tuned to
   compress them better. The effect of Z_FILTERED is to force more Huffman
   coding and less string matching; it is somewhat intermediate between
   Z_DEFAULT and Z_HUFFMAN_ONLY. Z_RLE is designed to be almost as fast as
   Z_HUFFMAN_ONLY, but give better compression for PNG image data. The strategy
   parameter only affects the compression ratio but not the correctness of the
   compressed output even if it is not set appropriately.

      deflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
   memory, Z_STREAM_ERROR if a parameter is invalid (such as an invalid
   method). msg is set to null if there is no error message.  deflateInit2 does
   not perform any compression: this will be done by deflate().
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TdeflateSetDictionary}
  TdeflateSetDictionary = function(var strm: TZStreamRec;
                                       {const} dictionary: PBytef;
                                       dictLength:uInt): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM deflateSetDictionary}
  deflateSetDictionary: TdeflateSetDictionary = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM deflateSetDictionary}
function deflateSetDictionary(var strm: TZStreamRec;
                              {const} dictionary: PBytef;
                              dictLength:uInt): Integer;
{$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     Initializes the compression dictionary from the given byte sequence
   without producing any compressed output. This function must be called
   immediately after deflateInit, deflateInit2 or deflateReset, before any
   call of deflate. The compressor and decompressor must use exactly the same
   dictionary (see inflateSetDictionary).

     The dictionary should consist of strings (byte sequences) that are likely
   to be encountered later in the data to be compressed, with the most commonly
   used strings preferably put towards the end of the dictionary. Using a
   dictionary is most useful when the data to be compressed is short and can be
   predicted with good accuracy; the data can then be compressed better than
   with the default empty dictionary.

     Depending on the size of the compression data structures selected by
   deflateInit or deflateInit2, a part of the dictionary may in effect be
   discarded, for example if the dictionary is larger than the window size in
   deflate or deflate2. Thus the strings most likely to be useful should be
   put at the end of the dictionary, not at the front.

     Upon return of this function, strm->adler is set to the adler32 value
   of the dictionary; the decompressor may later use this value to determine
   which dictionary has been used by the compressor. (The adler32 value
   applies to the whole dictionary even if only a subset of the dictionary is
   actually used by the compressor.) If a raw deflate was requested, then the
   adler32 value is not computed and strm->adler is not set.

     deflateSetDictionary returns Z_OK if success, or Z_STREAM_ERROR if a
   parameter is invalid (such as NULL dictionary) or the stream state is
   inconsistent (for example if deflate has already been called for this stream
   or if the compression method is bsort). deflateSetDictionary does not
   perform any compression: this will be done by deflate().
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TdeflateCopy}
  TdeflateCopy = function (var dest: TZStreamRec;
                           var source: TZStreamRec): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM deflateCopy}
  deflateCopy: TdeflateCopy = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM deflateCopy}
function deflateCopy(var dest: TZStreamRec;
                     var source: TZStreamRec): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}
{*
     Sets the destination stream as a complete copy of the source stream.

     This function can be useful when several compression strategies will be
   tried, for example when there are several ways of pre-processing the input
   data with a filter. The streams that will be discarded should then be freed
   by calling deflateEnd.  Note that deflateCopy duplicates the internal
   compression state which can be quite large, so this strategy is slow and
   can consume lots of memory.

     deflateCopy returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_STREAM_ERROR if the source stream state was inconsistent
   (such as zalloc being NULL). msg is left unchanged in both source and
   destination.
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TdeflateReset}
  TdeflateReset = function (var strm: TZStreamRec): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM deflateReset}
  deflateReset: TdeflateReset = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM deflateReset}
function deflateReset(var strm: TZStreamRec): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     This function is equivalent to deflateEnd followed by deflateInit,
   but does not free and reallocate all the internal compression state.
   The stream will keep the same compression level and any other attributes
   that may have been set by deflateInit2.

      deflateReset returns Z_OK if success, or Z_STREAM_ERROR if the source
   stream state was inconsistent (such as zalloc or state being NULL).
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TdeflateParams}
  TdeflateParams = function (var strm: TZStreamRec;
                             level: Integer;
                             strategy: Integer): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM deflateParams}
  deflateParams: TdeflateParams = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM deflateParams}
function deflateParams(var strm: TZStreamRec;
                       level: Integer;
                       strategy: Integer): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     Dynamically update the compression level and compression strategy.  The
   interpretation of level and strategy is as in deflateInit2.  This can be
   used to switch between compression and straight copy of the input data, or
   to switch to a different kind of input data requiring a different
   strategy. If the compression level is changed, the input available so far
   is compressed with the old level (and may be flushed); the new level will
   take effect only at the next call of deflate().

     Before the call of deflateParams, the stream state must be set as for
   a call of deflate(), since the currently available input may have to
   be compressed and flushed. In particular, strm->avail_out must be non-zero.

     deflateParams returns Z_OK if success, Z_STREAM_ERROR if the source
   stream state was inconsistent or if a parameter was invalid, Z_BUF_ERROR
   if strm->avail_out was zero.
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TdeflateBound}
  TdeflateBound = function (var strm: TZStreamRec;
                            sourceLen:uLong):uLong;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM deflateBound}
  deflateBound: TdeflateBound = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM deflateBound}
function deflateBound(var strm: TZStreamRec;
                      sourceLen:uLong):uLong;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     deflateBound() returns an upper bound on the compressed size after
   deflation of sourceLen bytes.  It must be called after deflateInit()
   or deflateInit2().  This would be used to allocate an output buffer
   for deflation in a single pass, and so would be called before deflate().
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TdeflatePending}
  TdeflatePending = function (var strm: TZStreamRec;
                              pending: PCardinal;
                              bits: PInteger): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM deflatePending}
  deflatePending: TdeflatePending = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM deflatePending}
function deflatePending(var strm: TZStreamRec;
                        pending: PCardinal;
                        bits: PInteger): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

(*
     deflatePending() returns the number of bytes and bits of output that have
   been generated, but not yet provided in the available output.  The bytes not
   provided would be due to the available output space having being consumed.
   The number of bits of output not provided are between 0 and 7, where they
   await more bits to join them in order to fill out a full byte.  If pending
   or bits are Z_NULL, then those values are not set.

     deflatePending returns Z_OK if success, or Z_STREAM_ERROR if the source
   stream state was inconsistent.
*)

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TdeflatePrime}
  TdeflatePrime = function (var strm: TZStreamRec;
                            bits: Integer;
                            value: Integer): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM deflatePrime}
  deflatePrime: TdeflatePrime = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM deflatePrime}
function deflatePrime(var strm: TZStreamRec;
                      bits: Integer;
                      value: Integer): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     deflatePrime() inserts bits in the deflate output stream.  The intent
  is that this function is used to start off the deflate output with the
  bits leftover from a previous deflate stream when appending to it.  As such,
  this function can only be used for raw deflate, and must be used before the
  first deflate() call after a deflateInit2() or deflateReset().  bits must be
  less than or equal to 16, and that many of the least significant bits of
  value will be inserted in the output.

      deflatePrime returns Z_OK if success, or Z_STREAM_ERROR if the source
   stream state was inconsistent.
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TinflateInit2_}
  TinflateInit2_ = function (var strm:z_stream;
                             windowBits: Integer;
                             {const} version: PAnsiChar;
                             stream_size: Integer): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM inflateInit2_}
  inflateInit2_: TinflateInit2_ = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM inflateInit2_}
function inflateInit2_(var strm:z_stream;
                       windowBits: Integer;
                       {const} version: PAnsiChar;
                       stream_size: Integer): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM inflateInit2}
function inflateInit2(var strm: TZStreamRec;
                      windowBits: Integer): Integer; // macro
{*
     This is another version of inflateInit with an extra parameter. The
   fields next_in, avail_in, zalloc, zfree and opaque must be initialized
   before by the caller.

     The windowBits parameter is the base two logarithm of the maximum window
   size (the size of the history buffer).  It should be in the range 8..15 for
   this version of the library. The default value is 15 if inflateInit is used
   instead. windowBits must be greater than or equal to the windowBits value
   provided to deflateInit2() while compressing, or it must be equal to 15 if
   deflateInit2() was not used. If a compressed stream with a larger window
   size is given as input, inflate() will return with the error code
   Z_DATA_ERROR instead of trying to allocate a larger window.

     windowBits can also be -8..-15 for raw inflate. In this case, -windowBits
   determines the window size. inflate() will then process raw deflate data,
   not looking for a zlib or gzip header, not generating a check value, and not
   looking for any check values for comparison at the end of the stream. This
   is for use with other formats that use the deflate compressed data format
   such as zip.  Those formats provide their own check values. If a custom
   format is developed using the raw deflate format for compressed data, it is
   recommended that a check value such as an adler32 or a crc32 be applied to
   the uncompressed data as is done in the zlib, gzip, and zip formats.  For
   most applications, the zlib format should be used as is. Note that comments
   above on the use in deflateInit2() applies to the magnitude of windowBits.

     windowBits can also be greater than 15 for optional gzip decoding. Add
   32 to windowBits to enable zlib and gzip decoding with automatic header
   detection, or add 16 to decode only the gzip format (the zlib format will
   return a Z_DATA_ERROR).

     inflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
   memory, Z_STREAM_ERROR if a parameter is invalid (such as a negative
   memLevel). msg is set to null if there is no error message.  inflateInit2
   does not perform any decompression apart from reading the zlib header if
   present: this will be done by inflate(). (So next_in and avail_in may be
   modified, but next_out and avail_out are unchanged.)
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TinflateSetDictionary}
  TinflateSetDictionary = function (var strm: TZStreamRec;
                                    {const} dictionary: PBytef;
                                    dictLength:uInt): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM inflateSetDictionary}
  inflateSetDictionary: TinflateSetDictionary = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM inflateSetDictionary}
function inflateSetDictionary(var strm: TZStreamRec;
                              {const} dictionary: PBytef;
                              dictLength:uInt): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     Initializes the decompression dictionary from the given uncompressed byte
   sequence. This function must be called immediately after a call of inflate
   if this call returned Z_NEED_DICT. The dictionary chosen by the compressor
   can be determined from the adler32 value returned by this call of
   inflate. The compressor and decompressor must use exactly the same
   dictionary (see deflateSetDictionary).

     inflateSetDictionary returns Z_OK if success, Z_STREAM_ERROR if a
   parameter is invalid (such as NULL dictionary) or the stream state is
   inconsistent, Z_DATA_ERROR if the given dictionary doesn't match the
   expected one (incorrect adler32 value). inflateSetDictionary does not
   perform any decompression: this will be done by subsequent calls of
   inflate().
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TinflateSync}
  TinflateSync = function (var strm: TZStreamRec): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM inflateSync}
  inflateSync: TinflateSync = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM inflateSync}
function inflateSync(var strm: TZStreamRec): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
    Skips invalid compressed data until a full flush point (see above the
  description of deflate with Z_FULL_FLUSH) can be found, or until all
  available input is skipped. No output is provided.

    inflateSync returns Z_OK if a full flush point has been found, Z_BUF_ERROR
  if no more input was provided, Z_DATA_ERROR if no flush point has been found,
  or Z_STREAM_ERROR if the stream structure was inconsistent. In the success
  case, the application may save the current current value of total_in which
  indicates where valid compressed data was found. In the error case, the
  application may repeatedly call inflateSync, providing more input each time,
  until success or end of the input data.
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TinflateCopy}
  TinflateCopy = function (var dest: TZStreamRec;
                           var source: TZStreamRec): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM inflateCopy}
  inflateCopy: TinflateCopy = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM inflateCopy}
function inflateCopy(var dest: TZStreamRec;
                     var source: TZStreamRec): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     Sets the destination stream as a complete copy of the source stream.

     This function can be useful when randomly accessing a large stream.  The
   first pass through the stream can periodically record the inflate state,
   allowing restarting inflate at those points when randomly accessing the
   stream.

     inflateCopy returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_STREAM_ERROR if the source stream state was inconsistent
   (such as zalloc being NULL). msg is left unchanged in both source and
   destination.
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TinflateReset}
  TinflateReset = function (var strm: TZStreamRec): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM inflateReset}
  inflateReset: TinflateReset = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM inflateReset}
function inflateReset(var strm: TZStreamRec): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     This function is equivalent to inflateEnd followed by inflateInit,
   but does not free and reallocate all the internal decompression state.
   The stream will keep attributes that may have been set by inflateInit2.

      inflateReset returns Z_OK if success, or Z_STREAM_ERROR if the source
   stream state was inconsistent (such as zalloc or state being NULL).
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TinflateBackInit_}
  TinflateBackInit_ = function (var strm:z_stream;
                                windowBits: Integer;
                                window: PByte;
                                {const} version: PAnsiChar;
                                stream_size: Integer): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM inflateBackInit_}
  inflateBackInit_: TinflateBackInit_ = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM inflateBackInit_}
function inflateBackInit_(var strm:z_stream;
                          windowBits: Integer;
                          window: PByte;
                          {const} version: PAnsiChar;
                          stream_size: Integer): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM inflateBackInit}
function inflateBackInit(var strm: TZStreamRec;
                         windowBits: Integer;
                         window: PByte): Integer; // macro
{*
     Initialize the internal stream state for decompression using inflateBack()
   calls.  The fields zalloc, zfree and opaque in strm must be initialized
   before the call.  If zalloc and zfree are Z_NULL, then the default library-
   derived memory allocation routines are used.  windowBits is the base two
   logarithm of the window size, in the range 8..15.  window is a caller
   supplied buffer of that size.  Except for special applications where it is
   assured that deflate was used with small window sizes, windowBits must be 15
   and a 32K byte window must be supplied to be able to decompress general
   deflate streams.

     See inflateBack() for the usage of these routines.

     inflateBackInit will return Z_OK on success, Z_STREAM_ERROR if any of
   the paramaters are invalid, Z_MEM_ERROR if the internal state could not
   be allocated, or Z_VERSION_ERROR if the version of the library does not
   match the version of the header file.
*}

type
  {$EXTERNALSYM in_func}
  in_func = function(p1: Pointer; p2: PByte):UnsignedInt;
  {$EXTERNALSYM out_func}
  out_func = function (p1: Pointer; p2: PByte; p3:UnsignedInt): Longint;
  {$EXTERNALSYM TFNInFunc}
  TFNInFunc = in_func;
  {$EXTERNALSYM TFNOutFunc}
  TFNOutFunc = out_func;

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TinflateBack}
  TinflateBack = function (var strm: TZStreamRec;
                           input:TFNInFunc;
                           in_desc: Pointer;
                           ouput:TFNOutFunc;
                           out_desc: Pointer): Integer; // OS: CHECKTHIS - should the parameter names
                                                        //     be the same as in PHs translation? They
                                                        //     are wrong there, but in/out are reserved
                                                        //     words in Delphi
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM inflateBack}
  inflateBack: TinflateBack = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM inflateBack}
function inflateBack(var strm: TZStreamRec;
                     input:TFNInFunc;
                     in_desc: Pointer;
                     ouput:TFNOutFunc;
                     out_desc: Pointer): Integer; // OS: CHECKTHIS - should the parameter names
                                                //     be the same as in PHs translation? They
                                                //     are wrong there, but in/out are reserved
                                                //     words in Delphi
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     inflateBack() does a raw inflate with a single call using a call-back
   interface for input and output.  This is more efficient than inflate() for
   file i/o applications in that it avoids copying between the output and the
   sliding window by simply making the window itself the output buffer.  This
   function trusts the application to not change the output buffer passed by
   the output function, at least until inflateBack() returns.

     inflateBackInit() must be called first to allocate the internal state
   and to initialize the state with the user-provided window buffer.
   inflateBack() may then be used multiple times to inflate a complete, raw
   deflate stream with each call.  inflateBackEnd() is then called to free
   the allocated state.

     A raw deflate stream is one with no zlib or gzip header or trailer.
   This routine would normally be used in a utility that reads zip or gzip
   files and writes out uncompressed files.  The utility would decode the
   header and process the trailer on its own, hence this routine expects
   only the raw deflate stream to decompress.  This is different from the
   normal behavior of inflate(), which expects either a zlib or gzip header and
   trailer around the deflate stream.

     inflateBack() uses two subroutines supplied by the caller that are then
   called by inflateBack() for input and output.  inflateBack() calls those
   routines until it reads a complete deflate stream and writes out all of the
   uncompressed data, or until it encounters an error.  The function's
   parameters and return types are defined above in the in_func and out_func
   typedefs.  inflateBack() will call in(in_desc, &buf) which should return the
   number of bytes of provided input, and a pointer to that input in buf.  If
   there is no input available, in() must return zero--buf is ignored in that
   case--and inflateBack() will return a buffer error.  inflateBack() will call
   out(out_desc, buf, len) to write the uncompressed data buf[0..len-1].  out()
   should return zero on success, or non-zero on failure.  If out() returns
   non-zero, inflateBack() will return with an error.  Neither in() nor out()
   are permitted to change the contents of the window provided to
   inflateBackInit(), which is also the buffer that out() uses to write from.
   The length written by out() will be at most the window size.  Any non-zero
   amount of input may be provided by in().

     For convenience, inflateBack() can be provided input on the first call by
   setting strm->next_in and strm->avail_in.  If that input is exhausted, then
   in() will be called.  Therefore strm->next_in must be initialized before
   calling inflateBack().  If strm->next_in is Z_NULL, then in() will be called
   immediately for input.  If strm->next_in is not Z_NULL, then strm->avail_in
   must also be initialized, and then if strm->avail_in is not zero, input will
   initially be taken from strm->next_in[0 .. strm->avail_in - 1].

     The in_desc and out_desc parameters of inflateBack() is passed as the
   first parameter of in() and out() respectively when they are called.  These
   descriptors can be optionally used to pass any information that the caller-
   supplied in() and out() functions need to do their job.

     On return, inflateBack() will set strm->next_in and strm->avail_in to
   pass back any unused input that was provided by the last in() call.  The
   return values of inflateBack() can be Z_STREAM_END on success, Z_BUF_ERROR
   if in() or out() returned an error, Z_DATA_ERROR if there was a format
   error in the deflate stream (in which case strm->msg is set to indicate the
   nature of the error), or Z_STREAM_ERROR if the stream was not properly
   initialized.  In the case of Z_BUF_ERROR, an input or output error can be
   distinguished using strm->next_in which will be Z_NULL only if in() returned
   an error.  If strm->next is not Z_NULL, then the Z_BUF_ERROR was due to
   out() returning non-zero.  (in() will always be called before out(), so
   strm->next_in is assured to be defined if out() returns non-zero.)  Note
   that inflateBack() cannot return Z_OK.
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TinflateBackEnd}
  TinflateBackEnd = function (var strm: TZStreamRec): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM inflateBackEnd}
  inflateBackEnd: TinflateBackEnd = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM inflateBackEnd}
function inflateBackEnd(var strm: TZStreamRec): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     All memory allocated by inflateBackInit() is freed.

     inflateBackEnd() returns Z_OK on success, or Z_STREAM_ERROR if the stream
   state was inconsistent.
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TzlibCompileFlags}
  TzlibCompileFlags = function ():uLong;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM zlibCompileFlags}
  zlibCompileFlags: TzlibCompileFlags = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM zlibCompileFlags}
function zlibCompileFlags():uLong;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{* Return flags indicating compile-time options.

    Type sizes, two bits each, 00 = 16 bits, 01 = 32, 10 = 64, 11 = other:
     1.0: size of uInt
     3.2: size of uLong
     5.4: size of voidpf (pointer)
     7.6: size of z_off_t

    Compiler, assembler, and debug options:
     8: DEBUG
     9: ASMV or ASMINF -- use ASM code
     10: ZLIB_WINAPI -- exported functions use the WINAPI calling convention
     11: 0 (reserved)

    One-time table building (smaller code, but not thread-safe if true):
     12: BUILDFIXED -- build static block decoding tables when needed
     13: DYNAMIC_CRC_TABLE -- build CRC calculation tables when needed
     14,15: 0 (reserved)

    Library content (indicates missing functionality):
     16: NO_GZCOMPRESS -- gz* functions cannot compress (to avoid linking
                          deflate code when not needed)
     17: NO_GZIP -- deflate can't write gzip streams, and inflate can't detect
                    and decode gzip streams (to avoid linking crc code)
     18-19: 0 (reserved)

    Operation variations (changes in library functionality):
     20: PKZIP_BUG_WORKAROUND -- slightly more permissive inflate
     21: FASTEST -- deflate algorithm with only one, lowest compression level
     22,23: 0 (reserved)

    The sprintf variant used by gzprintf (zero is best):
     24: 0 = vs*, 1 = s* -- 1 means limited to 20 arguments after the format
     25: 0 = *nprintf, 1 = *printf -- 1 means gzprintf() not secure!
     26: 0 = returns value, 1 = void -- 1 means inferred string length returned

    Remainder:
     27-31: 0 (reserved)
 *}


                        {* utility functions *}

{*
     The following utility functions are implemented on top of the
   basic stream-oriented functions. To simplify the interface, some
   default options are assumed (compression level and memory usage,
   standard memory allocation functions). The source code of these
   utility functions can easily be modified if you need special options.
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM Tcompress}
  Tcompress = function (dest: PBytef;
                        var destLen:uLongf;
                        {const} source: PBytef;
                        sourceLen:uLong): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM compress}
  compress: Tcompress = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM compress}
function compress(dest: PBytef;
                  var destLen:uLongf;
                  {const} source: PBytef;
                  sourceLen:uLong): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     Compresses the source buffer into the destination buffer.  sourceLen is
   the byte length of the source buffer. Upon entry, destLen is the total
   size of the destination buffer, which must be at least the value returned
   by compressBound(sourceLen). Upon exit, destLen is the actual size of the
   compressed buffer.
     This function can be used to compress a whole file at once if the
   input file is mmap'ed.
     compress returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_BUF_ERROR if there was not enough room in the output
   buffer.
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM Tcompress2}
  Tcompress2 = function (dest: PBytef;
                         var destLen:uLongf;
                         {const} source: PBytef;
                         sourceLen:uLong;
                         level: Integer): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM compress2}
  compress2: Tcompress2 = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM compress2}
function compress2(dest: PBytef;
                   var destLen:uLongf;
                   {const} source: PBytef;
                   sourceLen:uLong;
                   level: Integer): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     Compresses the source buffer into the destination buffer. The level
   parameter has the same meaning as in deflateInit.  sourceLen is the byte
   length of the source buffer. Upon entry, destLen is the total size of the
   destination buffer, which must be at least the value returned by
   compressBound(sourceLen). Upon exit, destLen is the actual size of the
   compressed buffer.

     compress2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
   memory, Z_BUF_ERROR if there was not enough room in the output buffer,
   Z_STREAM_ERROR if the level parameter is invalid.
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TcompressBound}
  TcompressBound = function (sourceLen:uLong):uLong;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM compressBound}
  compressBound: TcompressBound = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM compressBound}
function compressBound(sourceLen:uLong):uLong;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     compressBound() returns an upper bound on the compressed size after
   compress() or compress2() on sourceLen bytes.  It would be used before
   a compress() or compress2() call to allocate the destination buffer.
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM Tuncompress}
  Tuncompress = function (dest: PBytef;
                          var destLen:uLongf;
                          {const} source: PBytef;
                          sourceLen:uLong): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM uncompress}
  uncompress: Tuncompress = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM uncompress}
function uncompress(dest: PBytef;
                    var destLen:uLongf;
                    {const} source: PBytef;
                    sourceLen:uLong): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{*
     Decompresses the source buffer into the destination buffer.  sourceLen is
   the byte length of the source buffer. Upon entry, destLen is the total
   size of the destination buffer, which must be large enough to hold the
   entire uncompressed data. (The size of the uncompressed data must have
   been saved previously by the compressor and transmitted to the decompressor
   by some mechanism outside the scope of this compression library.)
   Upon exit, destLen is the actual size of the compressed buffer.
     This function can be used to decompress a whole file at once if the
   input file is mmap'ed.

     uncompress returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_BUF_ERROR if there was not enough room in the output
   buffer, or Z_DATA_ERROR if the input data was corrupted or incomplete.
*}

(*
type
  gzFile = voidp;

function gzopen(path: PAnsiChar; mode: PAnsiChar):gzFile;
{*
     Opens a gzip (.gz) file for reading or writing. The mode parameter
   is as in fopen ("rb" or "wb") but can also include a compression level
   ("wb9") or a strategy: 'f' for filtered data as in "wb6f", 'h' for
   Huffman only compression as in "wb1h", or 'R' for run-length encoding
   as in "wb1R". (See the description of deflateInit2 for more information
   about the strategy parameter.)

     gzopen can be used to read a file which is not in gzip format; in this
   case gzread will directly read from the file without decompression.

     gzopen returns NULL if the file could not be opened or if there was
   insufficient memory to allocate the (de)compression state; errno
   can be checked to distinguish the two cases (if errno is zero, the
   zlib error is Z_MEM_ERROR).  *}

function gzdopen(fd: Integer; mode: PAnsiChar):gzFile;
{*
     gzdopen() associates a gzFile with the file descriptor fd.  File
   descriptors are obtained from calls like open, dup, creat, pipe or
   fileno (in the file has been previously opened with fopen).
   The mode parameter is as in gzopen.
     The next call of gzclose on the returned gzFile will also close the
   file descriptor fd, just like fclose(fdopen(fd), mode) closes the file
   descriptor fd. If you want to keep fd open, use gzdopen(dup(fd), mode).
     gzdopen returns NULL if there was insufficient memory to allocate
   the (de)compression state.
*}

function gzsetparams(file_:gzFile; level: Integer; strategy: Integer): Integer;
{*
     Dynamically update the compression level or strategy. See the description
   of deflateInit2 for the meaning of these parameters.
     gzsetparams returns Z_OK if success, or Z_STREAM_ERROR if the file was not
   opened for writing.
*}

function gzread(file_:gzFile; buf:voidp; len:UnsignedInt): Integer;
{*
     Reads the given number of uncompressed bytes from the compressed file.
   If the input file was not in gzip format, gzread copies the given number
   of bytes into the buffer.
     gzread returns the number of uncompressed bytes actually read (0 for
   end of file, -1 for error). *}

function gzwrite(file_:gzFile;
                 buf:voidpc;
                 len:UnsignedInt): Integer;
{*
     Writes the given number of uncompressed bytes into the compressed file.
   gzwrite returns the number of uncompressed bytes actually written
   (0 in case of error).
*}

// function gzprintf(file_:gzFile; format: PAnsiChar, ...): Integer;
// No ellipsis in Delphi
{*
     Converts, formats, and writes the args to the compressed file under
   control of the format string, as in fprintf. gzprintf returns the number of
   uncompressed bytes actually written (0 in case of error).  The number of
   uncompressed bytes written is limited to 4095. The caller should assure that
   this limit is not exceeded. If it is exceeded, then gzprintf() will return
   return an error (0) with nothing written. In this case, there may also be a
   buffer overflow with unpredictable consequences, which is possible only if
   zlib was compiled with the insecure functions sprintf() or vsprintf()
   because the secure snprintf() or vsnprintf() functions were not available.
*}

function gzputs(file_:gzFile; s: PAnsiChar): Integer;
(*
      Writes the given null-terminated string to the compressed file, excluding
   the terminating null character.
      gzputs returns the number of characters written, or -1 in case of error.
*}

function gzgets(file_:gzFile; buf: PAnsiChar; len: Integer): PAnsiChar;
{*
      Reads bytes from the compressed file until len-1 characters are read, or
   a newline character is read and transferred to buf, or an end-of-file
   condition is encountered.  The string is then terminated with a null
   character.
      gzgets returns buf, or Z_NULL in case of error.
*}

function gzputc(file_:gzFile; c: Integer): Integer;
{*
      Writes c, converted to an unsigned char, into the compressed file.
   gzputc returns the value that was written, or -1 in case of error.
*}

function gzgetc(file_:gzFile): Integer;
{*
      Reads one byte from the compressed file. gzgetc returns this byte
   or -1 in case of end of file or error.
*}

function gzungetc(c: Integer; file_:gzFile): Integer;
{*
      Push one character back onto the stream to be read again later.
   Only one character of push-back is allowed.  gzungetc() returns the
   character pushed, or -1 on failure.  gzungetc() will fail if a
   character has been pushed but not read yet, or if c is -1. The pushed
   character will be discarded if the stream is repositioned with gzseek()
   or gzrewind().
*}

function gzflush(file_:gzFile; flush: Integer): Integer;
{*
     Flushes all pending output into the compressed file. The parameter
   flush is as in the deflate() function. The return value is the zlib
   error number (see function gzerror below). gzflush returns Z_OK if
   the flush parameter is Z_FINISH and all output could be flushed.
     gzflush should be called only when strictly necessary because it can
   degrade compression.
*}

function gzseek(file_:gzFile;
                offset:z_off_t;
                whence: Integer):z_off_t;
{*
      Sets the starting position for the next gzread or gzwrite on the
   given compressed file. The offset represents a number of bytes in the
   uncompressed data stream. The whence parameter is defined as in lseek(2);
   the value SEEK_END is not supported.
     If the file is opened for reading, this function is emulated but can be
   extremely slow. If the file is opened for writing, only forward seeks are
   supported; gzseek then compresses a sequence of zeroes up to the new
   starting position.

      gzseek returns the resulting offset location as measured in bytes from
   the beginning of the uncompressed stream, or -1 in case of error, in
   particular if the file is opened for writing and the new starting position
   would be before the current position.
*}

function gzrewind(file_:gzFile): Integer;
{*
     Rewinds the given file. This function is supported only for reading.

   gzrewind(file) is equivalent to (int)gzseek(file, 0L, SEEK_SET)
*}

function gztell(file_:gzFile):z_off_t;
{*
     Returns the starting position for the next gzread or gzwrite on the
   given compressed file. This position represents a number of bytes in the
   uncompressed data stream.

   gztell(file) is equivalent to gzseek(file, 0L, SEEK_CUR)
*}

function gzeof(file_:gzFile): Integer;
{*
     Returns 1 when EOF has previously been detected reading the given
   input stream, otherwise zero.
*}

function gzclose(file_:gzFile): Integer;
{*
     Flushes all pending output if necessary, closes the compressed file
   and deallocates all the (de)compression state. The return value is the zlib
   error number (see function gzerror below).
*}

function gzerror(file_:gzFile; var errnum: Integer): PAnsiChar;
{*
     Returns the error message for the last error which occurred on the
   given compressed file. errnum is set to zlib error number. If an
   error occurred in the file system and not in the compression library,
   errnum is set to Z_ERRNO and the application may consult errno
   to get the exact error code.
*}

procedure gzclearerr(file_:gzFile);
{*
     Clears the error and end-of-file flags for file. This is analogous to the
   clearerr() function in stdio. This is useful for continuing to read a gzip
   file that is being written concurrently.
*}
*)
                        {* checksum functions *}

{*
     These functions are not related to compression but are exported
   anyway because they might be useful in applications using the
   compression library.
*}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM Tadler32}
  Tadler32 = function (adler:uLong; {const} buf: PBytef; len:uInt):uLong;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM adler32}
  adler32: Tadler32 = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM adler32}
function adler32(adler:uLong; {const} buf: PBytef; len:uInt):uLong;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

(*
     Update a running Adler-32 checksum with the bytes buf[0..len-1] and
   return the updated checksum. If buf is NULL, this function returns
   the required initial value for the checksum.
   An Adler-32 checksum is almost as reliable as a CRC32 but can be computed
   much faster. Usage example:

     uLong adler = adler32(0L, Z_NULL, 0);

     while (read_buffer(buffer, length) != EOF) {
       adler = adler32(adler, buffer, length);
     }
     if (adler != original_adler) error();
*)

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM tcrc32}
  tcrc32 = function (crc:uLong; {const} buf: PBytef; len:uInt):uLong;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM crc32}
  crc32: tcrc32 = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM crc32}
function crc32 (crc:uLong; {const} buf: PBytef; len:uInt):uLong;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

(*
     Update a running crc with the bytes buf[0..len-1] and return the updated
   crc. If buf is NULL, this function returns the required initial value
   for the crc. Pre- and post-conditioning (one's complement) is performed
   within this function so it shouldn't be done by the application.
   Usage example:

     uLong crc = crc32(0L, Z_NULL, 0);

     while (read_buffer(buffer, length) != EOF) {
       crc = crc32(crc, buffer, length);
     }
     if (crc != original_crc) error();
*)

                        {* various hacks, don't look :) *)

{* deflateInit and inflateInit are macros to allow checking the zlib version
 * and the compiler's view of z_stream:
 *}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TzError}
  TzError = function (err: Integer): PAnsiChar;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM zError}
  zError: TzError = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM zError}
function zError(err: Integer): PAnsiChar;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM TinflateSyncPoint}
  TinflateSyncPoint = function (var z: TZStreamRec): Integer;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM inflateSyncPoint}
  inflateSyncPoint: TinflateSyncPoint = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM inflateSyncPoint}
function inflateSyncPoint(var z: TZStreamRec): Integer;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

{$IFDEF ZLIB_LINKONREQUEST}

type
  {$EXTERNALSYM Tget_crc_table}
  Tget_crc_table = function ():PuLongf;
    {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}
var
  {$EXTERNALSYM get_crc_table}
  get_crc_table: Tget_crc_table = nil;

{$ELSE ~ZLIB_LINKONREQUEST}

{$EXTERNALSYM get_crc_table}
function get_crc_table():PuLongf;
  {$IFDEF ZLIB_EXPORT_CDECL} cdecl; {$ENDIF ZLIB_EXPORT_CDECL}

{$ENDIF ~ZLIB_LINKONREQUEST}

//-----------------------------------------------------------------------------
// from zutil.h
//-----------------------------------------------------------------------------

const
  DEF_WBITS = MAX_WBITS;
  {$EXTERNALSYM DEF_WBITS}

// default windowBits for decompression. MAX_WBITS is for compression only

  DEF_MEM_LEVEL = 8;
  {$EXTERNALSYM DEF_MEM_LEVEL}

//DOM-IGNORE-END
{$ENDIF ~ZLIB_RTL}

const
  {$IFDEF MSWINDOWS}
  ZLibDefaultLibraryName = 'zlib1.dll';
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  ZLibDefaultLibraryName = 'libz.so';
  {$ENDIF UNIX}
  ZLibzlibVersionDefaultExportName = 'zlibVersion';
  ZLibdeflateInit_DefaultExportName = 'deflateInit_';
  ZLibdeflateDefaultExportName = 'deflate';
  ZLibdeflateEndDefaultExportName = 'deflateEnd';
  ZLibinflateInit_DefaultExportName = 'inflateInit_';
  ZLibinflateDefaultExportName = 'inflate';
  ZLibinflateEndDefaultExportName = 'inflateEnd';
  ZLibdeflateInit2_DefaultExportName = 'deflateInit2_';
  ZLibdeflateSetDictionaryDefaultExportName = 'deflateSetDictionary';
  ZLibdeflateCopyDefaultExportName = 'deflateCopy';
  ZLibdeflateResetDefaultExportName = 'deflateReset';
  ZLibdeflateParamsDefaultExportName = 'deflateParams';
  ZLibdeflateBoundDefaultExportName = 'deflateBound';
  ZLibdeflatePendingDefaultExportName = 'deflatePending';
  ZLibdeflatePrimeDefaultExportName = 'deflatePrime';
  ZLibinflateInit2_DefaultExportName = 'inflateInit2_';
  ZLibinflateSetDictionaryDefaultExportName = 'inflateSetDictionary';
  ZLibinflateSyncDefaultExportName = 'inflateSync';
  ZLibinflateCopyDefaultExportName = 'inflateCopy';
  ZLibinflateResetDefaultExportName = 'inflateReset';
  ZLibinflateBackInit_DefaultExportName = 'inflateBackInit_';
  ZLibinflateBackDefaultExportName = 'inflateBack';
  ZLibinflateBackEndDefaultExportName = 'inflateBackEnd';
  ZLibzlibCompileFlagsDefaultExportName = 'zlibCompileFlags';
  ZLibcompressDefaultExportName = 'compress';
  ZLibcompress2DefaultExportName = 'compress2';
  ZLibcompressBoundDefaultExportName = 'compressBound';
  ZLibuncompressDefaultExportName = 'uncompress';
  ZLibadler32DefaultExportName = 'adler32';
  ZLibcrc32DefaultExportName = 'crc32';
  ZLibzErrorDefaultExportName = 'zError';
  ZLibinflateSyncPointDefaultExportName = 'inflateSyncPoint';
  ZLibget_crc_tableDefaultExportName = 'get_crc_table';
{$IFDEF ZLIB_LINKONREQUEST}
var
  ZLibLibraryName: string = ZLibDefaultLibraryName;
  ZLibzlibVersionExportName: string = ZLibzlibVersionDefaultExportName;
  ZLibdeflateInit_ExportName: string = ZLibdeflateInit_DefaultExportName;
  ZLibdeflateExportName: string = ZLibdeflateDefaultExportName;
  ZLibdeflateEndExportName: string = ZLibdeflateEndDefaultExportName;
  ZLibinflateInit_ExportName: string = ZLibinflateInit_DefaultExportName;
  ZLibinflateExportName: string = ZLibinflateDefaultExportName;
  ZLibinflateEndExportName: string = ZLibinflateEndDefaultExportName;
  ZLibdeflateInit2_ExportName: string = ZLibdeflateInit2_DefaultExportName;
  ZLibdeflateSetDictionaryExportName: string = ZLibdeflateSetDictionaryDefaultExportName;
  ZLibdeflateCopyExportName: string = ZLibdeflateCopyDefaultExportName;
  ZLibdeflateResetExportName: string = ZLibdeflateResetDefaultExportName;
  ZLibdeflateParamsExportName: string = ZLibdeflateParamsDefaultExportName;
  ZLibdeflateBoundExportName: string = ZLibdeflateBoundDefaultExportName;
  ZLibdeflatePendingExportName: string = ZLibdeflatePendingDefaultExportName;
  ZLibdeflatePrimeExportName: string = ZLibdeflatePrimeDefaultExportName;
  ZLibinflateInit2_ExportName: string = ZLibinflateInit2_DefaultExportName;
  ZLibinflateSetDictionaryExportName: string = ZLibinflateSetDictionaryDefaultExportName;
  ZLibinflateSyncExportName: string = ZLibinflateSyncDefaultExportName;
  ZLibinflateCopyExportName: string = ZLibinflateCopyDefaultExportName;
  ZLibinflateResetExportName: string = ZLibinflateResetDefaultExportName;
  ZLibinflateBackInit_ExportName: string = ZLibinflateBackInit_DefaultExportName;
  ZLibinflateBackExportName: string = ZLibinflateBackDefaultExportName;
  ZLibinflateBackEndExportName: string = ZLibinflateBackEndDefaultExportName;
  ZLibzlibCompileFlagsExportName: string = ZLibzlibCompileFlagsDefaultExportName;
  ZLibcompressExportName: string = ZLibcompressDefaultExportName;
  ZLibcompress2ExportName: string = ZLibcompress2DefaultExportName;
  ZLibcompressBoundExportName: string = ZLibcompressBoundDefaultExportName;
  ZLibuncompressExportName: string = ZLibuncompressDefaultExportName;
  ZLibadler32ExportName: string = ZLibadler32DefaultExportName;
  ZLibcrc32ExportName: string = ZLibcrc32DefaultExportName;
  ZLibzErrorExportName: string = ZLibzErrorDefaultExportName;
  ZLibinflateSyncPointExportName: string = ZLibinflateSyncPointDefaultExportName;
  ZLibget_crc_tableExportName: string = ZLibget_crc_tableDefaultExportName;
{$ENDIF ZLIB_LINKONREQUEST}

var
  ZLibModuleHandle: TModuleHandle = INVALID_MODULEHANDLE_VALUE;

function IsZLibLoaded: Boolean;
function LoadZLib: Boolean;
procedure UnloadZLib;

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

{$IFNDEF ZLIB_RTL}

uses
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils;
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils;
  {$ENDIF ~HAS_UNITSCOPE}

//-----------------------------------------------------------------------------
//
//  These are macros in the C version, just passing down the ZLIB version and
//  the size of TZStreamRec (alias z_stream)
//
//-----------------------------------------------------------------------------

function deflateInit(var strm: TZStreamRec; level: Integer): Integer;
begin
  Result := deflateInit_(strm, level, ZLIB_VERSION, sizeof(TZStreamRec));
end;

function inflateInit(var strm: TZStreamRec): Integer;
begin
  Result := inflateInit_(strm, ZLIB_VERSION, sizeof(TZStreamRec));
end;

function deflateInit2(var strm: TZStreamRec; level: Integer; method: Integer; windowBits: Integer; memLevel: Integer; strategy: Integer): Integer;
begin
  Result := deflateInit2_(strm, level, method, windowBits, memLevel, strategy, ZLIB_VERSION, sizeof(TZStreamRec));
end;

function inflateInit2(var strm: TZStreamRec; windowBits: Integer): Integer;
begin
  Result := inflateInit2_(strm, windowBits, ZLIB_VERSION, sizeof(TZStreamRec));
end;

function inflateBackInit(var strm: TZStreamRec; windowBits: Integer; window: PByte): Integer;
begin
  Result := inflateBackInit_(strm, windowBits, window, ZLIB_VERSION, sizeof(TZStreamRec));
end;

{$IFDEF ZLIB_STATICLINK}

{$IFDEF CPU32}
{$LINK ..\windows\obj\zlib\win32\adler32.obj} // OS: CHECKTHIS - Unix version may need forward slashes?
{$LINK ..\windows\obj\zlib\win32\compress.obj}
{$LINK ..\windows\obj\zlib\win32\crc32.obj}
{$LINK ..\windows\obj\zlib\win32\deflate.obj}
{$LINK ..\windows\obj\zlib\win32\infback.obj}
{$LINK ..\windows\obj\zlib\win32\inffast.obj}
{$LINK ..\windows\obj\zlib\win32\inflate.obj}
{$LINK ..\windows\obj\zlib\win32\inftrees.obj}
{$LINK ..\windows\obj\zlib\win32\trees.obj}
{$LINK ..\windows\obj\zlib\win32\uncompr.obj}
{$LINK ..\windows\obj\zlib\win32\zutil.obj}
{$ENDIF CPU32}
{$IFDEF CPU64}
{$LINK ..\windows\obj\zlib\win64\adler32.obj}
{$LINK ..\windows\obj\zlib\win64\compress.obj}
{$LINK ..\windows\obj\zlib\win64\crc32.obj}
{$LINK ..\windows\obj\zlib\win64\deflate.obj}
{$LINK ..\windows\obj\zlib\win64\infback.obj}
{$LINK ..\windows\obj\zlib\win64\inffast.obj}
{$LINK ..\windows\obj\zlib\win64\inflate.obj}
{$LINK ..\windows\obj\zlib\win64\inftrees.obj}
{$LINK ..\windows\obj\zlib\win64\trees.obj}
{$LINK ..\windows\obj\zlib\win64\uncompr.obj}
{$LINK ..\windows\obj\zlib\win64\zutil.obj}
{$ENDIF CPU64}

// Core functions
function zlibVersion;          external;
function deflateInit_;         external; // wrapped by deflateInit()
function deflate;              external;
function deflateEnd;           external;
function inflateInit_;         external; // wrapped by inflateInit()
function inflate;              external;
function inflateEnd;           external;
function deflateInit2_;        external; // wrapped by deflateInit2()
function deflateSetDictionary; external;
function deflateCopy;          external;
function deflateReset;         external;
function deflateParams;        external;
function deflateBound;         external;
function deflatePending;       external;
function deflatePrime;         external;
function inflateInit2_;        external; // wrapped by inflateInit2()
function inflateSetDictionary; external;
function inflateSync;          external;
function inflateCopy;          external;
function inflateReset;         external;
function inflateBackInit_;     external;
function inflateBack;          external;
function inflateBackEnd;       external;
function zlibCompileFlags;     external;
function compress;             external;
function compress2;            external;
function compressBound;        external;
function uncompress;           external;
// Checksums
function adler32;              external;
function crc32;                external;
function zError;               external;
function inflateSyncPoint;     external;
function get_crc_table;        external;

{$IFDEF LINKTO_MSVCRT_DLL}

{ Win32 implementation specific!!! Imports from MSVCRT.DLL
  Checked availability for Windows 95B and Windows 2000 SP4

  _memcpy      ->            MSVCRT:memcpy
  _memset      ->            MSVCRT:memset
  _malloc      ->            MSVCRT:malloc
  _strlen      ->            MSVCRT:strlen
  ___errno     ->            MSVCRT:_errno
  _fopen       ->            MSVCRT:fopen
  _fdopen      ->            MSVCRT:_fdopen
  _fprintf     ->            MSVCRT:fprintf
  _ftell       ->            MSVCRT:ftell
  _sprintf     ->            MSVCRT:sprintf
  _fwrite      ->            MSVCRT:fwrite
  _fread       ->            MSVCRT:fread
  _free        ->            MSVCRT:free
  _fclose      ->            MSVCRT:fclose
  _vsnprintf   ->            MSVCRT:_vsnprintf
  _fflush      ->            MSVCRT:fflush
  _fseek       ->            MSVCRT:fseek
  _fputc       ->            MSVCRT:fputc
  _strcat      ->            MSVCRT:strcat
  _clearerr    ->            MSVCRT:clearerr
}

{* Just as a hint. Since these functions are already bound at the time the OBJ
 * file was created, it's only important that they be of CDECL calling convention.
 * Actually it's not even important wether the parameters or the number of
 * parameters is correct (especially important for variable-parameter functions).
 * Only the symbol names and the calling convention are important here as long
 * as only the OBJ use these functions
 *
 * This is just a "dirty" trick to get these "missing" imports linked
 *}

const
  szMSVCRT = 'MSVCRT.DLL';

function _memcpy(dest, src: Pointer; count: size_t): Pointer; cdecl; external szMSVCRT name 'memcpy';
function _memset(dest: Pointer; val: Integer; count: size_t): Pointer; cdecl; external szMSVCRT name 'memset';
function _malloc(size: size_t): Pointer; cdecl; external szMSVCRT name 'malloc';
procedure _free(pBlock: Pointer); cdecl; external szMSVCRT name 'free';
function ___errno(): Integer; cdecl; external szMSVCRT name '_errno';
function _fopen(filename: PAnsiChar; mode: PAnsiChar): Pointer; cdecl; external szMSVCRT name 'fopen';
function _fdopen(handle: Integer; mode: PAnsiChar): Pointer; cdecl; external szMSVCRT name '_fdopen';
function _fprintf(stream: Pointer; format: PAnsiChar {, ...}): Integer; cdecl; external szMSVCRT name 'fprintf';
function _ftell(stream: Pointer): Longint; cdecl; external szMSVCRT name 'ftell';
function _sprintf(buffer: PAnsiChar; format: PAnsiChar {, ...}): Integer; cdecl; external szMSVCRT name 'sprintf';
function _fwrite(buffer: Pointer; size: size_t; count: size_t; stream: Pointer): size_t; cdecl; external szMSVCRT name 'fwrite';
function _fread(buffer: Pointer; size: size_t; count: size_t; stream: Pointer): size_t; cdecl; external szMSVCRT name 'fread';
function _fclose(stream: Pointer): Integer; cdecl; external szMSVCRT name 'fclose';
function _vsnprintf(buffer: PAnsiChar; count: size_t; format: PAnsiChar; argptr:array of const): Integer; cdecl; external szMSVCRT name '_vsnprintf';
function _fflush(stream: Pointer): Integer; cdecl; external szMSVCRT name 'fflush';
function _fseek(stream: Pointer; offset: Longint; origin: Integer): Integer; cdecl; external szMSVCRT name 'fseek';
function _fputc(c: Integer; stream: Pointer): Integer; cdecl; external szMSVCRT name 'fputc';
function _strcat(strDestination: PAnsiChar; strSource: PAnsiChar): PAnsiChar; cdecl; external szMSVCRT name 'strcat';
function _strlen(str: PAnsiChar): size_t; cdecl; external szMSVCRT name 'strlen';
procedure _clearerr(stream: Pointer); cdecl; external szMSVCRT name 'clearerr';

{$ELSE ~LINK_TO_MSVCRT}

{$IFDEF CPU32}
function _memcpy(dest, src: Pointer; count: size_t): Pointer; cdecl;
{$ENDIF CPU32}
{$IFDEF CPU64}
function memcpy(dest, src: Pointer; count: size_t): Pointer;
{$ENDIF CPU64}
begin
  Move(src^, dest^, count);
  Result := dest;
end;

{$IFDEF CPU32}
function _memset(dest: Pointer; val: Integer; count: size_t): Pointer; cdecl;
{$ENDIF CPU32}
{$IFDEF CPU64}
function memset(dest: Pointer; val: Integer; count: size_t): Pointer;
{$ENDIF CPU64}
begin
  FillChar(dest^, count, val);
  Result := dest;
end;

{$IFDEF CPU32}
function _malloc(size: size_t): Pointer; cdecl;
{$ENDIF CPU32}
{$IFDEF CPU64}
function malloc(size: size_t): Pointer;
{$ENDIF CPU64}
begin
  GetMem(Result, size);
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

{$ENDIF ~LINK_TO_MSVCRT}

{$IFDEF CPU32}
procedure __llmod; cdecl;
asm
  jmp System.@_llmod;
end;
{$ENDIF CPU32}

{$ENDIF ZLIB_STATICLINK}
{$ENDIF ~ZLIB_RTL}

function IsZLibLoaded: Boolean;
begin
  {$IFDEF ZLIB_LINKONREQUEST}
  Result := ZLibModuleHandle <> INVALID_MODULEHANDLE_VALUE;
  {$ELSE ~ZLIB_LINKONREQUEST}
  Result := True;
  {$ENDIF ~ZLIB_LINKONREQUEST}
end;

function LoadZLib: Boolean;
{$IFDEF ZLIB_LINKONREQUEST}
begin
  Result := ZLibModuleHandle <> INVALID_MODULEHANDLE_VALUE;
  if Result then
    Exit;

  Result := JclSysUtils.LoadModule(ZLibModuleHandle, ZLibLibraryName);
  if Result then
  begin
    @zlibVersion := GetModuleSymbol(ZLibModuleHandle, ZLIBzlibVersionExportName);
    @deflateInit_ := GetModuleSymbol(ZLibModuleHandle, ZLIBdeflateInit_ExportName);
    @deflate := GetModuleSymbol(ZLibModuleHandle, ZLIBdeflateExportName);
    @deflateEnd := GetModuleSymbol(ZLibModuleHandle, ZLIBdeflateEndExportName);
    @inflateInit_ := GetModuleSymbol(ZLibModuleHandle, ZLIBinflateInit_ExportName);
    @inflate := GetModuleSymbol(ZLibModuleHandle, ZLIBinflateExportName);
    @inflateEnd := GetModuleSymbol(ZLibModuleHandle, ZLIBinflateEndExportName);
    @deflateInit2_ := GetModuleSymbol(ZLibModuleHandle, ZLIBdeflateInit2_ExportName);
    @deflateSetDictionary := GetModuleSymbol(ZLibModuleHandle, ZLIBdeflateSetDictionaryExportName);
    @deflateCopy := GetModuleSymbol(ZLibModuleHandle, ZLIBdeflateCopyExportName);
    @deflateReset := GetModuleSymbol(ZLibModuleHandle, ZLIBdeflateResetExportName);
    @deflateParams := GetModuleSymbol(ZLibModuleHandle, ZLIBdeflateParamsExportName);
    @deflateBound := GetModuleSymbol(ZLibModuleHandle, ZLIBdeflateBoundExportName);
    @deflatePending := GetModuleSymbol(ZLibModuleHandle, ZLIBdeflatePendingExportName);
    @deflatePrime := GetModuleSymbol(ZLibModuleHandle, ZLIBdeflatePrimeExportName);
    @inflateInit2_ := GetModuleSymbol(ZLibModuleHandle, ZLIBinflateInit2_ExportName);
    @inflateSetDictionary := GetModuleSymbol(ZLibModuleHandle, ZLIBinflateSetDictionaryExportName);
    @inflateSync := GetModuleSymbol(ZLibModuleHandle, ZLIBinflateSyncExportName);
    @inflateCopy := GetModuleSymbol(ZLibModuleHandle, ZLIBinflateCopyExportName);
    @inflateReset := GetModuleSymbol(ZLibModuleHandle, ZLIBinflateResetExportName);
    @inflateBackInit_ := GetModuleSymbol(ZLibModuleHandle, ZLIBinflateBackInit_ExportName);
    @inflateBack := GetModuleSymbol(ZLibModuleHandle, ZLIBinflateBackExportName);
    @inflateBackEnd := GetModuleSymbol(ZLibModuleHandle, ZLIBinflateBackEndExportName);
    @zlibCompileFlags := GetModuleSymbol(ZLibModuleHandle, ZLIBzlibCompileFlagsExportName);
    @compress := GetModuleSymbol(ZLibModuleHandle, ZLIBcompressExportName);
    @compress2 := GetModuleSymbol(ZLibModuleHandle, ZLIBcompress2ExportName);
    @compressBound := GetModuleSymbol(ZLibModuleHandle, ZLIBcompressBoundExportName);
    @uncompress := GetModuleSymbol(ZLibModuleHandle, ZLIBuncompressExportName);
    @adler32 := GetModuleSymbol(ZLibModuleHandle, ZLIBadler32ExportName);
    @crc32 := GetModuleSymbol(ZLibModuleHandle, ZLIBcrc32ExportName);
    @zError := GetModuleSymbol(ZLibModuleHandle, ZLIBzErrorExportName);
    @inflateSyncPoint := GetModuleSymbol(ZLibModuleHandle, ZLIBinflateSyncPointExportName);
    @get_crc_table := GetModuleSymbol(ZLibModuleHandle, ZLIBget_crc_tableExportName);
  end;
end;
{$ELSE ~ZLIB_LINKONREQUEST}
begin
  Result := True;
end;
{$ENDIF ~ZLIB_LINKONREQUEST}

procedure UnloadZLib;
begin
  {$IFDEF ZLIB_LINKONREQUEST}
  JclSysUtils.UnloadModule(ZLibModuleHandle);
  {$ENDIF ZLIB_LINKONREQUEST}
end;

{$IFNDEF ZLIB_RTL}
{$IFDEF ZLIB_LINKDLL}
// Core functions
function zlibVersion;          external ZLibDefaultLibraryName name ZLibzlibVersionDefaultExportName;
function deflateInit_;         external ZLibDefaultLibraryName name ZLibdeflateInit_DefaultExportName;
function deflate;              external ZLibDefaultLibraryName name ZLibdeflateDefaultExportName;
function deflateEnd;           external ZLibDefaultLibraryName name ZLibdeflateEndDefaultExportName;
function inflateInit_;         external ZLibDefaultLibraryName name ZLibinflateInit_DefaultExportName;
function inflate;              external ZLibDefaultLibraryName name ZLibinflateDefaultExportName;
function inflateEnd;           external ZLibDefaultLibraryName name ZLibinflateEndDefaultExportName;
function deflateInit2_;        external ZLibDefaultLibraryName name ZLibdeflateInit2_DefaultExportName;
function deflateSetDictionary; external ZLibDefaultLibraryName name ZLibdeflateSetDictionaryDefaultExportName;
function deflateCopy;          external ZLibDefaultLibraryName name ZLibdeflateCopyDefaultExportName;
function deflateReset;         external ZLibDefaultLibraryName name ZLibdeflateResetDefaultExportName;
function deflateParams;        external ZLibDefaultLibraryName name ZLibdeflateParamsDefaultExportName;
function deflateBound;         external ZLibDefaultLibraryName name ZLibdeflateBoundDefaultExportName;
function deflatePending;       external ZLibDefaultLibraryName name ZLibdeflatePendingDefaultExportName;
function deflatePrime;         external ZLibDefaultLibraryName name ZLibdeflatePrimeDefaultExportName;
function inflateInit2_;        external ZLibDefaultLibraryName name ZLibinflateInit2_DefaultExportName;
function inflateSetDictionary; external ZLibDefaultLibraryName name ZLibinflateSetDictionaryDefaultExportName;
function inflateSync;          external ZLibDefaultLibraryName name ZLibinflateSyncDefaultExportName;
function inflateCopy;          external ZLibDefaultLibraryName name ZLibinflateCopyDefaultExportName;
function inflateReset;         external ZLibDefaultLibraryName name ZLibinflateResetDefaultExportName;

function inflateBackInit_;     external ZLibDefaultLibraryName name ZLibinflateBackInit_DefaultExportName;
function inflateBack;          external ZLibDefaultLibraryName name ZLibinflateBackDefaultExportName;
function inflateBackEnd;       external ZLibDefaultLibraryName name ZLibinflateBackEndDefaultExportName;
function zlibCompileFlags;     external ZLibDefaultLibraryName name ZLibzlibCompileFlagsDefaultExportName;
function compress;             external ZLibDefaultLibraryName name ZLibcompressDefaultExportName;
function compress2;            external ZLibDefaultLibraryName name ZLibcompress2DefaultExportName;
function compressBound;        external ZLibDefaultLibraryName name ZLibcompressBoundDefaultExportName;
function uncompress;           external ZLibDefaultLibraryName name ZLibuncompressDefaultExportName;

// Checksums
function adler32;              external ZLibDefaultLibraryName name ZLibadler32DefaultExportName;
function crc32;                external ZLibDefaultLibraryName name ZLibcrc32DefaultExportName;

function zError;               external ZLibDefaultLibraryName name ZLibzErrorDefaultExportName;
function inflateSyncPoint;     external ZLibDefaultLibraryName name ZLibinflateSyncPointDefaultExportName;
function get_crc_table;        external ZLibDefaultLibraryName name ZLibget_crc_tableDefaultExportName;
{$ENDIF ZLIB_LINKDLL}

{$ENDIF ~ZLIB_RTL}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

