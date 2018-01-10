{*************************************************************************************************
*  ZLibEx.pas                                                                                    *
*                                                                                                *
*  copyright (c) 2000-2013 base2 technologies                                                    *
*  copyright (c) 1995-2002 Borland Software Corporation                                          *
*                                                                                                *
*  revision history                                                                              *
*    2012.05.21  updated for win64 (delphi xe2)                                                  *
*                added NativeInt type for delphi 2007-                                           *
*                added NativeUInt type for delphi 2007-                                          *
*    2011.07.21  fixed routines to validate size before calling Move                             *
*    2010.07.01  hide overloaded Z*String* routines for delphi 5                                 *
*    2010.05.02  added ZDelfateEx and ZInflateEx                                                 *
*    2010.04.20  added TZ*Buffer classes                                                         *
*    2010.04.15  moved core zlib routines to separate unit (ZLibExApi.pas)                       *
*                added ZDeflate* and ZInflate*                                                   *
*    2010.04.14  fixed ZInternalCompress loops                                                   *
*                fixed ZInternalDecompress loops                                                 *
*                updated ZInternalCompressStream loops                                           *
*                updated ZInternalDecompressStream loops                                         *
*    2010.01.27  updated for delphi 2010                                                         *
*    2009.04.14  added overloaded string routines for AnsiString and UnicodeString               *
*    2009.01.28  updated for delphi 2009 String (UnicodeString)                                  *
*    2008.05.15  added TStreamPos type for Stream.Position variants                              *
*                added TCustomZStream.Stream* methods                                            *
*    2007.08.17  modified TZCompressionStream.Write to use Write instead of WriteBuffer          *
*    2007.03.15  moved gzip routines to separate unit (ZLibExGZ.pas)                             *
*    2006.10.07  fixed EZLibError constructor for c++ builder compatibility                      *
*    2006.03.28  moved Z_DEFLATED to interface section                                           *
*                added custom compression levels zcLevel1 thru zcLevel9                          *
*    2006.03.27  added ZCompressStreamWeb                                                        *
*    2006.03.24  added ZAdler32 and ZCrc32                                                       *
*    2005.11.29  changed FStreamPos to Int64 for delphi 6+                                       *
*    2005.03.04  modified ZInternalCompressStream loops                                          *
*                modified ZInternalDecompressStream loops                                        *
*    2005.02.07  fixed ZInternalCompressStream loop conditions                                   *
*                fixed ZInternalDecompressStream loop conditions                                 *
*    2005.01.11  added ZCompressStrWeb                                                           *
*    2003.04.14  added ZCompress2 and ZDecompress2                                               *
*                added ZCompressStr2 and ZDecompressStr2                                         *
*                added ZCompressStream2 and ZDecompressStream2                                   *
*                added overloaded T*Stream constructors to support InflateInit2                  *
*                  and DeflateInit2                                                              *
*                fixed ZDecompressStream to use ZDecompressCheck instead of ZCompressCheck       *
*    2001.11.27  enhanced TZDecompressionStream.Read to adjust source stream position upon end   *
*                  of compression data                                                           *
*                fixed endless loop in TZDecompressionStream.Read when destination count was     *
*                  greater than uncompressed data                                                *
*    2001.10.26  renamed unit to integrate "nicely" with delphi 6                                *
*    2000.11.24  added soFromEnd condition to TZDecompressionStream.Seek                         *
*                added ZCompressStream and ZDecompressStream                                     *
*    2000.06.13  optimized, fixed, rewrote, and enhanced the zlib.pas unit included on the       *
*                  delphi cd (zlib version 1.1.3)                                                *
*                                                                                                *
*  acknowledgments                                                                               *
*    erik turner                                                                                 *
*      2001.10.26  Z*Stream routines                                                             *
*                                                                                                *
*    david bennion                                                                               *
*      2001.11.27  finding the nasty little endless loop quirk with the                          *
*                    TZDecompressionStream.Read method                                           *
*                                                                                                *
*    luigi sandon                                                                                *
*      2005.02.07  pointing out the missing loop condition (Z_STREAM_END) in                     *
*                    ZInternalCompressStream and ZInternalDecompressStream                       *
*                                                                                                *
*    ferry van genderen                                                                          *
*      2005.03.04  assisting me fine tune and beta test ZInternalCompressStream and              *
*                    ZInternalDecompressStream                                                   *
*                                                                                                *
*    j. rathlev                                                                                  *
*      2005.11.28  pointing out the FStreamPos and TStream.Position type inconsistency           *
*                                                                                                *
*    anders johansen                                                                             *
*      2006.10.07  pointing out the ELibError constructor incompatibility with c++ builder       *
*                                                                                                *
*    marcin szafranski                                                                           *
*      2009.01.28  beta testing the delphi 2009 changes                                          *
*                                                                                                *
*    iztok kacin                                                                                 *
*      2009.04.14  assisting me design and further improve support for delphi 2009               *
*                                                                                                *
*    oleg matrozov                                                                               *
*      2010.04.14  pointing out the missing loop condition (avail_in > 0) in ZInternalCompress   *
*                    and ZInternalDecompress                                                     *
*      2010.04.20  prototyping and assisting with the TZ*Buffer classes                          *
*                                                                                                *
*    edward koo                                                                                  *
*      2010.07.01  pointing out the delphi 5 incompatibility with the overloaded Z*String*       *
*                    routines                                                                    *
*                                                                                                *
*    egron elbra                                                                                 *
*      2011.07.20  pointing out the range exception when moving empty strings                    *
*                                                                                                *
*    marian pascalau                                                                             *
*      2012.05.21  providing their win64 modifications                                           *
*                                                                                                *
*  donations                                                                                     *
*    2011.05.06  farshad mohajeri                                                                *
*    2012.06.07  marat safin                                                                     *
*    2012.12.14  moacir schmidt                                                                  *
*    2013.05.23  roman ganz                                                                      *
*************************************************************************************************}

unit ZLibEx;

interface

{$I ZLibEx.inc}

uses
  SysUtils, Classes, ZLibExApi;

type

{$ifndef UNICODE}

  RawByteString = AnsiString;

  UnicodeString = WideString;
  UnicodeChar = WideChar;

{$else ifdef Version2010Plus}

  UnicodeChar = WideChar;

{$endif}

{$ifndef Version2009Plus}

  NativeInt = Integer;
  NativeUInt = Cardinal;

{$endif}

  TStreamPos = {$ifdef Version6Plus} Int64 {$else} Longint {$endif};

  TZCompressionLevel = (
    zcNone,
    zcFastest,
    zcDefault,
    zcMax,
    zcLevel1,
    zcLevel2,
    zcLevel3,
    zcLevel4,
    zcLevel5,
    zcLevel6,
    zcLevel7,
    zcLevel8,
    zcLevel9
  );

  TZStrategy = (
    zsDefault,
    zsFiltered,
    zsHuffman,
    zsRLE,
    zsFixed
  );

  TZError = (
    zeError,
    zeStreamError,
    zeDataError,
    zeMemoryError,
    zeBufferError,
    zeVersionError
  );

  TZFlush = (
    zfNoFlush,
    zfPartialFlush,
    zfSyncFlush,
    zfFullFlush,
    zfFinish,
    zfBlock,
    zfTrees
  );

const
  ZLevels: Array [TZCompressionLevel] of Integer = (
    Z_NO_COMPRESSION,       // zcNone
    Z_BEST_SPEED,           // zcFastest
    Z_DEFAULT_COMPRESSION,  // zcDefault
    Z_BEST_COMPRESSION,     // zcMax
    1,                      // zcLevel1
    2,                      // zcLevel2
    3,                      // zcLevel3
    4,                      // zcLevel4
    5,                      // zcLevel5
    6,                      // zcLevel6
    7,                      // zcLevel7
    8,                      // zcLevel8
    9                       // zcLevel9
  );

  ZStrategies: Array [TZStrategy] of Integer = (
    Z_DEFAULT_STRATEGY,     // zsDefault
    Z_FILTERED,             // zsFiltered
    Z_HUFFMAN_ONLY,         // zsHuffman
    Z_RLE,                  // zsRLE
    Z_FIXED                 // zsFixed
  );

  ZErrors: Array [TZError] of Integer = (
    Z_ERRNO,                // zeError
    Z_STREAM_ERROR,         // zeStreamError
    Z_DATA_ERROR,           // zeDataError
    Z_MEM_ERROR,            // zeMemoryError
    Z_BUF_ERROR,            // zeBufferError
    Z_VERSION_ERROR         // zeVersionError
  );

  ZFlushes: Array [TZFlush] of Integer = (
    Z_NO_FLUSH,             // zfNoFlush
    Z_PARTIAL_FLUSH,        // zfPartialFlush
    Z_SYNC_FLUSH,           // zfSyncFlush
    Z_FULL_FLUSH,           // zfFullFlush
    Z_FINISH,               // zfFinish
    Z_BLOCK,                // zfBlock
    Z_TREES                 // zfTrees
  );

type
  {** TZ*Function *******************************************************************************}

  TZReadFunction = function (param: Pointer; var buffer;
    size: Integer): Integer;

  TZWriteFunction = function (param: Pointer; const buffer;
    size: Integer): Integer;

  {** TZInformation *****************************************************************************}

  TZInformation = packed record
    CompressedFlags  : Longint;
    CompressedSize   : TStreamPos;
    CompressedCrc    : Longint;
    CompressedAdler  : Longint;

    UncompressedFlags: Longint;
    UncompressedSize : TStreamPos;
    UncompressedCrc  : Longint;
    UncompressedAdler: Longint;
  end;

  {** TCustomZStream ****************************************************************************}

  TCustomZStream = class(TStream)
  private
    FStream    : TStream;
    FStreamPos : TStreamPos;
    FOnProgress: TNotifyEvent;

    FZStream   : TZStreamRec;
    FBuffer    : Array [Word] of Byte;

    function  GetStreamPosition: TStreamPos;
    procedure SetStreamPosition(value: TStreamPos);
  protected
    constructor Create(stream: TStream);

    function  StreamRead(var buffer; count: Longint): Longint;
    function  StreamWrite(const buffer; count: Longint): Longint;
    function  StreamSeek(offset: Longint; origin: Word): Longint;

    procedure StreamReadBuffer(var buffer; count: Longint);
    procedure StreamWriteBuffer(const buffer; count: Longint);

    procedure DoProgress; dynamic;

    property StreamPosition: TStreamPos read GetStreamPosition write SetStreamPosition;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

  {** TZCompressionStream ***********************************************************************}

  TZCompressionStream = class(TCustomZStream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(dest: TStream;
      compressionLevel: TZCompressionLevel = zcDefault); overload;

    constructor Create(dest: TStream; compressionLevel: TZCompressionLevel;
      windowBits, memLevel: Integer; strategy: TZStrategy); overload;

    destructor  Destroy; override;

    function  Read(var buffer; count: Longint): Longint; override;
    function  Write(const buffer; count: Longint): Longint; override;
    function  Seek(offset: Longint; origin: Word): Longint; override;

    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;

  {** TZDecompressionStream *********************************************************************}

  TZDecompressionStream = class(TCustomZStream)
  public
    constructor Create(source: TStream); overload;
    constructor Create(source: TStream; windowBits: Integer); overload;

    destructor  Destroy; override;

    function  Read(var buffer; count: Longint): Longint; override;
    function  Write(const buffer; count: Longint): Longint; override;
    function  Seek(offset: Longint; origin: Word): Longint; override;

    property OnProgress;
  end;

  {** TZCustomBuffer ****************************************************************************}

  TZCustomBuffer = class(TObject)
  private
    FBuffer        : Pointer;
    FBufferCapacity: Integer;
    FBufferSize    : Integer;
  protected
    FZStream: TZStreamRec;

    procedure BufferWrite(const buffer: Pointer; size: Integer);
    procedure BufferRead(var buffer: Pointer; size: Integer);

    procedure BufferCapacity(capacity: Integer);

    property BufferSize: Integer read FBufferSize;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear; virtual;

    procedure Flush(flush: TZFlush); virtual;

    function  Write(const buffer: Pointer; size: Integer): Integer; overload;
      virtual; abstract;

    function  Write(const s: AnsiString): Integer; overload;

    function  Read(var buffer: Pointer; size: Integer): Integer; overload;
    function  Read(var s: AnsiString): Integer; overload;
  end;

  {** TZCompressionBuffer ***********************************************************************}

  TZCompressionBuffer = class(TZCustomBuffer)
  public
    constructor Create(level: TZCompressionLevel = zcDefault); overload;
    constructor Create(level: TZCompressionLevel;
      windowBits, memLevel: Integer; strategy: TZStrategy); overload;

    destructor  Destroy; override;

    procedure Clear; override;

    procedure Flush(flush: TZFlush); override;

    function  Write(const buffer: Pointer; size: Integer): Integer;
      override;
  end;

  {** TZDecompressionBuffer *********************************************************************}

  TZDecompressionBuffer = class(TZCustomBuffer)
  public
    constructor Create; overload;
    constructor Create(windowBits: Integer); overload;

    destructor  Destroy; override;

    procedure Clear; override;

    function  Write(const buffer: Pointer; size: Integer): Integer; override;
  end;

{** zlib deflate routines ***********************************************************************}

function  ZDeflateInit(var stream: TZStreamRec;
  level: TZCompressionLevel): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

function  ZDeflateInit2(var stream: TZStreamRec;
  level: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

function  ZDeflate(var stream: TZStreamRec; flush: TZFlush): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

function  ZDeflateEnd(var stream: TZStreamRec): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

function  ZDeflateReset(var stream: TZStreamRec): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

{** zlib inflate routines ***********************************************************************}

function  ZInflateInit(var stream: TZStreamRec): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

function  ZInflateInit2(var stream: TZStreamRec;
  windowBits: Integer): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

function  ZInflate(var stream: TZStreamRec; flush: TZFlush): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

function  ZInflateEnd(var stream: TZStreamRec): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

function  ZInflateReset(var stream: TZStreamRec): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

{** zlib checksum routines **********************************************************************}

function  ZAdler32(adler: Longint; const buffer; size: Integer): Longint;
  {$ifdef Version2005Plus} inline; {$endif}

function  ZCrc32(crc: Longint; const buffer; size: Integer): Longint;
  {$ifdef Version2005Plus} inline; {$endif}

{** zlib custom routines ************************************************************************}

procedure ZDeflateEx(var stream: TZStreamRec; param: Pointer;
  read: TZReadFunction; write: TZWriteFunction; flush: TZFlush);

procedure ZInflateEx(var stream: TZStreamRec; param: Pointer;
  read: TZReadFunction; write: TZWriteFunction; flush: TZFlush);

{*************************************************************************************************
*  ZCompress                                                                                     *
*                                                                                                *
*  pre-conditions                                                                                *
*    inBuffer  = pointer to uncompressed data                                                    *
*    inSize    = size of inBuffer (bytes)                                                        *
*    outBuffer = pointer (unallocated)                                                           *
*    level     = compression level                                                               *
*                                                                                                *
*  post-conditions                                                                               *
*    outBuffer = pointer to compressed data (allocated)                                          *
*    outSize   = size of outBuffer (bytes)                                                       *
*************************************************************************************************}

procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TZCompressionLevel = zcDefault);

{*************************************************************************************************
*  ZCompress2                                                                                    *
*                                                                                                *
*  pre-conditions                                                                                *
*    inBuffer   = pointer to uncompressed data                                                   *
*    inSize     = size of inBuffer (bytes)                                                       *
*    outBuffer  = pointer (unallocated)                                                          *
*    level      = compression level                                                              *
*    method     = compression method                                                             *
*    windowBits = window bits                                                                    *
*    memLevel   = memory level                                                                   *
*    strategy   = compression strategy                                                           *
*                                                                                                *
*  post-conditions                                                                               *
*    outBuffer = pointer to compressed data (allocated)                                          *
*    outSize   = size of outBuffer (bytes)                                                       *
*************************************************************************************************}

procedure ZCompress2(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; level: TZCompressionLevel;
  windowBits, memLevel: Integer; strategy: TZStrategy);

{*************************************************************************************************
*  ZDecompress                                                                                   *
*                                                                                                *
*  pre-conditions                                                                                *
*    inBuffer    = pointer to compressed data                                                    *
*    inSize      = size of inBuffer (bytes)                                                      *
*    outBuffer   = pointer (unallocated)                                                         *
*    outEstimate = estimated size of uncompressed data (bytes)                                   *
*                                                                                                *
*  post-conditions                                                                               *
*    outBuffer = pointer to decompressed data (allocated)                                        *
*    outSize   = size of outBuffer (bytes)                                                       *
*************************************************************************************************}

procedure ZDecompress(const inBuffer: Pointer; inSize: Integer;
 out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer = 0);

{*************************************************************************************************
*  ZDecompress2                                                                                  *
*                                                                                                *
*  pre-conditions                                                                                *
*    inBuffer    = pointer to compressed data                                                    *
*    inSize      = size of inBuffer (bytes)                                                      *
*    outBuffer   = pointer (unallocated)                                                         *
*    windowBits  = window bits                                                                   *
*    outEstimate = estimated size of uncompressed data (bytes)                                   *
*                                                                                                *
*  post-conditions                                                                               *
*    outBuffer = pointer to decompressed data (allocated)                                        *
*    outSize   = size of outBuffer (bytes)                                                       *
*************************************************************************************************}

procedure ZDecompress2(const inBuffer: Pointer; inSize: Integer;
 out outBuffer: Pointer; out outSize: Integer; windowBits: Integer;
 outEstimate: Integer = 0);

{** string routines *****************************************************************************}

{*************************************************************************************************
*  ZCompressStr                                                                                  *
*                                                                                                *
*  pre-conditions                                                                                *
*    s     = uncompressed data string                                                            *
*    level = compression level                                                                   *
*                                                                                                *
*  return                                                                                        *
*    compressed data string                                                                      *
*************************************************************************************************}

function  ZCompressStr(const s: AnsiString;
  level: TZCompressionLevel = zcDefault): RawByteString;

procedure ZCompressString(var result: RawByteString; const s: AnsiString;
  level: TZCompressionLevel = zcDefault); overload;

{$ifdef Version6Plus}
procedure ZCompressString(var result: RawByteString; const s: UnicodeString;
  level: TZCompressionLevel = zcDefault); overload;
{$endif}

{*************************************************************************************************
*  ZCompressStrEx                                                                                *
*                                                                                                *
*  pre-conditions                                                                                *
*    s     = uncompressed data string                                                            *
*    level = compression level                                                                   *
*                                                                                                *
*  return                                                                                        *
*    compressed data string with 4 byte (integer) header indicating                              *
*    original uncompressed data length                                                           *
*************************************************************************************************}

function  ZCompressStrEx(const s: AnsiString;
  level: TZCompressionLevel = zcDefault): RawByteString;

procedure ZCompressStringEx(var result: RawByteString; const s: AnsiString;
  level: TZCompressionLevel = zcDefault); overload;

{$ifdef Version6Plus}
procedure ZCompressStringEx(var result: RawByteString; const s: UnicodeString;
  level: TZCompressionLevel = zcDefault); overload;
{$endif}

{*************************************************************************************************
*  ZCompressStr2                                                                                 *
*                                                                                                *
*  pre-conditions                                                                                *
*    s          = uncompressed data string                                                       *
*    level      = compression level                                                              *
*    windowBits = window bits                                                                    *
*    memLevel   = memory level                                                                   *
*    strategy   = compression strategy                                                           *
*                                                                                                *
*  return                                                                                        *
*    compressed data string                                                                      *
*************************************************************************************************}

function  ZCompressStr2(const s: AnsiString; level: TZCompressionLevel;
  windowBits, memLevel: Integer; strategy: TZStrategy): RawByteString;

procedure ZCompressString2(var result: RawByteString; const s: AnsiString;
  level: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy); overload;

{$ifdef Version6Plus}
procedure ZCompressString2(var result: RawByteString; const s: UnicodeString;
  level: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy); overload;
{$endif}

{*************************************************************************************************
*  ZCompressStrWeb                                                                               *
*                                                                                                *
*  pre-conditions                                                                                *
*    s = uncompressed data string                                                                *
*                                                                                                *
*  return                                                                                        *
*    compressed data string                                                                      *
*************************************************************************************************}

function  ZCompressStrWeb(const s: AnsiString): RawByteString;

procedure ZCompressStringWeb(var result: RawByteString; const s: AnsiString);
  overload;

{$ifdef Version6Plus}
procedure  ZCompressStringWeb(var result: RawByteString;
  const s: UnicodeString); overload;
{$endif}

{*************************************************************************************************
*  ZDecompressStr                                                                                *
*                                                                                                *
*  pre-conditions                                                                                *
*    s = compressed data string                                                                  *
*                                                                                                *
*  return                                                                                        *
*    uncompressed data string                                                                    *
*************************************************************************************************}

function  ZDecompressStr(const s: RawByteString): AnsiString;

procedure ZDecompressString(var result: AnsiString; const s: RawByteString);
  overload;

{$ifdef Version6Plus}
procedure ZDecompressString(var result: UnicodeString;
  const s: RawByteString); overload;
{$endif}

{*************************************************************************************************
*  ZDecompressStrEx                                                                              *
*                                                                                                *
*  pre-conditions                                                                                *
*    s = compressed data string with 4 byte (integer) header indicating                          *
*        original uncompressed data length                                                       *
*                                                                                                *
*  return                                                                                        *
*    uncompressed data string                                                                    *
*************************************************************************************************}

function  ZDecompressStrEx(const s: RawByteString): AnsiString;

procedure ZDecompressStringEx(var result: AnsiString; const s: RawByteString);
  overload;

{$ifdef Version6Plus}
procedure ZDecompressStringEx(var result: UnicodeString;
  const s: RawByteString); overload;
{$endif}

{*************************************************************************************************
*  ZDecompressStr2                                                                               *
*                                                                                                *
*  pre-conditions                                                                                *
*    s          = compressed data string                                                         *
*    windowBits = window bits                                                                    *
*                                                                                                *
*  return                                                                                        *
*    uncompressed data string                                                                    *
*************************************************************************************************}

function  ZDecompressStr2(const s: RawByteString;
  windowBits: Integer): AnsiString;

procedure ZDecompressString2(var result: AnsiString; const s: RawByteString;
  windowBits: Integer); overload;

{$ifdef Version6Plus}
procedure ZDecompressString2(var result: UnicodeString;
  const s: RawByteString; windowBits: Integer); overload;
{$endif}

{** stream routines *****************************************************************************}

procedure ZCompressStream(inStream, outStream: TStream;
  level: TZCompressionLevel = zcDefault);

procedure ZCompressStream2(inStream, outStream: TStream;
  level: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy);

procedure ZCompressStreamWeb(inStream, outStream: TStream);

procedure ZDecompressStream(inStream, outStream: TStream);

procedure ZDecompressStream2(inStream, outStream: TStream;
  windowBits: Integer);

{************************************************************************************************}

type
  EZLibErrorClass = class of EZlibError;

  EZLibError = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(code: Integer; const dummy: String = ''); overload;
    constructor Create(error: TZError; const dummy: String = ''); overload;

    property ErrorCode: Integer read FErrorCode write FErrorCode;
  end;

  EZCompressionError = class(EZLibError);
  EZDecompressionError = class(EZLibError);

implementation

const
  SZInvalid = 'Invalid ZStream operation!';

{************************************************************************************************}

function ZCompressCheck(code: Integer): Integer;
begin
  result := code;

  if code < 0 then
  begin
    raise EZCompressionError.Create(code);
  end;
end;

function ZDecompressCheck(code: Integer; raiseBufferError: Boolean = True): Integer;
begin
  Result := code;

  if code < 0 then
  begin
    if (code <> Z_BUF_ERROR) or raiseBufferError then
    begin
      raise EZDecompressionError.Create(code);
    end;
  end;
end;

{** zlib deflate routines ***********************************************************************}

function ZDeflateInit(var stream: TZStreamRec;
  level: TZCompressionLevel): Integer;
begin
  result := deflateInit_(stream, ZLevels[level], ZLIB_VERSION,
    SizeOf(TZStreamRec));
end;

function ZDeflateInit2(var stream: TZStreamRec;
  level: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy): Integer;
begin
  result := deflateInit2_(stream, ZLevels[level], Z_DEFLATED, windowBits,
    memLevel, ZStrategies[strategy], ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function ZDeflate(var stream: TZStreamRec; flush: TZFlush): Integer;
begin
  result := deflate(stream, ZFlushes[flush]);
end;

function ZDeflateEnd(var stream: TZStreamRec): Integer;
begin
  result := deflateEnd(stream);
end;

function ZDeflateReset(var stream: TZStreamRec): Integer;
begin
  result := deflateReset(stream);
end;

{** zlib inflate routines ***********************************************************************}

function ZInflateInit(var stream: TZStreamRec): Integer;
begin
  result := inflateInit_(stream, ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function ZInflateInit2(var stream: TZStreamRec;
  windowBits: Integer): Integer;
begin
  result := inflateInit2_(stream, windowBits, ZLIB_VERSION,
    SizeOf(TZStreamRec));
end;

function ZInflate(var stream: TZStreamRec; flush: TZFlush): Integer;
begin
  result := inflate(stream, ZFlushes[flush]);
end;

function ZInflateEnd(var stream: TZStreamRec): Integer;
begin
  result := inflateEnd(stream);
end;

function ZInflateReset(var stream: TZStreamRec): Integer;
begin
  result := inflateReset(stream);
end;

{** zlib checksum routines **********************************************************************}

function ZAdler32(adler: Longint; const buffer; size: Integer): Longint;
begin
  result := adler32(adler,buffer,size);
end;

function ZCrc32(crc: Longint; const buffer; size: Integer): Longint;
begin
  result := crc32(crc,buffer,size);
end;

{** zlib extended routines **********************************************************************}

procedure ZDeflateEx(var stream: TZStreamRec; param: Pointer;
  read: TZReadFunction; write: TZWriteFunction; flush: TZFlush);
const
  bufferSize = 8192;
var
  zresult    : Integer;
  readBuffer : Array [0..bufferSize - 1] of Byte;
  writeBuffer: Array [0..bufferSize - 1] of Byte;
  writeSize  : Integer;
  flushEx    : TZFlush;
begin
  if Assigned(read) then
  begin
    stream.avail_in := read(param, readBuffer, bufferSize);
  end
  else stream.avail_in := 0;

  repeat
    stream.next_in := @readBuffer;

    repeat
      stream.avail_out := bufferSize;
      stream.next_out := @writeBuffer;

      flushEx := flush;

      if (flushEx = zfFinish) and (stream.avail_in = bufferSize) then
      begin
        flushEx := zfNoFlush;
      end;

      zresult := ZCompressCheck(ZDeflate(stream, flushEx));

      writeSize := bufferSize - stream.avail_out;

      write(param, writeBuffer, writeSize);
    until stream.avail_out > 0;

    //assert: stream.avail_in = 0

    if (zresult <> Z_STREAM_END) and Assigned(read) then
    begin
      stream.avail_in := read(param, readBuffer, bufferSize);
    end;
  until (stream.avail_in = 0) and (flush = flushEx);
end;

procedure ZInflateEx(var stream: TZStreamRec; param: Pointer;
  read: TZReadFunction; write: TZWriteFunction; flush: TZFlush);
const
  bufferSize = 8192;
var
  zresult    : Integer;
  readBuffer : Array [0..bufferSize - 1] of Byte;
  writeBuffer: Array [0..bufferSize - 1] of Byte;
  writeSize  : Integer;
begin
  if Assigned(read) then
  begin
    stream.avail_in := read(param, readBuffer, bufferSize);
  end
  else stream.avail_in := 0;

  zresult := Z_OK;

  while (zresult <> Z_STREAM_END) and (stream.avail_in > 0) do
  begin
    stream.next_in := @readBuffer;

    repeat
      stream.avail_out := bufferSize;
      stream.next_out := @writeBuffer;

      zresult := ZDecompressCheck(ZInflate(stream, flush), False);

      writeSize := bufferSize - stream.avail_out;

      write(param, writeBuffer, writeSize);
    until stream.avail_out > 0;

    if (zresult <> Z_STREAM_END) and Assigned(read) then
    begin
      stream.avail_in := read(param, readBuffer, bufferSize);
    end;
  end;
end;

{** private buffer routines *********************************************************************}

type
  PZBufferParam = ^TZBufferParam;
  TZBufferParam = packed record
    InBuffer   : Pointer;
    InPosition : Integer;
    InSize     : Integer;
    OutBuffer  : Pointer;
    OutPosition: Integer;
    OutSize    : Integer;
  end;

function ZBufferRead(p: Pointer; var buffer; size: Integer): Integer;
var
  param: PZBufferParam;
begin
  param := PZBufferParam(p);

  result := param^.InSize - param^.InPosition;
  if result > size then result := size;

  Move(Pointer(Integer(param^.InBuffer) + param^.InPosition)^, buffer, result);

  Inc(param^.InPosition, result);
end;

function ZBufferWrite(p: Pointer; const buffer; size: Integer): Integer;
var
  param: PZBufferParam;
begin
  param := PZBufferParam(p);

  if param^.OutPosition + size > param^.OutSize then
  begin
    param^.OutSize := param^.OutPosition + size;

    ReallocMem(Pointer(param^.OutBuffer), param^.OutSize);
  end;

  Move(buffer, Pointer(Integer(param^.OutBuffer) + param^.OutPosition)^, size);

  Inc(param^.OutPosition, size);

  result := size;
end;

procedure ZInternalCompressEx(var zstream: TZStreamRec; const inBuffer: Pointer;
  inSize: Integer; out outBuffer: Pointer; out outSize: Integer);
var
  param: TZBufferParam;
begin
  FillChar(param, SizeOf(TZBufferParam), 0);

  outBuffer := Nil;
  outSize := 0;

  param.InBuffer := inBuffer;
  param.InSize := inSize;

  try
    ZDeflateEx(zstream, @param, @ZBufferRead, @ZBufferWrite, zfFinish);

    ZCompressCheck(ZDeflateEnd(zstream));

    outBuffer := param.OutBuffer;
    outSize := param.OutSize;
  except
    FreeMem(param.OutBuffer);

    raise;
  end;
end;

procedure ZInternalDecompressEx(zstream: TZStreamRec; const inBuffer: Pointer;
  inSize: Integer; out outBuffer: Pointer; out outSize: Integer;
  outEstimate: Integer);
var
  param: TZBufferParam;
begin
  FillChar(param, SizeOf(TZBufferParam), 0);

  outBuffer := Nil;
  outSize := 0;

  param.InBuffer := inBuffer;
  param.InSize := inSize;

  if outEstimate > 0 then
  begin
    GetMem(param.OutBuffer, outEstimate);

    param.OutSize := outEstimate;
  end;

  try
    ZInflateEx(zstream, @param, @ZBufferRead, @ZBufferWrite, zfNoFlush);

    ZDecompressCheck(ZInflateEnd(zstream));

    outBuffer := param.OutBuffer;
    outSize := param.OutSize;
  except
    FreeMem(param.OutBuffer);

    raise;
  end;
end;

procedure ZInternalCompress(var zstream: TZStreamRec; const inBuffer: Pointer;
  inSize: Integer; out outBuffer: Pointer; out outSize: Integer);
const
  delta = 256;
var
  zresult: Integer;
begin
  outSize := ((inSize + (inSize div 10) + 12) + 255) and not 255;

  outBuffer := Nil;

  try
    try
      zstream.next_in := inBuffer;
      zstream.avail_in := inSize;

      repeat
        ReallocMem(outBuffer, outSize);

        zstream.next_out := PByte(NativeUInt(outBuffer) + zstream.total_out);
        zstream.avail_out := NativeUInt(outSize) - zstream.total_out;

        zresult := ZCompressCheck(ZDeflate(zstream, zfNoFlush));

        Inc(outSize, delta);
      until (zresult = Z_STREAM_END) or (zstream.avail_in = 0);

      while zresult <> Z_STREAM_END do
      begin
        ReallocMem(outBuffer, outSize);

        zstream.next_out := PByte(NativeUInt(outBuffer) + zstream.total_out);
        zstream.avail_out := NativeUInt(outSize) - zstream.total_out;

        zresult := ZCompressCheck(ZDeflate(zstream, zfFinish));

        Inc(outSize, delta);
      end;
    finally
      ZCompressCheck(ZDeflateEnd(zstream));
    end;

    ReallocMem(outBuffer, zstream.total_out);

    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

procedure ZInternalDecompress(zstream: TZStreamRec; const inBuffer: Pointer;
  inSize: Integer; out outBuffer: Pointer; out outSize: Integer;
  outEstimate: Integer);
var
  zresult: Integer;
  delta  : Integer;
begin
  delta := (inSize + 255) and not 255;

  if outEstimate = 0 then outSize := delta
  else outSize := outEstimate;

  outBuffer := Nil;

  try
    try
      zresult := Z_OK;

      zstream.avail_in := inSize;
      zstream.next_in := inBuffer;

      while (zresult <> Z_STREAM_END) and (zstream.avail_in > 0) do
      begin
        repeat
          ReallocMem(outBuffer, outSize);

          zstream.next_out := PByte(NativeUInt(outBuffer) + zstream.total_out);
          zstream.avail_out := NativeUInt(outSize) - zstream.total_out;

          zresult := ZDecompressCheck(ZInflate(zstream, zfNoFlush), False);

          Inc(outSize, delta);
        until (zresult = Z_STREAM_END) or (zstream.avail_out > 0);
      end;
    finally
      ZDecompressCheck(ZInflateEnd(zstream));
    end;

    ReallocMem(outBuffer, zstream.total_out);

    outSize := zstream.total_out;
  except
    if Assigned(outBuffer) then FreeMem(outBuffer);

    raise;
  end;
end;

{** buffer routines *****************************************************************************}

procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TZCompressionLevel);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  ZCompressCheck(ZDeflateInit(zstream, level));

  ZInternalCompress(zstream, inBuffer, inSize, outBuffer, outSize);
end;

procedure ZCompress2(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; level: TZCompressionLevel;
  windowBits, memLevel: Integer; strategy: TZStrategy);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  ZCompressCheck(ZDeflateInit2(zstream, level, windowBits, memLevel,
    strategy));

  ZInternalCompress(zstream, inBuffer, inSize, outBuffer, outSize);
end;

procedure ZDecompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  ZDecompressCheck(ZInflateInit(zstream));

  ZInternalDecompress(zstream, inBuffer, inSize, outBuffer, outSize,
    outEstimate);
end;

procedure ZDecompress2(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; windowBits: Integer;
  outEstimate: Integer);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  ZDecompressCheck(ZInflateInit2(zstream, windowBits));

  ZInternalDecompress(zstream, inBuffer, inSize, outBuffer, outSize,
    outEstimate);
end;

{** string routines *****************************************************************************}

function ZCompressStr(const s: AnsiString;
  level: TZCompressionLevel): RawByteString;
begin
  ZCompressString(result, s, level);
end;

procedure ZCompressString(var result: RawByteString; const s: AnsiString;
  level: TZCompressionLevel);
var
  buffer: Pointer;
  size  : Integer;
begin
  ZCompress(Pointer(s), Length(s), buffer, size, level);

  SetLength(result, size);

  if size > 0 then
  begin
    Move(buffer^, result[1], size);
  end;

  FreeMem(buffer);
end;

{$ifdef Version6Plus}
procedure ZCompressString(var result: RawByteString; const s: UnicodeString;
  level: TZCompressionLevel);
var
  buffer: Pointer;
  size  : Integer;
begin
  ZCompress(Pointer(s), Length(s) * SizeOf(UnicodeChar), buffer, size, level);

  SetLength(result, size);

  if size > 0 then
  begin
    Move(buffer^, result[1], size);
  end;

  FreeMem(buffer);
end;
{$endif}

function ZCompressStrEx(const s: AnsiString;
  level: TZCompressionLevel): RawByteString;
begin
  ZCompressStringEx(result, s, level);
end;

procedure ZCompressStringEx(var result: RawByteString; const s: AnsiString;
  level: TZCompressionLevel);
var
  buffer: Pointer;
  size  : Integer;
begin
  ZCompress(Pointer(s), Length(s), buffer, size, level);

  SetLength(result, size + SizeOf(Integer));

  if size > 0 then
  begin
    Move(buffer^, result[1 + SizeOf(Integer)], size);
  end;

  size := Length(s);

  Move(size, result[1], SizeOf(Integer));

  FreeMem(buffer);
end;

{$ifdef Version6Plus}
procedure ZCompressStringEx(var result: RawByteString; const s: UnicodeString;
  level: TZCompressionLevel);
var
  buffer: Pointer;
  size  : Integer;
begin
  ZCompress(Pointer(s), Length(s) * SizeOf(UnicodeChar), buffer, size, level);

  SetLength(result, size + SizeOf(Integer));

  if size > 0 then
  begin
    Move(buffer^, result[1 + SizeOf(Integer)], size);
  end;

  size := Length(s) * SizeOf(UnicodeChar);

  Move(size, result[1], SizeOf(Integer));

  FreeMem(buffer);
end;
{$endif}

function ZCompressStr2(const s: AnsiString; level: TZCompressionLevel;
  windowBits, memLevel: Integer; strategy: TZStrategy): RawByteString;
begin
  ZCompressString2(result, s, level, windowBits, memLevel, strategy);
end;

procedure ZCompressString2(var result: RawByteString; const s: AnsiString;
  level: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy);
var
  buffer: Pointer;
  size  : Integer;
begin
  ZCompress2(Pointer(s), Length(s), buffer, size, level, windowBits,
    memLevel, strategy);

  SetLength(result, size);

  if size > 0 then
  begin
    Move(buffer^, result[1], size);
  end;

  FreeMem(buffer);
end;

{$ifdef Version6Plus}
procedure ZCompressString2(var result: RawByteString; const s: UnicodeString;
  level: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy);
var
  buffer: Pointer;
  size  : Integer;
begin
  ZCompress2(Pointer(s), Length(s) * SizeOf(UnicodeChar), buffer, size,
    level, windowBits, memLevel, strategy);

  SetLength(result, size);

  if size > 0 then
  begin
    Move(buffer^, result[1], size);
  end;

  FreeMem(buffer);
end;
{$endif}

function ZCompressStrWeb(const s: AnsiString): RawByteString;
begin
  ZCompressStringWeb(result, s);
end;

procedure ZCompressStringWeb(var result: RawByteString; const s: AnsiString);
begin
  ZCompressString2(result, s, zcFastest, -15, 9, zsDefault);
end;

{$ifdef Version6Plus}
procedure ZCompressStringWeb(var result: RawBytestring;
  const s: UnicodeString);
begin
  ZCompressString2(result, s, zcFastest, -15, 9, zsDefault);
end;
{$endif}

function ZDecompressStr(const s: RawByteString): AnsiString;
begin
  ZDecompressString(result, s);
end;

procedure ZDecompressString(var result: AnsiString;
  const s: RawByteString);
var
  buffer: Pointer;
  size  : Integer;
begin
  ZDecompress(Pointer(s), Length(s), buffer, size);

  SetLength(result, size);

  if size > 0 then
  begin
    Move(buffer^, result[1], size);
  end;

  FreeMem(buffer);
end;

{$ifdef Version6Plus}
procedure ZDecompressString(var result: UnicodeString;
  const s: RawByteString);
var
  buffer: Pointer;
  size  : Integer;
begin
  ZDecompress(Pointer(s), Length(s), buffer, size);

  SetLength(result, size div SizeOf(UnicodeChar));

  if size > 0 then
  begin
    Move(buffer^, result[1], size);
  end;

  FreeMem(buffer);
end;
{$endif}

function ZDecompressStrEx(const s: RawByteString): AnsiString;
begin
  ZDecompressStringEx(result, s);
end;

procedure ZDecompressStringEx(var result: AnsiString; const s: RawByteString);
var
  buffer  : Pointer;
  size    : Integer;
  data    : AnsiString;
  dataSize: Integer;
begin
  Move(s[1], size, SizeOf(Integer));

  dataSize := Length(s) - SizeOf(Integer);

  SetLength(data, dataSize);

  if dataSize > 0 then
  begin
    Move(s[1 + SizeOf(Integer)], data[1], dataSize);

    ZDecompress(Pointer(data), dataSize, buffer, size, size);

    SetLength(result, size);

    if size > 0 then
    begin
      Move(buffer^, result[1], size);
    end;

    FreeMem(buffer);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

{$ifdef Version6Plus}
procedure ZDecompressStringEx(var result: UnicodeString;
  const s: RawByteString);
var
  buffer  : Pointer;
  size    : Integer;
  data    : AnsiString;
  dataSize: Integer;
begin
  Move(s[1], size, SizeOf(Integer));

  dataSize := Length(s) - SizeOf(Integer);

  if dataSize > 0 then
  begin
    SetLength(data, dataSize);

    Move(s[1 + SizeOf(Integer)], data[1], dataSize);

    ZDecompress(Pointer(data), dataSize, buffer, size, size);

    SetLength(result, size div SizeOf(UnicodeChar));

    if size > 0 then
    begin
      Move(buffer^, result[1], size);
    end;

    FreeMem(buffer);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;
{$endif}

function ZDecompressStr2(const s: RawByteString;
  windowBits: Integer): AnsiString;
begin
  ZDecompressString2(result, s, windowBits);
end;

procedure ZDecompressString2(var result: AnsiString; const s: RawByteString;
  windowBits: Integer);
var
  buffer: Pointer;
  size  : Integer;
begin
  ZDecompress2(Pointer(s), Length(s), buffer, size, windowBits);

  SetLength(result, size);

  if size > 0 then
  begin
    Move(buffer^, result[1], size);
  end;

  FreeMem(buffer);
end;

{$ifdef Version6Plus}
procedure ZDecompressString2(var result: UnicodeString;
  const s: RawByteString; windowBits: Integer);
var
  buffer: Pointer;
  size  : Integer;
begin
  ZDecompress2(Pointer(s), Length(s), buffer, size, windowBits);

  SetLength(result, size div SizeOf(UnicodeChar));

  if size > 0 then
  begin
    Move(buffer^, result[1], size);
  end;

  FreeMem(buffer);
end;
{$endif}

{** private stream routines *********************************************************************}

type
  PZStreamParam = ^TZStreamParam;
  TZStreamParam = packed record
    InStream   : TStream;
    OutStream  : TStream;
  end;

function ZStreamRead(p: Pointer; var buffer; size: Integer): Integer;
var
  param: PZStreamParam;
begin
  param := PZStreamParam(p);

  result := param^.InStream.Read(buffer, size);
end;

function ZStreamWrite(p: Pointer; const buffer; size: Integer): Integer;
var
  param: PZStreamParam;
begin
  param := PZStreamParam(p);

  result := param^.OutStream.Write(buffer, size);
end;

procedure ZInternalCompressStreamEx(zstream: TZStreamRec; inStream,
  outStream: TStream);
var
  param: TZStreamParam;
begin
  FillChar(param, SizeOf(TZStreamParam), 0);

  param.InStream := inStream;
  param.OutStream := outStream;

  ZDeflateEx(zstream, @param, @ZBufferRead, @ZBufferWrite, zfFinish);

  ZCompressCheck(ZDeflateEnd(zstream));
end;

procedure ZInternalDecompressStreamEx(zstream: TZStreamRec; inStream,
  outStream: TStream);
var
  param: TZStreamParam;
begin
  FillChar(param, SizeOf(TZStreamParam), 0);

  param.InStream := inStream;
  param.OutStream := outStream;

  ZInflateEx(zstream, @param, @ZBufferRead, @ZBufferWrite, zfNoFlush);

  ZDecompressCheck(ZInflateEnd(zstream));
end;

procedure ZInternalCompressStream(zstream: TZStreamRec; inStream,
  outStream: TStream);
const
  bufferSize = 32768;
var
  zresult  : Integer;
  inBuffer : Array [0..bufferSize - 1] of Byte;
  outBuffer: Array [0..bufferSize - 1] of Byte;
  outSize  : Integer;
begin
  zresult := Z_STREAM_END;

  zstream.avail_in := inStream.Read(inBuffer, bufferSize);

  while zstream.avail_in > 0 do
  begin
    zstream.next_in := @inBuffer;

    repeat
      zstream.next_out := @outBuffer;
      zstream.avail_out := bufferSize;

      zresult := ZCompressCheck(ZDeflate(zstream, zfNoFlush));

      outSize := bufferSize - zstream.avail_out;

      outStream.Write(outBuffer, outSize);
    until (zresult = Z_STREAM_END) or (zstream.avail_in = 0);

    zstream.avail_in := inStream.Read(inBuffer, bufferSize);
  end;

  while zresult <> Z_STREAM_END do
  begin
    zstream.next_out := @outBuffer;
    zstream.avail_out := bufferSize;

    zresult := ZCompressCheck(ZDeflate(zstream, zfFinish));

    outSize := bufferSize - zstream.avail_out;

    outStream.Write(outBuffer, outSize);
  end;

  ZCompressCheck(ZDeflateEnd(zstream));
end;

procedure ZInternalDecompressStream(zstream: TZStreamRec; inStream,
  outStream: TStream);
const
  bufferSize = 32768;
var
  zresult  : Integer;
  inBuffer : Array [0..bufferSize-1] of Byte;
  outBuffer: Array [0..bufferSize-1] of Byte;
  outSize  : Integer;
begin
  try
    zresult := Z_OK;

    zstream.avail_in := inStream.Read(inBuffer, bufferSize);

    while (zresult <> Z_STREAM_END) and (zstream.avail_in > 0) do
    begin
      zstream.next_in := @inBuffer;

      repeat
        zstream.next_out := @outBuffer;
        zstream.avail_out := bufferSize;

        zresult := ZDecompressCheck(ZInflate(zstream, zfNoFlush), False);

        outSize := bufferSize - zstream.avail_out;

        outStream.Write(outBuffer, outSize);
      until (zresult = Z_STREAM_END) or (zstream.avail_out > 0);

      if zstream.avail_in > 0 then
      begin
        inStream.Position := inStream.Position - zstream.avail_in;
      end;

      if zresult <> Z_STREAM_END then
      begin
        zstream.avail_in := inStream.Read(inBuffer, bufferSize);
      end;
    end;
  finally
    ZDecompressCheck(ZInflateEnd(zstream));
  end;
end;

{** stream routines *****************************************************************************}

procedure ZCompressStream(inStream, outStream: TStream;
  level: TZCompressionLevel);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  ZCompressCheck(ZDeflateInit(zstream, level));

  ZInternalCompressStream(zstream, inStream, outStream);
end;

procedure ZCompressStream2(inStream, outStream: TStream;
  level: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  ZCompressCheck(ZDeflateInit2(zstream, level, windowBits, memLevel,
    strategy));

  ZInternalCompressStream(zstream,inStream,outStream);
end;

procedure ZCompressStreamWeb(inStream, outStream: TStream);
begin
  ZCompressStream2(inStream, outStream, zcFastest, -15, 9, zsDefault);
end;

procedure ZDecompressStream(inStream, outStream: TStream);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  ZDecompressCheck(ZInflateInit(zstream));

  ZInternalDecompressStream(zstream, inStream, outStream);
end;

procedure ZDecompressStream2(inStream, outStream: TStream;
  windowBits: Integer);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  ZDecompressCheck(ZInflateInit2(zstream, windowBits));

  ZInternalDecompressStream(zstream, inStream, outStream);
end;

{** TCustomZStream ******************************************************************************}

constructor TCustomZStream.Create(stream: TStream);
begin
  inherited Create;

  FStream := stream;
  FStreamPos := stream.Position;
end;

function TCustomZStream.StreamRead(var buffer; count: Longint): Longint;
begin
  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  result := FStream.Read(buffer,count);

  FStreamPos := FStreamPos + result;
end;

function TCustomZStream.StreamWrite(const buffer; count: Longint): Longint;
begin
  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  result := FStream.Write(buffer,count);

  FStreamPos := FStreamPos + result;
end;

function TCustomZStream.StreamSeek(offset: Longint; origin: Word): Longint;
begin
  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  result := FStream.Seek(offset,origin);

  FStreamPos := FStream.Position;
end;

procedure TCustomZStream.StreamReadBuffer(var buffer; count: Longint);
begin
  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  FStream.ReadBuffer(buffer,count);

  FStreamPos := FStreamPos + count;
end;

procedure TCustomZStream.StreamWriteBuffer(const buffer; count: Longint);
begin
  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  FStream.WriteBuffer(buffer,count);

  FStreamPos := FStreamPos + count;
end;

procedure TCustomZStream.DoProgress;
begin
  if Assigned(FOnProgress) then FOnProgress(Self);
end;

function TCustomZStream.GetStreamPosition: TStreamPos;
begin
  result := FStream.Position;
end;

procedure TCustomZStream.SetStreamPosition(value: TStreamPos);
begin
  FStream.Position := value;
  FStreamPos := FStream.Position;
end;

{** TZCompressionStream *************************************************************************}

constructor TZCompressionStream.Create(dest: TStream;
  compressionLevel: TZCompressionLevel);
begin
  inherited Create(dest);

  FZStream.next_out := @FBuffer;
  FZStream.avail_out := SizeOf(FBuffer);

  ZCompressCheck(ZDeflateInit(FZStream, compressionLevel));
end;

constructor TZCompressionStream.Create(dest: TStream;
  compressionLevel: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy);
begin
  inherited Create(dest);

  FZStream.next_out := @FBuffer;
  FZStream.avail_out := SizeOf(FBuffer);

  ZCompressCheck(ZDeflateInit2(FZStream, compressionLevel, windowBits,
    memLevel, strategy));
end;

destructor TZCompressionStream.Destroy;
begin
  FZStream.next_in := Nil;
  FZStream.avail_in := 0;

  try
    while ZCompressCheck(ZDeflate(FZStream, zfFinish)) <> Z_STREAM_END do
    begin
      StreamWriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);

      FZStream.next_out := @FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);
    end;

    if FZStream.avail_out < SizeOf(FBuffer) then
    begin
      StreamWriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);
    end;
  finally
    ZDeflateEnd(FZStream);
  end;

  inherited Destroy;
end;

function TZCompressionStream.Read(var buffer; count: Longint): Longint;
begin
  raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.Write(const buffer; count: Longint): Longint;
var
  writeCount: Longint;
begin
  result := count;

  FZStream.next_in := @buffer;
  FZStream.avail_in := count;

  while FZStream.avail_in > 0 do
  begin
    ZCompressCheck(ZDeflate(FZStream, zfNoFlush));

    if FZStream.avail_out = 0 then
    begin
      writeCount := StreamWrite(FBuffer,SizeOf(FBuffer));

      if writeCount = SizeOf(FBuffer) then
      begin
        FZStream.next_out := @FBuffer;
        FZStream.avail_out := SizeOf(FBuffer);

        DoProgress;
      end
      else
      begin
        StreamPosition := StreamPosition - writeCount;

        result := Cardinal(count) - FZStream.avail_in;

        FZStream.avail_in := 0;
      end;
    end;
  end;
end;

function TZCompressionStream.Seek(offset: Longint; origin: Word): Longint;
begin
  if (offset = 0) and (origin = soFromCurrent) then
  begin
    result := FZStream.total_in;
  end
  else raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.GetCompressionRate: Single;
begin
  if FZStream.total_in = 0 then result := 0
  else result := (1.0 - (FZStream.total_out / FZStream.total_in)) * 100.0;
end;

{** TZDecompressionStream ***********************************************************************}

constructor TZDecompressionStream.Create(source: TStream);
begin
  inherited Create(source);

  FZStream.next_in := @FBuffer;
  FZStream.avail_in := 0;

  ZDecompressCheck(ZInflateInit(FZStream));
end;

constructor TZDecompressionStream.Create(source: TStream;
  windowBits: Integer);
begin
  inherited Create(source);

  FZStream.next_in := @FBuffer;
  FZStream.avail_in := 0;

  ZDecompressCheck(ZInflateInit2(FZStream, windowBits));
end;

destructor TZDecompressionStream.Destroy;
begin
  ZInflateEnd(FZStream);

  inherited Destroy;
end;

function TZDecompressionStream.Read(var buffer; count: Longint): Longint;
var
  zresult: Integer;
begin
  FZStream.next_out := @buffer;
  FZStream.avail_out := count;

  zresult := Z_OK;

  while (FZStream.avail_out > 0) and (zresult <> Z_STREAM_END) do
  begin
    if FZStream.avail_in = 0 then
    begin
      FZStream.avail_in := StreamRead(FBuffer,SizeOf(FBuffer));

      if FZStream.avail_in = 0 then
      begin
        result := Cardinal(count) - FZStream.avail_out;

        Exit;
      end;

      FZStream.next_in := @FBuffer;

      DoProgress;
    end;

    zresult := ZDecompressCheck(ZInflate(FZStream, zfNoFlush));
  end;

  if (zresult = Z_STREAM_END) and (FZStream.avail_in > 0) then
  begin
    StreamPosition := StreamPosition - FZStream.avail_in;

    FZStream.avail_in := 0;
  end;

  result := Cardinal(count) - FZStream.avail_out;
end;

function TZDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EZDecompressionError.Create(SZInvalid);
end;

function TZDecompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  buf: Array [0..8191] of Byte;
  i  : Integer;
begin
  if (offset = 0) and (origin = soFromBeginning) then
  begin
    ZDecompressCheck(ZInflateReset(FZStream));

    FZStream.next_in := @FBuffer;
    FZStream.avail_in := 0;

    StreamPosition := 0;
  end
  else if ((offset >= 0) and (origin = soFromCurrent)) or
          (((Cardinal(offset) - FZStream.total_out) > 0) and (origin = soFromBeginning)) then
  begin
    if origin = soFromBeginning then Dec(offset, FZStream.total_out);

    if offset > 0 then
    begin
      for i := 1 to offset div SizeOf(buf) do ReadBuffer(buf, SizeOf(buf));
      ReadBuffer(buf, offset mod SizeOf(buf));
    end;
  end
  else if (offset = 0) and (origin = soFromEnd) then
  begin
    while Read(buf, SizeOf(buf)) > 0 do ;
  end
  else raise EZDecompressionError.Create(SZInvalid);

  result := FZStream.total_out;
end;

{** TZCustomBuffer ******************************************************************************}

constructor TZCustomBuffer.Create;
begin
  inherited Create;

  FillChar(FZStream, SizeOf(TZStreamRec), 0);

  FBuffer := Nil;
  FBufferCapacity := 0;

  FBufferSize := 0;
end;

destructor TZCustomBuffer.Destroy;
begin
  BufferCapacity(0);

  inherited Destroy;
end;

procedure TZCustomBuffer.Clear;
begin
  BufferCapacity(0);

  FBufferSize := 0;
end;

procedure TZCustomBuffer.Flush(flush: TZFlush);
begin
  // to be implemented by descendents as needed
end;

function TZCustomBuffer.Write(const s: AnsiString): Integer;
begin
  result := Write(Pointer(s), Length(s));
end;

function TZCustomBuffer.Read(var buffer: Pointer; size: Integer): Integer;
begin
  result := BufferSize;
  if size < result then result := size;

  BufferRead(buffer, result);
end;

function TZCustomBuffer.Read(var s: AnsiString): Integer;
begin
  SetLength(s, BufferSize);

  result := Read(Pointer(s), Length(s));
end;

procedure TZCustomBuffer.BufferWrite(const buffer: Pointer; size: Integer);
begin
  if size > 0 then
  begin
    BufferCapacity(FBufferSize + size);

    Move(buffer^, Pointer(Integer(FBuffer) + FBufferSize)^, size);

    Inc(FBufferSize, size);
  end;
end;

procedure TZCustomBuffer.BufferRead(var buffer: Pointer; size: Integer);
begin
  if size > 0 then
  begin
    Move(FBuffer^, buffer^, size);

    Move(Pointer(Integer(FBuffer) + size)^, FBuffer^, FBufferSize - size);

    Dec(FBufferSize, size);
  end;
end;

procedure TZCustomBuffer.BufferCapacity(capacity: Integer);
const
  delta = 8192; // must be a power of 2
begin
  if capacity > 0 then
  begin
    capacity := (capacity + (delta - 1)) and not (delta - 1);
  end;

  if FBufferCapacity <> capacity then
  begin
    if capacity = 0 then FreeMem(FBuffer)
    else if FBufferCapacity = 0 then GetMem(FBuffer, capacity)
    else ReallocMem(FBuffer, capacity);

    FBufferCapacity := capacity;
  end;
end;

{** TZCompressionBuffer *************************************************************************}

constructor TZCompressionBuffer.Create(level: TZCompressionLevel);
begin
  inherited Create;

  ZCompressCheck(ZDeflateInit(FZStream, level));
end;

constructor TZCompressionBuffer.Create(level: TZCompressionLevel;
  windowBits, memLevel: Integer; strategy: TZStrategy);
begin
  inherited Create;

  ZCompressCheck(ZDeflateInit2(FZStream, level, windowBits, memLevel,
    strategy));
end;

destructor TZCompressionBuffer.Destroy;
begin
  ZCompressCheck(ZDeflateEnd(FZStream));

  inherited Destroy;
end;

procedure TZCompressionBuffer.Clear;
begin
  inherited Clear;

  ZCompressCheck(ZDeflateReset(FZStream));
end;

procedure TZCompressionBuffer.Flush(flush: TZFlush);
const
  outSize = 32768;
var
  zresult  : Integer;
  outBuffer: Array [0..outSize - 1] of Byte;
  outCount : Integer;
begin
  FZStream.next_in := Nil;
  FZStream.avail_in := 0;

  repeat
    FZStream.next_out := @outBuffer;
    FZStream.avail_out := outSize;

    zresult := ZCompressCheck(ZDeflate(FZStream, flush));

    outCount := outSize - FZStream.avail_out;

    BufferWrite(@outBuffer, outCount);
  until (zresult = Z_STREAM_END) or (FZStream.avail_out > 0);
end;

function TZCompressionBuffer.Write(const buffer: Pointer;
  size: Integer): Integer;
const
  outSize = 32768;
var
  zresult  : Integer;
  outBuffer: Array [0..outSize - 1] of Byte;
  outCount : Integer;
begin
  zresult := Z_OK;

  FZStream.next_in := buffer;
  FZStream.avail_in := size;

  while (zresult <> Z_STREAM_END) and (FZStream.avail_in > 0) do
  begin
    repeat
      FZStream.next_out := @outBuffer;
      FZStream.avail_out := outSize;

      zresult := ZCompressCheck(ZDeflate(FZStream, zfNoFlush));

      outCount := outSize - FZStream.avail_out;

      BufferWrite(@outBuffer, outCount);
    until (zresult = Z_STREAM_END) or (FZStream.avail_out > 0);
  end;

  result := Cardinal(size) - FZStream.avail_in;
end;

{** TZDecompressionBuffer ***********************************************************************}

constructor TZDecompressionBuffer.Create;
begin
  inherited Create;

  ZDecompressCheck(ZInflateInit(FZStream));
end;

constructor TZDecompressionBuffer.Create(windowBits: Integer);
begin
  inherited Create;

  ZDecompressCheck(ZInflateInit2(FZStream, windowBits));
end;

destructor TZDecompressionBuffer.Destroy;
begin
  ZDecompressCheck(ZInflateEnd(FZStream));

  inherited Destroy;
end;

procedure TZDecompressionBuffer.Clear;
begin
  inherited Clear;

  ZDecompressCheck(ZInflateReset(FZStream));
end;

function TZDecompressionBuffer.Write(const buffer: Pointer;
  size: Integer): Integer;
const
  outSize = 32768;
var
  zresult  : Integer;
  outBuffer: Array [0..outSize - 1] of Byte;
  outCount : Integer;
begin
  zresult := Z_OK;

  FZStream.next_in := buffer;
  FZStream.avail_in := size;

  while (zresult <> Z_STREAM_END) and (FZStream.avail_in > 0) do
  begin
    repeat
      FZStream.next_out := @outBuffer;
      FZStream.avail_out := outSize;

      zresult := ZDecompressCheck(ZInflate(FZStream, zfNoFlush), False);

      outCount := outSize - FZStream.avail_out;

      BufferWrite(@outBuffer, outCount);
    until (zresult = Z_STREAM_END) or (FZStream.avail_out > 0);
  end;

  result := Cardinal(size) - FZStream.avail_in;
end;

{** EZLibError **********************************************************************************}

constructor EZLibError.Create(code: Integer; const dummy: String);
begin
  inherited Create(z_errmsg[2 - code]);

  FErrorCode := code;
end;

constructor EZLibError.Create(error: TZError; const dummy: String);
begin
  Create(ZErrors[error], dummy);
end;

end.
