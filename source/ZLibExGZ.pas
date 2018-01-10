{*************************************************************************************************
*  ZLibExGZ.pas                                                                                  *
*                                                                                                *
*  copyright (c) 2000-2013 base2 technologies                                                    *
*  copyright (c) 1995-2002 Borland Software Corporation                                          *
*                                                                                                *
*  revision history                                                                              *
*    2012.05.23  updated for delphi xe2                                                          *
*                added overloaded GZCompressFile                                                 *
*    2011.07.21  fixed routines to validate size before calling Move                             *
*    2010.01.27  updated for delphi 2010                                                         *
*    2009.04.14  added overloaded string routines for AnsiString and                             *
*                  UnicodeString                                                                 *
*                removed deprecated Z*G routines                                                 *
*    2009.01.28  updated for delphi 2009 String (UnicodeString)                                  *
*    2008.05.15  added TGZCompressionStream and TGZDecompressionStream                           *
*    2007.11.06  changed TGZTrailer.Crc from Cardinal to Longint                                 *
*    2007.10.01  added GZDecompressStreamSize                                                    *
*                fixed GZDecompressStream position handling                                      *
*    2007.08.15  added GZCompressFile                                                            *
*    2007.07.18  fixed GZCompressStr filename and comment processing                             *
*    2007.03.18  modified naming convention for gzip routines GZ*                                *
*                deprecated previous gzip routines Z*G                                           *
*    2007.03.15  created separate unit for gzip routines/objects                                 *
*                added ZDecompressStreamG                                                        *
*                added overloaded ZCompressStrG                                                  *
*                added overloaded ZCompressStreamG                                               *
*    2007.02.24  added PWord declaration for delphi 5-                                           *
*    2006.08.10  added ZDecompressStrG                                                           *
*    2006.06.02  added DateTimeToUnix for delphi 5-                                              *
*    2006.03.27  added ZCompressStreamG                                                          *
*    2006.03.24  added ZCompressStrG                                                             *
*                                                                                                *
*  acknowledgments                                                                               *
*    ralf wenske                                                                                 *
*      2006.03.24  prototyping and assisting with ZCompressStrG and                              *
*                    ZCompressStreamG                                                            *
*                                                                                                *
*    roman krupicka                                                                              *
*      2006.06.02  pointing out the DateUtils unit and the DateTimeToUnix                        *
*                    function wasn't available prior to delphi 6                                 *                     *
*                                                                                                *
*    marcin treffler                                                                             *
*      2007.02.24  pointing out the missing PWord declaration for delphi 5                       *
*                                                                                                *
*    jean-jacques esquirol                                                                       *
*      2007.07.18  pointing out the "result" address issue when processing                       *
*                    filename and comment flags/content in GZCompressStr                         *
*      2007.11.06  pointing out the type differences with TGZTrailer.Crc                         *
*                    (Cardinal) and ZCrc32 (Longint)                                             *
*                                                                                                *
*    graham wideman                                                                              *
*      2007.10.01  beta testing GZDecompressStreamSize and pointing out the                      *
*                    position handling issue in GZDecompressStream                               *
*************************************************************************************************}

unit ZLibExGZ;

interface

{$I ZLibEx.inc}

uses
  ZLibEx, Windows, SysUtils, Classes {$IFDEF Version6Plus}, DateUtils {$ENDIF};

type
  {** TGZHeader *********************************************************************************}

  PGZHeader = ^TGZHeader;
  TGZHeader = packed record
    Id1       : Byte;
    Id2       : Byte;
    Method    : Byte;
    Flags     : Byte;
    Time      : Cardinal;
    ExtraFlags: Byte;
    OS        : Byte;
  end;

  {** TGZTrailer ********************************************************************************}

  PGZTrailer = ^TGZTrailer;
  TGZTrailer = packed record
    Crc : Longint;
    Size: Cardinal;
  end;

  {** TGZCompressionStream **********************************************************************}

  TGZCompressionStream = class(TZCompressionStream)
  private
    FFileName: AnsiString;
    FComment : AnsiString;
    FDateTime: TDateTime;

    FTrailer : TGZTrailer;
  public
    constructor Create(dest: TStream; const fileName, comment: AnsiString;
      dateTime: TDateTime); overload;

    constructor Create(dest: TStream); overload;

    destructor  Destroy; override;

    function  Write(const buffer; count: Longint): Longint; override;

    property FileName: AnsiString read FFileName;
    property Comment : AnsiString read FComment;
    property DateTime: TDateTime  read FDateTime;
  end;

  {** TGZDecompressionStream ********************************************************************}

  TGZDecompressionStream = class(TZDecompressionStream)
  private
    FFileName: AnsiString;
    FComment : AnsiString;
    FDateTime: TDateTime;

    FCrc     : Longint;
    FSize    : Cardinal;
  public
    constructor Create(source: TStream); reintroduce;
    destructor  Destroy; override;

    function Read(var buffer; count: Longint): Longint; override;

    property FileName: AnsiString read FFileName;
    property Comment : AnsiString read FComment;
    property DateTime: TDateTime  read FDateTime;
  end;

{** string routines *****************************************************************************}

{*************************************************************************************************
*  GZCompressStr                                                                                 *
*                                                                                                *
*  pre-conditions                                                                                *
*    s          = uncompressed data string                                                       *
*    fileName   = filename                                                                       *
*    comment    = comment                                                                        *
*    dateTime   = date/time                                                                      *
*                                                                                                *
*  return                                                                                        *
*    compressed data string in gzip format                                                       *
*************************************************************************************************}

function  GZCompressStr(const s: AnsiString; const fileName,
  comment: AnsiString; dateTime: TDateTime): RawByteString; overload;

procedure GZCompressString(var result: RawByteString; const s: AnsiString;
  const fileName, comment: AnsiString; dateTime: TDateTime); overload;

{$ifdef Version6Plus}
procedure GZCompressString(var result: RawByteString; const s: UnicodeString;
  const fileName, comment: AnsiString; dateTime: TDateTime); overload;
{$endif}

function  GZCompressStr(const s: AnsiString): RawByteString; overload;

procedure GZCompressString(var result: RawByteString; const s: AnsiString);
  overload;

{$ifdef Version6Plus}
procedure GZCompressString(var result: RawByteString; const s: UnicodeString);
  overload;
{$endif}

{*************************************************************************************************
*  GZDecompressStr                                                                               *
*                                                                                                *
*  pre-conditions                                                                                *
*    s = compressed data string in gzip format                                                   *
*                                                                                                *
*  post-conditions                                                                               *
*    fileName   = filename                                                                       *
*    comment    = comment                                                                        *
*    dateTime   = date/time                                                                      *
*                                                                                                *
*  return                                                                                        *
*    uncompressed data string                                                                    *
*************************************************************************************************}

function  GZDecompressStr(const s: RawByteString; var fileName,
  comment: AnsiString; var dateTime: TDateTime): AnsiString; overload;

procedure GZDecompressString(var result: AnsiString; const s: RawByteString;
  var fileName, comment: AnsiString; var dateTime: TDateTime); overload;

{$ifdef Version6Plus}
procedure GZDecompressString(var result: UnicodeString;
  const s: RawByteString; var fileName, comment: AnsiString;
  var dateTime: TDateTime); overload;
{$endif}

function  GZDecompressStr(const s: RawByteString): AnsiString; overload;

procedure GZDecompressString(var result: AnsiString; const s: RawByteString);
  overload;

{$ifdef Version6Plus}
procedure GZDecompressString(var result: UnicodeString;
  const s: RawByteString); overload;
{$endif}

{** stream routines *****************************************************************************}

procedure GZCompressStream(inStream, outStream: TStream; const fileName,
  comment: AnsiString; dateTime: TDateTime); overload;

procedure GZCompressStream(inStream, outStream: TStream); overload;

procedure GZDecompressStream(inStream, outStream: TStream; var fileName,
  comment: AnsiString; var dateTime: TDateTime); overload;

procedure GZDecompressStream(inStream, outStream: TStream); overload;

function  GZDecompressStreamSize(inStream: TStream; var fileName,
  comment: AnsiString; var dateTime: TDateTime): Longint; overload;

function  GZDecompressStreamSize(inStream: TStream): Longint; overload;

{** file routines *******************************************************************************}

procedure GZCompressFile(const inFileName, outFileName: String;
  const fileName, comment: AnsiString); overload;

procedure GZCompressFile(const inFileName, outFileName: String;
  const comment: AnsiString); overload;

procedure GZCompressFile(const inFileName, outFileName: String); overload;

procedure GZDecompressFile(const inFileName, outFolder: String;
  var comment: AnsiString); overload;

procedure GZDecompressFile(const inFileName, outFolder: String); overload;

implementation

uses
  ZLibExApi;

const
  GZ_ZLIB_WINDOWBITS = -15;
  GZ_ZLIB_MEMLEVEL   = 9;

  GZ_ASCII_TEXT  = $01;
  GZ_HEADER_CRC  = $02;
  GZ_EXTRA_FIELD = $04;
  GZ_FILENAME    = $08;
  GZ_COMMENT     = $10;
  GZ_RESERVED    = $E0;

  GZ_EXTRA_DEFAULT = 0;
  GZ_EXTRA_MAX     = 2;
  GZ_EXTRA_FASTEST = 4;

  SGZInvalid = 'Invalid GZStream operation!';

{$IFNDEF Version6Plus}

type
  PWord = ^Word;

{$ENDIF}

{** DateTimeToUnix ******************************************************************************}

{$IFNDEF Version6Plus}

{ Days between TDateTime basis (12/31/1899) and Unix time_t basis (1/1/1970) }

const
  UnixDateDelta = 25569;

function DateTimeToUnix(const AValue: TDateTime): Cardinal;
begin
  Result := Round((AValue - UnixDateDelta) * SecsPerDay);
end;

function UnixToDateTime(const AValue: Cardinal): TDateTime;
begin
  Result := AValue / SecsPerDay + UnixDateDelta;
end;

{$ENDIF}

{** FileAge *************************************************************************************}

{$ifndef Version2006Plus}

function FileAge(const fileName: String; var dateTime: TDateTime): Boolean;
begin
  dateTime := FileDateToDateTime(SysUtils.FileAge(fileName));

  result := True;
end;

{$endif}

{** string routines *****************************************************************************}

procedure GZInitializeCompressString(var result: RawByteString;
  const fileName, comment: AnsiString; dateTime: TDateTime);
var
  header: PGZHeader;
begin
  SetLength(result, SizeOf(TGZHeader));

  header := PGZHeader(@result[1]);

  FillChar(header^, SizeOf(TGZHeader), 0);

  header^.Id1 := $1F;
  header^.Id2 := $8B;
  header^.Method := Z_DEFLATED;

  if dateTime <> 0 then header^.Time := DateTimeToUnix(dateTime);

  header^.ExtraFlags := GZ_EXTRA_DEFAULT;
  header^.OS := 0;

  // build all flags first so "result" address doesn't change

  header^.Flags := 0;

  if Length(fileName) > 0 then
  begin
    header^.Flags := header^.Flags or GZ_FILENAME;
  end;

  if Length(comment) > 0 then
  begin
    header^.Flags := header^.Flags or GZ_COMMENT;
  end;

  // continue with content

  if Length(fileName) > 0 then
  begin
    result := result + fileName + #$00;
  end;

  if Length(comment) > 0 then
  begin
    result := result + comment + #$00;
  end;
end;

procedure GZFinalizeCompressString(var result: RawByteString; crc: Longint;
  size: Integer);
var
  trailer: PGZTrailer;
  index  : Integer;
begin
  index := Length(result);

  SetLength(result, index + SizeOf(TGZTrailer));

  trailer := PGZTrailer(@result[index + 1]);

  FillChar(trailer^, SizeOf(TGZTrailer), 0);

  trailer^.Crc := crc;
  trailer^.Size := size;
end;

procedure GZInitializeDecompressString(const s: RawByteString; var fileName,
  comment: AnsiString; var dateTime: TDateTime; var index: Integer);
var
  header  : PGZHeader;
  maxIndex: Integer;
  endIndex: Integer;
  size    : Integer;
begin
  if Length(s) < SizeOf(TGZHeader) then
  begin
    raise EZDecompressionError.Create(zeDataError);
  end;

  header := PGZHeader(@s[1]);

  if (header^.Id1 <> $1F) or (header^.Id2 <> $8B)
    or (header^.Method <> Z_DEFLATED)
    or ((header^.Flags and GZ_RESERVED) <> 0) then
  begin
    raise EZDecompressionError.Create(zeDataError);
  end;

  if header^.Time <> 0 then dateTime := UnixToDateTime(header^.Time)
  else dateTime := 0;

  maxIndex := Length(s) - SizeOf(TGZTrailer);

  index := SizeOf(TGZHeader) + 1;

  if (header^.Flags and GZ_EXTRA_FIELD) <> 0 then
  begin
    if index <= (maxIndex - 1) then
    begin
      size := PWord(@s[index])^;

      Inc(index, 2);

      if (size >= 0) and ((index + size) <= maxIndex) then Inc(index, size)
      else index := maxIndex + 1;
    end
    else index := maxIndex + 1;
  end;

  if (header^.Flags and GZ_FILENAME) <> 0 then
  begin
    endIndex := index;

    while (endIndex <= maxIndex) and (s[endIndex] <> #$00) do Inc(endIndex);

    SetLength(fileName, endIndex - index);

    if (endIndex - index) > 0 then
    begin
      Move(s[index], fileName[1], endIndex - index);
    end;

    index := endIndex;

    if index <= maxIndex then Inc(index);
  end
  else fileName := '';

  if (header^.Flags and GZ_COMMENT) <> 0 then
  begin
    endIndex := index;

    while (endIndex <= maxIndex) and (s[endIndex] <> #$00) do Inc(endIndex);

    SetLength(comment, endIndex - index);

    if (endIndex - index) > 0 then
    begin
      Move(s[index], comment[1], endIndex - index);
    end;

    index := endIndex;

    if index <= maxIndex then Inc(index);
  end
  else comment := '';

  if (header^.Flags and GZ_HEADER_CRC) <> 0 then
  begin
    if index <= (maxIndex - 1) then
    begin
      // todo: validate header crc

      Inc(index,2);
    end
    else index := maxIndex + 1; // force eof
  end;

  if index > maxIndex then
  begin
    raise EZDecompressionError.Create(zeDataError);
  end;
end;

procedure GZFinalizeDecompressString(const s: RawByteString; crc: Longint;
  size: Integer);
var
  trailer: PGZTrailer;
  index  : Integer;
begin
  index := Length(s) - SizeOf(TGZTrailer) + 1;

  trailer := PGZTrailer(@s[index]);

  if (trailer^.Crc <> crc)
    or (trailer^.Size <> Cardinal(size)) then
  begin
    raise EZDecompressionError.Create(zeDataError);
  end;
end;

function GZCompressStr(const s: AnsiString; const fileName,
  comment: AnsiString; dateTime: TDateTime): RawByteString;
begin
  GZCompressString(result, s, fileName, comment, dateTime);
end;

procedure GZCompressString(var result: RawByteString; const s: AnsiString;
  const fileName, comment: AnsiString; dateTime: TDateTime);
var
  buffer: RawByteString;
  crc   : Longint;
  size  : Integer;
begin
  GZInitializeCompressString(result, fileName, comment, dateTime);

  ZCompressString2(buffer, s, zcDefault, GZ_ZLIB_WINDOWBITS, GZ_ZLIB_MEMLEVEL,
    zsDefault);

  result := result + buffer;

  size := Length(s);

  crc := ZCrc32(0, s[1], size);

  GZFinalizeCompressString(result, crc, size);
end;

{$ifdef Version6Plus}
procedure GZCompressString(var result: RawByteString; const s: UnicodeString;
  const fileName, comment: AnsiString; dateTime: TDateTime);
var
  buffer: RawByteString;
  crc   : Longint;
  size  : Integer;
begin
  GZInitializeCompressString(result, fileName, comment, dateTime);

  ZCompressString2(buffer, s, zcDefault, GZ_ZLIB_WINDOWBITS, GZ_ZLIB_MEMLEVEL,
    zsDefault);

  result := result + buffer;

  size := Length(s) * SizeOf(UnicodeChar);

  crc := ZCrc32(0, s[1], size);

  GZFinalizeCompressString(result, crc, size);
end;
{$endif}

function GZCompressStr(const s: AnsiString): RawByteString;
begin
  GZCompressString(result, s);
end;

procedure GZCompressString(var result: RawByteString; const s: AnsiString);
begin
  GZCompressString(result, s, '', '', 0);
end;

{$ifdef Version6Plus}
procedure GZCompressString(var result: RawByteString; const s: UnicodeString);
begin
  GZCompressString(result, s, '', '', 0);
end;
{$endif}

function GZDecompressStr(const s: RawByteString; var fileName,
  comment: AnsiString; var dateTime: TDateTime): AnsiString;
begin
  GZDecompressString(result, s, fileName, comment, dateTime);
end;

procedure GZDecompressString(var result: AnsiString; const s: RawByteString;
  var fileName, comment: AnsiString; var dateTime: TDateTime);
var
  index: Integer;
  crc  : Longint;
  size : Integer;
begin
  result := '';

  GZInitializeDecompressString(s, fileName, comment, dateTime, index);

  size := Length(s) - SizeOf(TGZTrailer) - index + 1;

  ZDecompressString2(result, Copy(s, index, size), GZ_ZLIB_WINDOWBITS);

  size := Length(result);

  crc := ZCrc32(0, result[1], size);

  GZFinalizeDecompressString(s, crc, size);
end;

{$ifdef Version6Plus}
procedure GZDecompressString(var result: UnicodeString;
  const s: RawByteString; var fileName, comment: AnsiString;
  var dateTime: TDateTime);
var
  index: Integer;
  crc  : Longint;
  size : Integer;
begin
  result := '';

  GZInitializeDecompressString(s, fileName, comment, dateTime, index);

  size := Length(s) - SizeOf(TGZTrailer) - index + 1;

  ZDecompressString2(result, Copy(s, index, size), GZ_ZLIB_WINDOWBITS);

  size := Length(result) * SizeOf(UnicodeChar);

  crc := ZCrc32(0, result[1], size);

  GZFinalizeDecompressString(s, crc, size);
end;
{$endif}

function GZDecompressStr(const s: RawByteString): AnsiString;
begin
  GZDecompressString(result, s);
end;

procedure GZDecompressString(var result: AnsiString; const s: RawByteString);
var
  fileName: AnsiString;
  comment : AnsiString;
  dateTime: TDateTime;
begin
  GZDecompressString(result, s, fileName, comment, dateTime);
end;

{$ifdef Version6Plus}
procedure GZDecompressString(var result: UnicodeString;
  const s: RawByteString);
var
  fileName: AnsiString;
  comment : AnsiString;
  dateTime: TDateTime;
begin
  GZDecompressString(result, s, fileName, comment, dateTime);
end;
{$endif}

{** stream routines *****************************************************************************}

procedure GZCompressStream(inStream, outStream: TStream; const fileName,
  comment: AnsiString; dateTime: TDateTime);
const
  bufferSize = 32768;
var
  header    : TGZHeader;
  trailer   : TGZTrailer;
  buffer    : Array [0..bufferSize-1] of Byte;
  count     : Integer;
  position  : TStreamPos;
  nullString: AnsiString;
begin
  FillChar(header,SizeOf(TGZHeader),0);

  header.Id1 := $1F;
  header.Id2 := $8B;
  header.Method := Z_DEFLATED;

  if dateTime <> 0 then header.Time := DateTimeToUnix(dateTime);

  header.ExtraFlags := GZ_EXTRA_DEFAULT;
  header.OS := 0;

  header.Flags := 0;

  if Length(fileName) > 0 then header.Flags := header.Flags or GZ_FILENAME;
  if Length(comment) > 0 then header.Flags := header.Flags or GZ_COMMENT;

  FillChar(trailer, SizeOf(TGZTrailer), 0);

  trailer.Crc := 0;

  position := inStream.Position;

  while inStream.Position < inStream.Size do
  begin
    count := inStream.Read(buffer[0],bufferSize);

    trailer.Crc := ZCrc32(trailer.Crc,buffer[0],count);
  end;

  inStream.Position := position;

  trailer.Size := inStream.Size - inStream.Position;

  outStream.Write(header, SizeOf(TGZHeader));

  if Length(filename) > 0 then
  begin
    nullString := fileName + #$00;

    outStream.Write(nullString[1], Length(nullString));
  end;

  if Length(comment) > 0 then
  begin
    nullString := comment + #$00;

    outStream.Write(nullString[1], Length(nullString));
  end;

  ZCompressStream2(inStream, outStream, zcDefault, GZ_ZLIB_WINDOWBITS,
    GZ_ZLIB_MEMLEVEL, zsDefault);

  outStream.Write(trailer, SizeOf(TGZTrailer));
end;

procedure GZCompressStream(inStream, outStream: TStream);
begin
  GZCompressStream(inStream, outStream, '', '', 0);
end;

procedure GZDecompressStream(inStream, outStream: TStream; var fileName,
  comment: AnsiString; var dateTime: TDateTime);
const
  bufferSize = 32768;
var
  header     : TGZHeader;
  trailer    : TGZTrailer;
  buffer     : Array [0..bufferSize-1] of Byte;
  count      : Integer;
  position   : TStreamPos;
  endPosition: TStreamPos;
  size       : Integer;
  crc        : Longint;
  c          : AnsiChar;
begin
  if inStream.Read(header,SizeOf(TGZHeader)) <> SizeOf(TGZHeader) then
  begin
    raise EZDecompressionError.Create(zeDataError);
  end;

  if (header.Id1 <> $1F)
    or (header.Id2 <> $8B)
    or (header.Method <> Z_DEFLATED)
    or ((header.Flags and GZ_RESERVED) <> 0) then
  begin
    raise EZDecompressionError.Create(zeDataError);
  end;

  if header.Time <> 0 then dateTime := UnixToDateTime(header.Time)
  else dateTime := 0;

  if (header.Flags and GZ_EXTRA_FIELD) <> 0 then
  begin
    if inStream.Read(size,SizeOf(Word)) <> SizeOf(Word) then
    begin
      raise EZDecompressionError.Create(zeDataError);
    end;

    inStream.Position := inStream.Position + size;
  end;

  fileName := '';

  if (header.Flags and GZ_FILENAME) <> 0 then
  begin
    c := ' ';

    while (inStream.Position < inStream.Size) and (c <> #$00) do
    begin
      inStream.Read(c,1);

      if c <> #$00 then fileName := fileName + c;
    end;
  end;

  comment := '';

  if (header.Flags and GZ_COMMENT) <> 0 then
  begin
    c := ' ';

    while (inStream.Position < inStream.Size) and (c <> #$00) do
    begin
      inStream.Read(c, 1);

      if c <> #$00 then comment := comment + c;
    end;
  end;

  if (header.Flags and GZ_HEADER_CRC) <> 0 then
  begin
    // todo: validate header crc

    inStream.Position := inStream.Position + SizeOf(Word);
  end;

  if inStream.Position >= inStream.Size then
  begin
    raise EZDecompressionError.Create(zeDataError);
  end;

  position := outStream.Position;

  ZDecompressStream2(inStream,outStream,GZ_ZLIB_WINDOWBITS);

  endPosition := outStream.Position;

  if inStream.Read(trailer,SizeOf(TGZTrailer)) <> SizeOf(TGZTrailer) then
  begin
    raise EZDecompressionError.Create(zeDataError);
  end;

  crc := 0;

  outStream.Position := position;

  while outStream.Position < endPosition do
  begin
    size := bufferSize;

    if size > (endPosition - outStream.Position) then
    begin
      size := endPosition - outStream.Position;
    end;

    count := outStream.Read(buffer[0], size);

    crc := ZCrc32(crc, buffer[0], count);
  end;

  if (trailer.Crc <> crc)
    or (trailer.Size <> Cardinal(endPosition - position)) then
  begin
    raise EZDecompressionError.Create(zeDataError);
  end;
end;

procedure GZDecompressStream(inStream, outStream: TStream);
var
  fileName: AnsiString;
  comment : AnsiString;
  dateTime: TDateTime;
begin
  GZDecompressStream(inStream,outStream,fileName,comment,dateTime);
end;

type
  TNullStream = class(TStream)
  private
    FSize    : Int64;
    FPosition: Int64;
  public
    function  Read(var buffer; count: Longint): Longint; override;
    function  Write(const buffer; count: Longint): Longint; override;

    function  Seek(offset: Longint; origin: Word): Longint; override;
  end;

function TNullStream.Read(var buffer; count: Longint): Longint;
begin
  result := 0;

  if (FPosition >= 0) and (FPosition < FSize) and (count >= 0) then
  begin
    result := FSize - FPosition;
    if result > count then result := count;

    Inc(FPosition, result);
  end;
end;

function TNullStream.Write(const buffer; count: Longint): Longint;
begin
  result := count;

  FPosition := FPosition + count;
  if FPosition > FSize then FSize := FPosition;
end;

function TNullStream.Seek(offset: Longint; origin: Word): Longint;
begin
  case origin of
    soFromBeginning: FPosition := offset;
    soFromCurrent  : Inc(FPosition, offset);
    soFromEnd      : FPosition := FSize + offset;
  end;

  result := FPosition;
end;

function GZDecompressStreamSize(inStream: TStream; var fileName,
  comment: AnsiString; var dateTime: TDateTime): Longint;
const
  bufferSize = 32768;
var
  outStream: TNullStream;
  header   : TGZHeader;
  trailer  : TGZTrailer;
  position : TStreamPos;
  size     : Integer;
  c        : AnsiChar;
begin
  result := 0;

  position := inStream.Position;

  try
    if inStream.Read(header,SizeOf(TGZHeader)) <> SizeOf(TGZHeader) then
    begin
      raise EZDecompressionError.Create(zeDataError);
    end;

    if (header.Id1 <> $1F)
      or (header.Id2 <> $8B)
      or (header.Method <> Z_DEFLATED)
      or ((header.Flags and GZ_RESERVED) <> 0) then
    begin
      raise EZDecompressionError.Create(zeDataError);
    end;

    if header.Time <> 0 then dateTime := UnixToDateTime(header.Time)
    else dateTime := 0;

    if (header.Flags and GZ_EXTRA_FIELD) <> 0 then
    begin
      if inStream.Read(size,SizeOf(Word)) <> SizeOf(Word) then
      begin
        raise EZDecompressionError.Create(zeDataError);
      end;

      inStream.Position := inStream.Position + size;
    end;

    fileName := '';

    if (header.Flags and GZ_FILENAME) <> 0 then
    begin
      c := ' ';

      while (inStream.Position < inStream.Size) and (c <> #$00) do
      begin
        inStream.Read(c,1);

        if c <> #$00 then fileName := fileName + c;
      end;
    end;

    comment := '';

    if (header.Flags and GZ_COMMENT) <> 0 then
    begin
      c := ' ';

      while (inStream.Position < inStream.Size) and (c <> #$00) do
      begin
        inStream.Read(c,1);

        if c <> #$00 then comment := comment + c;
      end;
    end;

    if (header.Flags and GZ_HEADER_CRC) <> 0 then
    begin
      // todo: validate header crc

      inStream.Position := inStream.Position + SizeOf(Word);
    end;

    if inStream.Position >= inStream.Size then
    begin
      raise EZDecompressionError.Create(zeDataError);
    end;

    outStream := TNullStream.Create;

    try
      ZDecompressStream2(inStream,outStream,GZ_ZLIB_WINDOWBITS);

      result := outStream.Size;
    finally
      outStream.Free;
    end;

    if inStream.Read(trailer,SizeOf(TGZTrailer)) <> SizeOf(TGZTrailer) then
    begin
      raise EZDecompressionError.Create(zeDataError);
    end;

    if trailer.Size <> Cardinal(result) then
    begin
      raise EZDecompressionError.Create(zeDataError);
    end;
  finally
    inStream.Position := position;
  end;
end;

function GZDecompressStreamSize(inStream: TStream): Longint;
var
  fileName: AnsiString;
  comment : AnsiString;
  dateTime: TDateTime;
begin
  result := GZDecompressStreamSize(inStream,fileName,comment,dateTime);
end;

{** file routines *******************************************************************************}

procedure GZCompressFile(const inFileName, outFileName: String;
  const fileName, comment: AnsiString);
var
  inStream : TFileStream;
  outStream: TFileStream;
  dateTime : TDateTime;
begin
  inStream := TFileStream.Create(String(inFileName), fmOpenRead or fmShareDenyNone);

  try
    if not FileAge(inFileName, dateTime) then
    begin
      dateTime := 0;
    end;

    outStream := TFileStream.Create(outFileName, fmCreate);

    try
      GZCompressStream(inStream, outStream, fileName, comment, dateTime);
    finally
      outStream.Free;
    end;
  finally
    inStream.Free;
  end;
end;

procedure GZCompressFile(const inFileName, outFileName: String;
  const comment: AnsiString);
var
  fileName: AnsiString;
begin
  fileName := ExtractFilename(inFileName);

  GZCompressFile(inFileName, outFileName, fileName, '');
end;

procedure GZCompressFile(const inFileName, outFileName: String);
begin
  GZCompressFile(inFileName, outFileName, '');
end;

procedure GZDecompressFile(const inFileName, outFolder: String;
  var comment: AnsiString);
var
  inStream    : TFileStream;
  outStream   : TFileStream;
  outFileName : String;
  tempFileName: String;
  fileName    : AnsiString;
  dateTime    : TDateTime;
begin
  inStream := TFileStream.Create(inFileName, fmOpenRead or fmShareDenyNone);

  try
    if CompareText(ExtractFileExt(inFileName), '.gz') = 0 then
    begin
      tempFileName := IncludeTrailingPathDelimiter(outFolder);
      tempFileName := tempFileName + ChangeFileExt(ExtractFileName(inFileName), '');
    end
    else tempFileName := '_ZLIBEXGZ';

    outStream := TFileStream.Create(tempFileName, fmCreate);

    try
      GZDecompressStream(inStream, outStream, fileName, comment, dateTime);
    finally
      outStream.Free;
    end;

    if fileName <> '' then
    begin
      outFileName := ExtractFilePath(tempFileName) + ExtractFileName(String(fileName));

      RenameFile(tempFileName, outFileName);
    end
    else outFileName := tempFileName;

    if dateTime <> 0 then
    begin
      FileSetDate(outFileName, DateTimeToFileDate(dateTime));
    end;
  finally
    inStream.Free;
  end;
end;

procedure GZDecompressFile(const inFileName, outFolder: String);
var
  comment: AnsiString;
begin
  GZDecompressFile(inFileName, outFolder, comment);
end;

{** TGZCompressionStream ************************************************************************}

constructor TGZCompressionStream.Create(dest: TStream; const fileName,
  comment: AnsiString; dateTime: TDateTime);
var
  header    : TGZHeader;
  nullString: AnsiString;
begin
  inherited Create(dest, zcDefault, GZ_ZLIB_WINDOWBITS, GZ_ZLIB_MEMLEVEL,
    zsDefault);

  FFileName := fileName;
  FComment := comment;
  FDateTime := dateTime;

  FillChar(header,SizeOf(TGZHeader),0);

  header.Id1 := $1F;
  header.Id2 := $8B;
  header.Method := Z_DEFLATED;

  if FDateTime <> 0 then header.Time := DateTimeToUnix(FDateTime);

  header.ExtraFlags := GZ_EXTRA_DEFAULT;
  header.OS := 0;

  header.Flags := 0;

  if Length(fileName) > 0 then header.Flags := header.Flags or GZ_FILENAME;
  if Length(comment) > 0 then header.Flags := header.Flags or GZ_COMMENT;

  StreamWriteBuffer(header,SizeOf(TGZHeader));

  if Length(FFilename) > 0 then
  begin
    nullString := FFileName + #$00;

    StreamWriteBuffer(nullString[1],Length(nullString));
  end;

  if Length(FComment) > 0 then
  begin
    nullString := FComment + #$00;

    StreamWriteBuffer(nullString[1],Length(nullString));
  end;

  FillChar(FTrailer,SizeOf(TGZTrailer),0);
end;

constructor TGZCompressionStream.Create(dest: TStream);
begin
  Create(dest,'','',0);
end;

destructor TGZCompressionStream.Destroy;
begin
  inherited Destroy;

  StreamWriteBuffer(FTrailer,SizeOf(TGZTrailer));
end;

function TGZCompressionStream.Write(const buffer; count: Longint): Longint;
begin
  result := inherited Write(buffer,count);

  FTrailer.Crc := ZCrc32(FTrailer.Crc,buffer,result);
  FTrailer.Size := FTrailer.Size + Cardinal(result);
end;

{** TGZDecompressionStream **********************************************************************}

constructor TGZDecompressionStream.Create(source: TStream);

  function ReadNullString: AnsiString;
  var
    c: AnsiChar;
  begin
    result := '';

    c := ' ';

    while c <> #$00 do
    begin
      if StreamRead(c,1) <> 1 then
      begin
        raise EZDecompressionError.Create(zeStreamError);
      end;

      if c <> #$00 then result := result + c;
    end;
  end;

var
  header: TGZHeader;
  size  : Integer;
begin
  inherited Create(source,GZ_ZLIB_WINDOWBITS);

  if StreamRead(header,SizeOf(TGZHeader)) <> SizeOf(TGZHeader) then
  begin
    raise EZDecompressionError.Create(zeStreamError);
  end;

  if (header.Id1 <> $1F)
    or (header.Id2 <> $8B)
    or (header.Method <> Z_DEFLATED)
    or ((header.Flags and GZ_RESERVED) <> 0) then
  begin
    raise EZDecompressionError.Create(zeDataError);
  end;

  if header.Time <> 0 then FDateTime := UnixToDateTime(header.Time)
  else FDateTime := 0;

  if (header.Flags and GZ_EXTRA_FIELD) <> 0 then
  begin
    if StreamRead(size,SizeOf(Word)) <> SizeOf(Word) then
    begin
      raise EZDecompressionError.Create(zeStreamError);
    end;

    StreamPosition := StreamPosition + size;
  end;

  FFileName := '';

  if (header.Flags and GZ_FILENAME) <> 0 then
  begin
    FFileName := ReadNullString;
  end;

  FComment := '';

  if (header.Flags and GZ_COMMENT) <> 0 then
  begin
    FComment := ReadNullString;
  end;

  if (header.Flags and GZ_HEADER_CRC) <> 0 then
  begin
    // todo: validate header crc

    StreamPosition := StreamPosition + SizeOf(Word);
  end;

  FCrc := 0;
  FSize := 0;
end;

destructor TGZDecompressionStream.Destroy;
var
  trailer: TGZTrailer;
begin
  inherited Destroy;

  if StreamRead(trailer,SizeOf(TGZTrailer)) <> SizeOf(TGZTrailer) then
  begin
    raise EZDecompressionError.Create(zeStreamError);
  end;

  if (trailer.Crc <> FCrc) or (trailer.Size <> FSize) then
  begin
    raise EZDecompressionError.Create(zeDataError);
  end;
end;

function TGZDecompressionStream.Read(var buffer; count: Longint): Longint;
begin
  result := inherited Read(buffer,count);

  FCrc := ZCrc32(FCrc,buffer,result);
  FSize := FSize + Cardinal(result);
end;

end.
