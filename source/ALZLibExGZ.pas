{*****************************************************************************
*  ALZLibExGZ.pas                                                            *
*                                                                            *
*  copyright (c) 2000-2007 base2 technologies                                *
*  copyright (c) 1995-2002 Borland Software Corporation                      *
*                                                                            *
*  revision history                                                          *
*    2007.07.18  fixed GZCompressStr filename and comment processing         *
*    2007.03.18  modified naming convention for gzip routines GZ*            *
*                deprecated previous gzip routines *G                        *
*    2007.03.15  created separate unit for gzip routines/objects             *
*                added ZDecompressStreamG                                    *
*                added overloaded ZCompressStrG                              *
*                added overloaded ZCompressStreamG                           *
*    2007.02.24  added PWord declaration for delphi 5-                       *
*    2006.08.10  added ZDecompressStrG                                       *
*    2006.06.02  added DateTimeToUnix for delphi 5-                          *
*    2006.03.27  added ZCompressStreamG                                      *
*    2006.03.24  added ZCompressStrG                                         *
*                                                                            *
*  acknowledgments                                                           *
*    ralf wenske                                                             *
*      2006.03.24  prototyping and assisting with ZCompressStrG and          *
*                    ZCompressStreamG                                        *
*                                                                            *
*    roman krupicka                                                          *
*      2006.06.02  pointing out the DateUtils unit and the DateTimeToUnix    *
*                    function wasn't available prior to delphi 6             *                     *
*                                                                            *
*    marcin treffler                                                         *
*      2007.02.24  pointing out the missing PWord declaration for delphi 5   *
*                                                                            *
*    jean-jacques esquirol                                                   *
*      2007.07.18  pointing out the "result" address issue when processing   *
*                    filename and comment flags/content in GZCompressStr     *
*****************************************************************************}
unit ALZLibExGZ;

interface

uses
  ALZLibEx, SysUtils, Classes, DateUtils;

type
  {** TGZHeader *************************************************************}

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

  {** TGZTrailer ************************************************************}

  PGZTrailer = ^TGZTrailer;
  TGZTrailer = packed record
    Crc : Cardinal;
    Size: Cardinal;
  end;

{** string routines *********************************************************}

{*****************************************************************************
*  GZCompressStr                                                             *
*                                                                            *
*  pre-conditions                                                            *
*    s          = uncompressed data string                                   *
*    fileName   = filename                                                   *
*    comment    = comment                                                    *
*    dateTime   = date/time                                                  *
*                                                                            *
*  return                                                                    *
*    compressed data string in gzip format                                   *
*****************************************************************************}

function  GZCompressStr(const s: String; const fileName, comment: String;
  dateTime: TDateTime): String; overload;

function  GZCompressStr(const s: String): String; overload;

{*****************************************************************************
*  GZDecompressStr                                                           *
*                                                                            *
*  pre-conditions                                                            *
*    s = compressed data string in gzip format                               *
*                                                                            *
*  post-conditions                                                           *
*    fileName   = filename                                                   *
*    comment    = comment                                                    *
*    dateTime   = date/time                                                  *
*                                                                            *
*  return                                                                    *
*    uncompressed data string                                                *
*****************************************************************************}

function  GZDecompressStr(const s: String; var fileName, comment: String;
  var dateTime: TDateTime): String; overload;

function  GZDecompressStr(const s: String): String; overload;

{** stream routines *********************************************************}

procedure GZCompressStream(inStream, outStream: TStream; const fileName,
  comment: String; dateTime: TDateTime); overload;

procedure GZCompressStream(inStream, outStream: TStream); overload;

procedure GZDecompressStream(inStream, outStream: TStream; var fileName,
  comment: String; var dateTime: TDateTime); overload;

procedure GZDecompressStream(inStream, outStream: TStream); overload;

{** deprecated routines *****************************************************}

function  ZCompressStrG(const s: String; const fileName, comment: String;
  dateTime: TDateTime): String; overload;

function  ZCompressStrG(const s: String): String; overload;

function  ZDecompressStrG(const s: String; var fileName, comment: String;
  var dateTime: TDateTime): String; overload;

function  ZDecompressStrG(const s: String): String; overload;

procedure ZCompressStreamG(inStream, outStream: TStream; const fileName,
  comment: String; dateTime: TDateTime); overload;

procedure ZCompressStreamG(inStream, outStream: TStream); overload;

procedure ZDecompressStreamG(inStream, outStream: TStream; var fileName,
  comment: String; var dateTime: TDateTime); overload;

procedure ZDecompressStreamG(inStream, outStream: TStream); overload;

implementation

const
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

{** string routines *********************************************************}

function GZCompressStr(const s: String; const fileName, comment: String;
  dateTime: TDateTime): String;
var
  header : PGZHeader;
  trailer: PGZTrailer;
  len    : Integer;
begin
  SetLength(result,SizeOf(TGZHeader));

  header := PGZHeader(@result[1]);

  FillChar(header^,SizeOf(TGZHeader),0);

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

  result := result + ZCompressStr2(s,zcDefault,-15,9,zsDefault);

  len := Length(result);

  SetLength(result,len + SizeOf(TGZTrailer));

  trailer := PGZTrailer(@result[len + 1]);

  FillChar(trailer^,SizeOf(TGZTrailer),0);

  trailer^.Crc := ZCrc32(0,s[1],Length(s));
  trailer^.Size := Length(s);
end;

function GZCompressStr(const s: String): String;
begin
  result := GZCompressStr(s,'','',0);
end;

function GZDecompressStr(const s: String; var fileName, comment: String;
  var dateTime: TDateTime): String;
var
  header  : PGZHeader;
  trailer : PGZTrailer;
  index   : Integer;
  maxIndex: Integer;
  endIndex: Integer;
  size    : Integer;
begin
  result := '';

  if Length(s) < SizeOf(TGZHeader) then
  begin
    raise EZDecompressionError.Create(zeDataError);
  end;

  header := PGZHeader(@s[1]);

  if (header^.Id1 <> $1F)
    or (header^.Id2 <> $8B)
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

      Inc(index,2);

      if (size >= 0) and ((index + size) <= maxIndex) then Inc(index,size)
      else index := maxIndex + 1;
    end
    else index := maxIndex + 1;
  end;

  if (header^.Flags and GZ_FILENAME) <> 0 then
  begin
    endIndex := index;

    while (endIndex <= maxIndex) and (s[endIndex] <> #$00) do Inc(endIndex);

    SetLength(fileName,endIndex - index);
    Move(s[index],fileName[1],endIndex - index);

    index := endIndex;

    if index <= maxIndex then Inc(index);
  end
  else fileName := '';

  if (header^.Flags and GZ_COMMENT) <> 0 then
  begin
    endIndex := index;

    while (endIndex <= maxIndex) and (s[endIndex] <> #$00) do Inc(endIndex);

    SetLength(comment,endIndex - index);
    Move(s[index],comment[1],endIndex - index);

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

  size := maxIndex - index + 1;

  result := ZDecompressStr2(Copy(s,index,size),-15);

  Inc(index,size);

  trailer := PGZTrailer(@s[index]);

  if (trailer^.Crc <> Cardinal(ZCrc32(0,result[1],Length(result))))
    or (trailer^.Size <> Cardinal(Length(result))) then
  begin
    raise EZDecompressionError.Create(zeDataError);
  end;
end;

function GZDecompressStr(const s: String): String;
var
  fileName: String;
  comment : String;
  dateTime: TDateTime;
begin
  result := GZDecompressStr(s,fileName,comment,dateTime);
end;

{** stream routines *********************************************************}

procedure GZCompressStream(inStream, outStream: TStream; const fileName,
  comment: String; dateTime: TDateTime);
const
  bufferSize = 32768;
var
  header    : TGZHeader;
  trailer   : TGZTrailer;
  buffer    : Array [0..bufferSize-1] of Char;
  count     : Integer;
  position  : Int64;
  nullString: String;
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

  FillChar(trailer,SizeOf(TGZTrailer),0);

  trailer.Crc := 0;

  position := inStream.Position;

  while inStream.Position < inStream.Size do
  begin
    count := inStream.Read(buffer[0],bufferSize);

    trailer.Crc := ZCrc32(trailer.Crc,buffer[0],count);
  end;

  inStream.Position := position;

  trailer.Size := inStream.Size - inStream.Position;

  outStream.Write(header,SizeOf(TGZHeader));

  if Length(filename) > 0 then
  begin
    nullString := fileName + #$00;

    outStream.Write(nullString[1],Length(nullString));
  end;

  if Length(comment) > 0 then
  begin
    nullString := comment + #$00;

    outStream.Write(nullString[1],Length(nullString));
  end;

  ZCompressStream2(inStream,outStream,zcDefault,-15,9,zsDefault);

  outStream.Write(trailer,SizeOf(TGZTrailer));
end;

procedure GZCompressStream(inStream, outStream: TStream);
begin
  GZCompressStream(inStream,outStream,'','',0);
end;

procedure GZDecompressStream(inStream, outStream: TStream; var fileName,
  comment: String; var dateTime: TDateTime);
const
  bufferSize = 32768;
var
  header  : TGZHeader;
  trailer : TGZTrailer;
  buffer  : Array [0..bufferSize-1] of Char;
  count   : Integer;
  position: Int64;
  size    : Integer;
  crc     : Cardinal;
  c       : Char;
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

  position := outStream.Position;

  ZDecompressStream2(inStream,outStream,-15);

  if inStream.Read(trailer,SizeOf(TGZTrailer)) <> SizeOf(TGZTrailer) then
  begin
    raise EZDecompressionError.Create(zeDataError);
  end;

  outStream.Position := position;

  crc := 0;

  while outStream.Position < outStream.Size do
  begin
    count := outStream.Read(buffer[0],bufferSize);

    crc := ZCrc32(crc,buffer[0],count);
  end;

  if (trailer.Crc <> crc)
    or (trailer.Size <> Cardinal(outStream.Size - position)) then
  begin
    raise EZDecompressionError.Create(zeDataError);
  end;
end;

procedure GZDecompressStream(inStream, outStream: TStream);
var
  fileName: String;
  comment : String;
  dateTime: TDateTime;
begin
  GZDecompressStream(inStream,outStream,fileName,comment,dateTime);
end;

{** deprecated routines *****************************************************}

function ZCompressStrG(const s: String; const fileName, comment: String;
  dateTime: TDateTime): String;
begin
  result := GZCompressStr(s,fileName,comment,dateTime);
end;

function ZCompressStrG(const s: String): String;
begin
  result := GZCompressStr(s);
end;

function ZDecompressStrG(const s: String; var fileName, comment: String;
  var dateTime: TDateTime): String;
begin
  result := GZDecompressStr(s,fileName,comment,dateTime);
end;

function ZDecompressStrG(const s: String): String;
begin
  result := GZDecompressStr(s);
end;

procedure ZCompressStreamG(inStream, outStream: TStream; const fileName,
  comment: String; dateTime: TDateTime);
begin
  GZCompressStream(inStream,outStream,fileName,comment,dateTime);
end;

procedure ZCompressStreamG(inStream, outStream: TStream);
begin
  GZCompressStream(inStream,outStream);
end;

procedure ZDecompressStreamG(inStream, outStream: TStream; var fileName,
  comment: String; var dateTime: TDateTime);
begin
  GZDecompressStream(inStream,outStream,fileName,comment,dateTime);
end;

procedure ZDecompressStreamG(inStream, outStream: TStream);
begin
  GZDecompressStream(inStream,outStream);
end;

end.
