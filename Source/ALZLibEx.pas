unit ALZLibEx;

interface

uses
  system.classes,
  ZLibEx;

Function  ALZInflate4Browser(const s: AnsiString): AnsiString;
Function  ALZDeflate4Browser(const s: AnsiString): AnsiString;
function  ALZCompressStr(const s: AnsiString; level: TZCompressionLevel = zcDefault): AnsiString; inline;
function  ALZDecompressStr(const s: AnsiString): AnsiString; inline;
procedure ALZCompressStream(inStream, outStream: TStream; level: TZCompressionLevel = zcDefault); inline;
procedure ALZDecompressStream(inStream, outStream: TStream); inline;

implementation

{************************************************************}
Function  ALZDeflate4Browser(const s: AnsiString): AnsiString;
var buffer: Pointer;
    size  : Integer;
begin
  ZCompress2(PAnsiChar(s),Length(s),buffer,size,zcFastest,-15,9,zsDefault);
  SetLength(result,size);
  Move(buffer^,result[1],size);
  FreeMem(buffer);
end;

{************************************************************}
Function  ALZInflate4Browser(const s: AnsiString): AnsiString;
var buffer: Pointer;
    size  : Integer;
begin
  ZDecompress2(PAnsiChar(s),Length(s),buffer,size,-15);
  SetLength(result,size);
  Move(buffer^,result[1],size);
  FreeMem(buffer);
end;

{**********************************************************************************************}
function ALZCompressStr(const s: AnsiString; level: TZCompressionLevel = zcDefault): AnsiString;
Begin
  Result := ZCompressStr(s,level);
end;

{*********************************************************}
function ALZDecompressStr(const s: AnsiString): AnsiString;
begin
  Result := ZDecompressStr(s);
end;

{***********************************************************************************************}
procedure ALZCompressStream(inStream, outStream: TStream; level: TZCompressionLevel = zcDefault);
begin
  ZCompressStream(inStream, outStream, level);
end;

{**********************************************************}
procedure ALZDecompressStream(inStream, outStream: TStream);
begin
  ZDecompressStream(inStream, outStream);
end;

end.
