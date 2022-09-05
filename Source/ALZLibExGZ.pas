unit ALZLibExGZ;

interface

uses
  system.classes;

function  ALGZCompressStr(const s: AnsiString): AnsiString; inline;
function  ALGZDecompressStr(const s: AnsiString): AnsiString; inline;
procedure ALGZCompressStream(inStream, outStream: TStream); inline;
procedure ALGZDecompressStream(inStream, outStream: TStream); inline;

implementation

uses
  ZLibExGZ;

{********************************************************}
function ALGZCompressStr(const s: AnsiString): AnsiString;
Begin
  Result := GZCompressStr(s);
end;

{**********************************************************}
function ALGZDecompressStr(const s: AnsiString): AnsiString;
begin
  Result := GZDecompressStr(s);
end;

{*********************************************************}
procedure ALGZCompressStream(inStream, outStream: TStream);
begin
  GZCompressStream(inStream, outStream);
end;

{***********************************************************}
procedure ALGZDecompressStream(inStream, outStream: TStream);
begin
  GZDecompressStream(inStream, outStream);
end;

end.
