(*
    "The contents of this file are subject to the Mozilla Public License
    Version 1.1 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at
    http://www.mozilla.org/MPL/

    Software distributed under the License is distributed on an "AS IS"
    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
    License for the specific language governing rights and limitations
    under the License.

    The Initial Developer of the Original Code is
      Henri Gourvest <hgourvest@progdigy.com>.
*)

unit PDGUtils;
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}
{$I PDGAppServer.inc}

interface
uses Classes, SysUtils
{$IFDEF MSWINDOWS}
, windows
{$ENDIF}
{$IFDEF FPC}
, sockets
, paszlib
{$ELSE}
, WinSock
, PDGZLib
{$ENDIF}
;

{$IFDEF Darwin}
const
  ThreadIdNull = nil;
{$ELSE}
const
  ThreadIdNull = 1;
{$ENDIF}

type
{$IFNDEF FPC}
  PtrInt = Longint;
  TThreadID = LongWord;
{$ENDIF}

{$IFNDEF UNICODE}
  UnicodeString = WideString;
  RawByteString = AnsiString;
{$ENDIF}

  ERemoteError = class(Exception)
  end;

  TPooledMemoryStream = class(TStream)
  private
    FPageSize: integer;
    FList: TList;
    FSize: Integer;
    FPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    procedure Clear;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure WriteString(const str: string; writesize: boolean; cp: Integer = 0);
    procedure WriteInteger(const V: Integer);
    function ReadString: string;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    function SaveToSocket(socket: longint; writesize: boolean = true): boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    function LoadFromSocket(socket: longint; readsize: boolean = true): boolean;
    constructor Create(PageSize: integer = 1024); virtual;
    destructor Destroy; override;
  end;

function CompressStream(inStream, outStream: TStream; level: Integer = Z_DEFAULT_COMPRESSION): boolean; overload;
function DecompressStream(inStream, outStream: TStream): boolean; overload;

function CompressStream(inStream: TStream; outSocket: longint; level: Integer = Z_DEFAULT_COMPRESSION): boolean; overload;
function DecompressStream(inSocket: longint; outStream: TStream): boolean; overload;

function receive(s: longint; var Buf; len, flags: Integer): Integer;

// Base64 functions from <dirk.claessens.dc@belgium.agfa.com> (modified)
function StrTobase64(Buf: string): string;
function Base64ToStr(const B64: string): string;
procedure StreamToBase64(const StreamIn, StreamOut: TStream);
procedure Base64ToStream(const data: string; stream: TStream);

function FileToString(const FileName: string): string;
function StreamToStr(stream: TStream): string;

{$IFDEF UNIX}
function GetTickCount: Cardinal;
{$ENDIF}

implementation
uses uiblib, PDGOpenSSL;

const
  Base64Code: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

  Base64Map: array[#0..#127] of Integer = (
    Byte('='), 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
           64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
           64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 62, 64, 64, 64, 63,
           52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 64, 64, 64, 64, 64, 64,
           64,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
           15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 64, 64, 64, 64, 64,
           64, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
           41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 64, 64, 64, 64, 64);

{$IFDEF UNIX}
function GetTickCount: Cardinal;
var
  h, m, s, s1000: word;
begin
  decodetime(time, h, m, s, s1000);
  Result := Cardinal(h * 3600000 + m * 60000 + s * 1000 + s1000);
end;
{$ENDIF}

function StrTobase64(Buf: string): string;
var
  i: integer;
  x1, x2, x3, x4: byte;
  PadCount: integer;
begin
  PadCount := 0;
  // we need at least 3 input bytes...
  while length(Buf) < 3 do
  begin
    Buf := Buf + #0;
    inc(PadCount);
  end;

  // ...and all input must be an even multiple of 3
  while (length(Buf) mod 3) <> 0 do
  begin
    Buf := Buf + #0; // if not, zero padding is added
    inc(PadCount);
  end;

  Result := '';
  i := 1;

  // process 3-byte blocks or 24 bits
  while i <= length(Buf) - 2 do
  begin
    // each 3 input bytes are transformed into 4 index values
    // in the range of  0..63, by taking 6 bits each step

    // 6 high bytes of first char
    x1 := (Ord(Buf[i]) shr 2) and $3F;

    // 2 low bytes of first char + 4 high bytes of second char
    x2 := ((Ord(Buf[i]) shl 4) and $3F)
      or Ord(Buf[i + 1]) shr 4;

    // 4 low bytes of second char + 2 high bytes of third char
    x3 := ((Ord(Buf[i + 1]) shl 2) and $3F)
      or Ord(Buf[i + 2]) shr 6;

    // 6 low bytes of third char
    x4 := Ord(Buf[i + 2]) and $3F;

    // the index values point into the code array
    Result := Result
      + Base64Code[x1 + 1]
      + Base64Code[x2 + 1]
      + Base64Code[x3 + 1]
      + Base64Code[x4 + 1];
    inc(i, 3);
  end;

  // if needed, finish by forcing padding chars ('=')
  // at end of string
  if PadCount > 0 then
    for i := Length(Result) downto 1 do
    begin
      Result[i] := '=';
      dec(PadCount);
      if PadCount = 0 then Break;
    end;
end;

function Base64ToStr(const B64: string): string;
var
  i, PadCount: integer;
  x1, x2, x3: byte;
begin
  Result := '';
  // input _must_ be at least 4 chars long,
  // or multiple of 4 chars
  if (Length(B64) < 4) or (Length(B64) mod 4 <> 0) then Exit;

  PadCount := 0;
  i := Length(B64);
  // count padding chars, if any
  while (B64[i] = '=')
  and (i > 0) do
  begin
    inc(PadCount);
    dec(i);
  end;
  //
  Result := '';
  i := 1;
  while i <= Length(B64) - 3 do
  begin
    // reverse process of above
    x1 := (Base64Map[B64[i]] shl 2) or (Base64Map[B64[i + 1]] shr 4);
    Result := Result + Char(x1);
    x2 := (Base64Map[B64[i + 1]] shl 4) or (Base64Map[B64[i + 2]] shr 2);
    Result := Result + Char(x2);
    x3 := (Base64Map[B64[i + 2]] shl 6) or (Base64Map[B64[i + 3]]);
    Result := Result + Char(x3);
    inc(i, 4);
  end;

  // delete padding, if any
  while PadCount > 0 do
  begin
    Delete(Result, Length(Result), 1);
    dec(PadCount);
  end;
end;

procedure StreamToBase64(const StreamIn, StreamOut: TStream);
const
  Base64Code: PChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  EQ2: PChar = '==';
  EQ4: PChar = 'A===';
var
  V: array[0..2] of byte;
  C: array[0..3] of Char;
begin
  StreamIn.Seek(0, soFromBeginning);
  while true do
    case StreamIn.Read(V, 3) of
    3: begin
         C[0] := Base64Code[(V[0] shr 2) and $3F];
         C[1] := Base64Code[((V[0] shl 4) and $3F) or V[1] shr 4];
         C[2] := Base64Code[((V[1] shl 2) and $3F) or V[2] shr 6];
         C[3] := Base64Code[V[2] and $3F];
         StreamOut.Write(C, 4*SizeOf(Char));
       end;
    2: begin
         C[0] := Base64Code[(V[0] shr 2) and $3F];
         C[1] := Base64Code[((V[0] shl 4) and $3F) or V[1] shr 4];
         C[2] := Base64Code[((V[1] shl 2) and $3F) or 0    shr 6];
         StreamOut.Write(C, 3*SizeOf(Char));
         StreamOut.Write(EQ2^, 1*SizeOf(Char));
         Break;
       end;
    1: begin
         C[0] := Base64Code[(V[0] shr 2) and $3F];
         C[1] := Base64Code[((V[0] shl 4) and $3F) or 0 shr 4];
         StreamOut.Write(C, 2*SizeOf(Char));
         StreamOut.Write(EQ2^, 2*SizeOf(Char));
         Break;
       end;
    0: begin
         if StreamIn.Position = 0 then
           StreamOut.Write(EQ4^, 4*SizeOf(Char));
         Break;
       end;
    end;
end;


procedure Base64ToStream(const data: string; stream: TStream);
var
  i, PadCount: integer;
  buf: array[0..2] of  byte;
begin
  if (Length(data) < 4) or (Length(data) mod 4 <> 0) then Exit;
  PadCount := 0;
  i := Length(data);
  while (data[i] = '=')
  and (i > 0) do
  begin
    inc(PadCount);
    dec(i);
  end;

  i := 1;
  while i <= Length(data) - 3 do
  begin
    // reverse process of above
    buf[0] := (Base64Map[data[i]] shl byte(2)) or (Base64Map[data[i + 1]] shr byte(4));
    buf[1] := (Base64Map[data[i + 1]] shl 4) or (Base64Map[data[i + 2]] shr 2);
    buf[2] := (Base64Map[data[i + 2]] shl 6) or (Base64Map[data[i + 3]]);
    stream.Write(buf, sizeof(buf));
    inc(i, 4);
  end;

  // delete padding, if any
  if PadCount > 0 then
    stream.Position := stream.Position - PadCount;
end;

{$IF not declared(InterLockedCompareExchange)}
{$IFDEF MSWINDOWS}
function InterlockedCompareExchange(var Destination: longint; Exchange: longint; Comperand: longint): longint stdcall; external 'kernel32' name 'InterlockedCompareExchange';
{$ENDIF}
{$ifend}

const
  bufferSize = 32768;

function receive(s: longint; var Buf; len, flags: Integer): Integer;
var
  p: PChar;
  r, l: integer;
begin
  Result := 0;
  p := @Buf;
  l := len;
{$IFDEF FPC}
  r := fprecv(s, p, l, flags);
{$ELSE}
  r := recv(s, p^, l, flags);
{$ENDIF}
  while (r > 0) and (r < l) do
  begin
    inc(Result, r);
    dec(l, r);
    inc(p, r);
{$IFDEF FPC}
    r := fprecv(s, p, l, flags);
{$ELSE}
    r := recv(s, p^, l, flags);
{$ENDIF}
  end;
  inc(Result, r);
end;

function CompressStream(inStream, outStream: TStream; level: Integer): boolean;
var
  zstream: TZStream;
  zresult: Integer;
  inBuffer: array[0..bufferSize - 1] of byte;
  outBuffer: array[0..bufferSize - 1] of byte;
  inSize: Integer;
  outSize: Integer;
label
  error;
begin
  inStream.Seek(0, soFromBeginning);
  Result := False;
  FillChar(zstream, SizeOf(zstream), 0);
  if DeflateInit(zstream, level) < Z_OK then
    exit;
  inSize := inStream.Read(inBuffer, bufferSize);
  while inSize > 0 do
  begin
    zstream.next_in := @inBuffer;
    zstream.avail_in := inSize;
    repeat
      zstream.next_out := @outBuffer;
      zstream.avail_out := bufferSize;
      if deflate(zstream, Z_NO_FLUSH) < Z_OK then
        goto error;
      outSize := bufferSize - zstream.avail_out;
      outStream.Write(outBuffer, outSize);
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);
    inSize := inStream.Read(inBuffer, bufferSize);
  end;
  repeat
    zstream.next_out := @outBuffer;
    zstream.avail_out := bufferSize;
    zresult := deflate(zstream, Z_FINISH);
    if zresult < Z_OK then
      goto error;
    outSize := bufferSize - zstream.avail_out;
    outStream.Write(outBuffer, outSize);
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);
  Result := deflateEnd(zstream) >= Z_OK;
  exit;
  error:
  deflateEnd(zstream);
end;

function DecompressStream(inStream, outStream: TStream): boolean;
var
  zstream: TZStream;
  zresult: Integer;
  inBuffer: array[0..bufferSize - 1] of byte;
  outBuffer: array[0..bufferSize - 1] of byte;
  inSize: Integer;
  outSize: Integer;
label
  error;
begin
  Result := False;
  inStream.Seek(0, soFromBeginning);
  FillChar(zstream, SizeOf(zstream), 0);
  if InflateInit(zstream) < Z_OK then
    exit;
  inSize := inStream.Read(inBuffer, bufferSize);
  while inSize > 0 do
  begin
    zstream.next_in := @inBuffer;
    zstream.avail_in := inSize;
    repeat
      zstream.next_out := @outBuffer;
      zstream.avail_out := bufferSize;
      if inflate(zstream, Z_NO_FLUSH) < Z_OK then
        goto error;
      outSize := bufferSize - zstream.avail_out;
      outStream.Write(outBuffer, outSize);
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);
    inSize := inStream.Read(inBuffer, bufferSize);
  end;
  repeat
    zstream.next_out := @outBuffer;
    zstream.avail_out := bufferSize;
    zresult := inflate(zstream, Z_FINISH);
    if zresult < Z_OK then
      goto error;
    outSize := bufferSize - zstream.avail_out;
    outStream.Write(outBuffer, outSize);
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);
  Result := inflateEnd(zstream) >= Z_OK;
  exit;
  error:
  inflateEnd(zstream);
end;

function CompressStream(inStream: TStream; outSocket: longint; level: Integer): boolean;
var
  zstream: TZStream;
  zresult: Integer;
  inBuffer: array[0..bufferSize - 1] of byte;
  outBuffer: array[0..bufferSize - 1] of byte;
  inSize: Integer;
  outSize: Integer;
label
  error;
begin
  inStream.Seek(0, soFromBeginning);
  Result := False;
  FillChar(zstream, SizeOf(zstream), 0);
  if DeflateInit(zstream, level) < Z_OK then
    Exit;
  inSize := inStream.Read(inBuffer, bufferSize);
  while inSize > 0 do
  begin
    zstream.next_in := @inBuffer;
    zstream.avail_in := inSize;
    repeat
      zstream.next_out := @outBuffer;
      zstream.avail_out := bufferSize;
      if deflate(zstream, Z_NO_FLUSH) < Z_OK then
        goto error;

      outSize := bufferSize - zstream.avail_out;
      if outSize > 0 then
      begin
{$IFDEF FPC}
        if fpsend(outSocket, @outSize, sizeof(outSize), 0) <> sizeof(outSize) then
          goto error;
        if fpsend(outSocket, @outBuffer, outSize, 0) <> outSize then
          goto error;
{$ELSE}
        if send(outSocket, outSize, sizeof(outSize), 0) <> sizeof(outSize) then
          goto error;
        if send(outSocket, outBuffer, outSize, 0) <> outSize then
          goto error;
{$ENDIF}
      end;
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);
    inSize := inStream.Read(inBuffer, bufferSize);
  end;
  repeat
    zstream.next_out := @outBuffer;
    zstream.avail_out := bufferSize;
    zresult := deflate(zstream, Z_FINISH);
    if zresult < Z_OK then
      goto error;
    outSize := bufferSize - zstream.avail_out;
    if outSize > 0 then
    begin
{$IFDEF FPC}
      if fpsend(outSocket, @outSize, sizeof(outSize), 0) <> sizeof(outSize) then
        goto error;
      if fpsend(outSocket, @outBuffer, outSize, 0) <> outSize then
        goto error;
{$ELSE}
      if send(outSocket, outSize, sizeof(outSize), 0) <> sizeof(outSize) then
        goto error;
      if send(outSocket, outBuffer, outSize, 0) <> outSize then
        goto error;
{$ENDIF}
    end;
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);
  outsize := 0;
{$IFDEF FPC}
  if fpsend(outSocket, @outSize, sizeof(outSize), 0) <> sizeof(outSize) then
    goto error;
{$ELSE}
  if send(outSocket, outSize, sizeof(outSize), 0) <> sizeof(outSize) then
    goto error;
{$ENDIF}
  Result := deflateEnd(zstream) >= Z_OK;
  Exit;
  error:
  deflateEnd(zstream);
end;

function DecompressStream(inSocket: longint; outStream: TStream): boolean;
var
  zstream: TZStream;
  zresult: Integer;
  inBuffer: array[0..bufferSize - 1] of byte;
  outBuffer: array[0..bufferSize - 1] of byte;
  inSize: Integer;
  outSize: Integer;
label
  error;
begin
  Result := False;
  FillChar(zstream, SizeOf(zstream), 0);
  if InflateInit(zstream) < Z_OK then
    exit;
  if receive(inSocket, insize, sizeof(insize), 0) <> sizeof(insize) then
    goto error;
  if insize > 0 then
    if receive(inSocket, inBuffer, insize, 0) <> insize then
      goto error;
  while inSize > 0 do
  begin
    zstream.next_in := @inBuffer;
    zstream.avail_in := inSize;
    repeat
      zstream.next_out := @outBuffer;
      zstream.avail_out := bufferSize;
      if inflate(zstream, Z_NO_FLUSH) < Z_OK then
        goto error;
      outSize := bufferSize - zstream.avail_out;
      if outSize > 0 then
        outStream.Write(outBuffer, outSize);
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);
    if receive(inSocket, insize, sizeof(insize), 0) <> sizeof(insize) then
      goto error;
    if insize > 0 then
    begin
      if receive(inSocket, inBuffer, insize, 0) <> insize then
        goto error;
    end;
  end;
  repeat
    zstream.next_out := @outBuffer;
    zstream.avail_out := bufferSize;
    zresult := inflate(zstream, Z_FINISH);
    if zresult < Z_OK then
      goto error;
    outSize := bufferSize - zstream.avail_out;
    outStream.Write(outBuffer, outSize);
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);
  Result := inflateEnd(zstream) >= Z_OK;
  exit;
  error:
  inflateEnd(zstream);
end;

function StreamToStr(stream: TStream): string;
begin
  stream.Seek(0, soFromBeginning);
  SetLength(Result, stream.Size div SizeOf(Char));
  stream.Read(PChar(Result)^, stream.Size);
end;

function FileToString(const FileName: string): string;
var
  strm: TFileStream;
begin
  strm := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := StreamToStr(strm);
  finally
    strm.Free;
  end;
end;

{ TPooledMemoryStream }

procedure TPooledMemoryStream.Clear;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    FreeMem(FList[i]);
  FList.Clear;
  FSize := 0;
  FPosition := 0;
end;

constructor TPooledMemoryStream.Create(PageSize: integer);
begin
  Assert(PageSize > 0);
  FPageSize := PageSize;
  FList := TList.Create;
  FSize := 0;
  FPosition := 0;
end;

destructor TPooledMemoryStream.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TPooledMemoryStream.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TPooledMemoryStream.LoadFromSocket(socket: longint; readsize: boolean = true): boolean;
var
  count, i, j: integer;
  p: PByte;
begin
  Result := False;
  if readsize then
  begin
    if receive(socket, count, sizeof(count), 0) <> sizeof(count) then Exit;
    SetSize(count);
  end else
    count := Size;

  for i := 0 to FList.Count - 1 do
  begin
    p := FList[i];
    for j := 0 to FPageSize - 1 do
    begin
      if count > 0 then
      begin
        if receive(socket, p^, 1, 0) <> 1 then exit;
        dec(count);
      end else
        Break;
      inc(p);
    end;
  end;
  Result := true;
end;

procedure TPooledMemoryStream.LoadFromStream(Stream: TStream);
var
  s, count, i: integer;
begin
  Stream.Position := 0;
  SetSize(Stream.Size);
  count := FSize;
  i := 0;
  while count > 0 do
  begin
    if count > FPageSize then
      s := FPageSize else
      s := count;
    stream.ReadBuffer(FList[i]^, s);
    dec(count, s);
    inc(i);
  end;
end;

function TPooledMemoryStream.Read(var Buffer; Count: Integer): Longint;
var
  Pos, n: Integer;
  p, c: Pointer;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Pos := FPosition + Count;
    if Pos > 0 then
    begin
      if Pos > FSize then
        count := FSize - FPosition;
      Result := Count;
      c := @buffer;
      n := FPageSize - (FPosition mod FPageSize);
      if n > count then n := count;
      while n > 0 do
      begin
        p := Pointer(PtrInt(FList[FPosition div FPageSize]) + (FPosition mod FPageSize));
        Move(p^, c^, n);
        dec(count, n);
        inc(PtrInt(c), n);
        inc(FPosition, n);
        if count >= FPageSize then
          n := FPageSize else
          n := count;
      end;
      Exit;
    end;
  end;
  Result := 0;
end;

function TPooledMemoryStream.ReadString: string;
var
  s: Integer;
begin
  Result := '';
  Read(s, sizeof(s));
  if s > 0 then
  begin
    SetLength(Result, s);
    Read(Result[1], s);
  end;
end;

procedure TPooledMemoryStream.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TPooledMemoryStream.SaveToSocket(socket: longint; writesize: boolean): boolean;
var
  s, count, i: integer;
begin
  Result := False;
  count := FSize;
  if writesize then
{$IFDEF FPC}
    if fpsend(socket, @count, sizeof(count), 0) <> sizeof(count) then
      Exit;
{$ELSE}
    if send(socket, count, sizeof(count), 0) <> sizeof(count) then
      Exit;
{$ENDIF}
  i := 0;
  while count > 0 do
  begin
    if count >= FPageSize then
      s := FPageSize else
      s := count;
{$IFDEF FPC}
    if fpsend(socket, FList[i], s, 0) <> s then Exit;
{$ELSE}
    if send(socket, FList[i]^, s, 0) <> s then Exit;
{$ENDIF}
    dec(count, s);
    inc(i);
  end;
  Result := True;
end;

procedure TPooledMemoryStream.SaveToStream(Stream: TStream);
var
  s, count, i: integer;
begin
  count := FSize;
  i := 0;
  while count > 0 do
  begin
    if count >= FPageSize then
      s := FPageSize else
      s := count;
    stream.WriteBuffer(FList[i]^, s);
    dec(count, s);
    inc(i);
  end;
end;

function TPooledMemoryStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: Inc(FPosition, Offset);
    soFromEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

procedure TPooledMemoryStream.SetSize(NewSize: Integer);
var
  count, i: integer;
  p: Pointer;
begin
  if (NewSize mod FPageSize) > 0 then
    count := (NewSize div FPageSize) + 1 else
    count := (NewSize div FPageSize);
  if (count > FList.Count) then
  begin
    for i := FList.Count to count - 1 do
    begin
      GetMem(p, FPageSize);
      FList.Add(p);
    end;
  end else
  if (count < FList.Count) then
  begin
    for i := FList.Count - 1 downto Count do
    begin
      FreeMem(FList[i]);
      FList.Delete(i);
    end;
  end;
  FSize := NewSize;
  if FPosition > FSize then
    Seek(0, soFromEnd);
end;

function TPooledMemoryStream.Write(const Buffer; Count: Integer): Longint;
var
  Pos, n: Integer;
  p, c: Pointer;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Pos := FPosition + Count;
    if Pos > 0 then
    begin
      Result := Count;
      if Pos > FSize then
        SetSize(Pos);
      c := @buffer;
      n := FPageSize - (FPosition mod FPageSize);
      if n > count then n := count;
      while n > 0 do
      begin
        p := Pointer(PtrInt(FList[FPosition div FPageSize]) + (FPosition mod FPageSize));
        Move(c^, p^, n);
        dec(count, n);
        inc(PtrInt(c), n);
        inc(FPosition, n);
        if count >= FPageSize then
          n := FPageSize else
          n := count;
      end;
      Exit;
    end;
  end;
  Result := 0;
end;

procedure TPooledMemoryStream.WriteInteger(const V: Integer);
begin
  Write(v, sizeof(v));
end;

procedure TPooledMemoryStream.WriteString(const str: string; writesize: boolean; cp: Integer);
var
  s: Integer;
  data: RawByteString;
begin
  data := MBUEncode(str, cp);
  s := Length(data);
  if writesize then
    Write(s, sizeof(s));
  if s > 0 then
    Write(data[1], s * SizeOf(AnsiChar))
end;

end.
