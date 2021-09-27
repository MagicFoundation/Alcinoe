{*
 * SQLite for Delphi and FreePascal/Lazarus
 *
 * This unit contains miscellaneous utility functions
 *
 * Copyright 2010-2013 Yury Plashenkov
 * http://plashenkov.github.io/sqlite/
 *
 * The MIT License (MIT)
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom
 * the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *}

unit SQLite3Utils;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

function StrToUTF8(const S: WideString): AnsiString;
function UTF8ToStr(const S: PAnsiChar; const Len: Integer = -1): WideString;
function QuotedStr(const S: WideString): WideString;
function FloatToSQLStr(Value: Extended): WideString;

implementation

uses
  {$IFNDEF FPC}Windows,{$ENDIF} SysUtils;

function StrToUTF8(const S: WideString): AnsiString;
begin
  Result := UTF8Encode(S);
end;

function UTF8ToStr(const S: PAnsiChar; const Len: Integer): WideString;
var
  UTF8Str: AnsiString;
begin
  if Len < 0 then
  begin
    Result := UTF8Decode(S);
  end
  else if Len > 0 then
  begin
    SetLength(UTF8Str, Len);
    Move(S^, UTF8Str[1], Len);
    Result := UTF8Decode(UTF8Str);
  end
  else Result := '';
end;

function QuotedStr(const S: WideString): WideString;
const
  Quote = #39;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if Result[I] = Quote then Insert(Quote, Result, I);
  Result := Quote + Result + Quote;
end;

function FloatToSQLStr(Value: Extended): WideString;
var
  FS: TFormatSettings;
begin
{$IFDEF FPC}
  FS := DefaultFormatSettings;
{$ELSE}
  GetLocaleFormatSettings(GetThreadLocale, FS);
{$ENDIF}
  FS.DecimalSeparator := '.';
  Result := FloatToStr(Value, FS);
end;

end.
