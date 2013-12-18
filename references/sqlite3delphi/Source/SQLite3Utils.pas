{
  $Id: SQLite3Utils.pas 10 2010-02-13 05:37:54Z yury@plashenkov.com $

  Complete SQLite3 API translation and simple wrapper for Delphi and FreePascal
  Copyright © 2010 IndaSoftware Inc. and contributors. All rights reserved.
  http://www.indasoftware.com/fordev/sqlite3/

  SQLite is a software library that implements a self-contained, serverless,
  zero-configuration, transactional SQL database engine. The source code for
  SQLite is in the public domain and is thus free for use for any purpose,
  commercial or private. SQLite is the most widely deployed SQL database engine
  in the world.

  This package contains complete SQLite3 API translation for Delphi and
  FreePascal, as well as a simple Unicode-enabled object wrapper to simplify
  the use of this database engine.

  The contents of this file are used with permission, subject to the Mozilla
  Public License Version 1.1 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.
}

{
  Miscellaneous utility functions
}
unit SQLite3Utils;

{$WARN SYMBOL_DEPRECATED OFF}

interface

function StrToUTF8(const S: WideString): AnsiString;
function UTF8ToStr(const S: PAnsiChar; const Len: Integer = -1): WideString;
function QuotedStr(const S: WideString): WideString;
function FloatToSQLStr(Value: Extended): WideString;

implementation

uses SysUtils;

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
  SaveSeparator: Char;
begin
  SaveSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    Result := FloatToStr(Value);
  finally
    DecimalSeparator := SaveSeparator;
  end;
end;

end.
