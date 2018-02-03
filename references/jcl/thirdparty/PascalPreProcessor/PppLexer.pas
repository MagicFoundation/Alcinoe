{ **************************************************************************** }
{                                                                              }
{    Pascal PreProcessor Lexer                                                 }
{    Copyright (c) 2001 Barry Kelly.                                           }
{    barry_j_kelly@hotmail.com                                                 }
{                                                                              }
{    The contents of this file are subject to the Mozilla Public License       }
{    Version 1.1 (the "License"); you may not use this file except in          }
{    compliance with the License. You may obtain a copy of the License at      }
{    http://www.mozilla.org/MPL/                                               }
{                                                                              }
{    Software distributed under the License is distributed on an "AS IS"       }
{    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   }
{    License for the specific language governing rights and limitations        }
{    under the License.                                                        }
{                                                                              }
{    The Original Code is PppLexer.pas                                         }
{                                                                              }
{    The Initial Developer of the Original Code is Barry Kelly.                }
{    Portions created by Barry Kelly are Copyright (C) 2001                    }
{    Barry Kelly. All Rights Reserved.                                         }
{                                                                              }
{    Alternatively, the contents of this file may be used under the terms      }
{    of the Lesser GNU Public License (the  "LGPL License"), in which case     }
{    the provisions of LGPL License are applicable instead of those            }
{    above.  If you wish to allow use of your version of this file only        }
{    under the terms of the LPGL License and not to allow others to use        }
{    your version of this file under the MPL, indicate your decision by        }
{    deleting  the provisions above and replace  them with the notice and      }
{    other provisions required by the LGPL License.  If you do not delete      }
{    the provisions above, a recipient may use your version of this file       }
{    under either the MPL or the LPGL License.                                 }
{                                                                              }
{ **************************************************************************** }
{ $Id$ }

unit PppLexer;

interface

uses
  SysUtils, Classes, JclStrHashMap, PCharUtils;

type
  TPppToken = (ptEof, ptComment, ptText,
    ptDefine, ptUndef, ptIfdef, ptIfndef, ptElse, ptEndif,
    ptInclude);

  EPppLexerError = class(Exception);

  TPppLexer = class
  private
    FBuf: string;
    FTokenHash: TStringHashMap;
    FCurrPos: PChar;
    FCurrLine: Integer;
    FCurrTok: TPppToken;
    FTokenAsString: string;
    FRawComment: string;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;

    procedure Error(const AMsg: string);
    procedure NextTok;
    procedure Reset;
    property CurrTok: TPppToken read FCurrTok;
    { TokenAsString is the preprocessor symbol for $IFDEF & $IFNDEF,
      and the file name for $I and $INCLUDE, and is the actual text
      for ptComment and ptText. }
    property TokenAsString: string read FTokenAsString;
    { The raw comment for $IFDEF, etc. when TokenAsString becomes the
      file name / preprocessor symbol. }
    property RawComment: string read FRawComment;
  end;

implementation

{ TPppLexer }

constructor TPppLexer.Create(AStream: TStream);
  procedure AddToken(const AIdent: string; AValue: TPppToken);
  var
    x: Integer;
  begin
    x := Ord(AValue);
    FTokenHash.Add(AIdent, x);
  end;
begin
  FTokenHash := TStringHashMap.Create(CaseInsensitiveTraits, 19);

  AddToken('i', ptInclude);
  AddToken('include', ptInclude);
  AddToken('ifdef', ptIfdef);
  AddToken('ifndef', ptIfndef);
  AddToken('else', ptElse);
  AddToken('endif', ptEndif);
  AddToken('define', ptDefine);
  AddToken('undef', ptUndef);

  SetLength(FBuf, AStream.Size);
  AStream.ReadBuffer(Pointer(FBuf)^, Length(FBuf));
  Reset;
end;

destructor TPppLexer.Destroy;
begin
  FTokenHash.Free;
  inherited;
end;

procedure TPppLexer.Error(const AMsg: string);
begin
  raise EPppLexerError.CreateFmt('(%d): %s', [FCurrLine, AMsg]);
end;

procedure TPppLexer.NextTok;

  procedure HandleDirective(APos: PChar);

    { needs to be special, because it checks for not * or }
    function ReadString(cp: PChar; var ident: string): PChar;
    var
      start: PChar;
    begin
      if cp^ = '"' then
      begin
        Inc(cp);
        start := cp;
        while not (cp^ in [#0, #10, #13, '"']) do
          Inc(cp);
        if cp^ in [#0, #10, #13] then
          Error('Unterminated string');
        SetString(ident, start, cp - start);
        Result := cp + 1;
      end
      else
      begin
        start := cp;
        while not (cp^ in [#0..#32, '*', '}']) do
          Inc(cp);
        if cp^ = #0 then
          Error('Unterminated string');
        SetString(ident, start, cp - start);
        Result := cp;
      end;
    end;

  var
    start: PChar;
    ident: string;
    tokInt: Integer;
  begin
    Assert(APos^ = '$');
    Inc(APos);
    start := APos;

    { read identifier }
    while APos^ in ['a'..'z', 'A'..'Z', '_'] do
      Inc(APos);
    SetString(ident, start, APos - start);

    { find identifier in hash map }
    if FTokenHash.Find(ident, tokInt) then
    begin
      FCurrTok := TPppToken(tokInt);

      case FCurrTok of
        ptDefine,
        ptUndef,
        ptIfdef,
        ptIfndef:
          ReadIdent(SkipWhite(APos), FTokenAsString);
        ptInclude:
          ReadString(SkipWhite(APos), FTokenAsString);
      end;
    end
    else
      { other directives must pass through; therefore call them text }
      FCurrTok := ptText;
  end;

var
  cp, start: PChar;
  cl: Integer;
label
  Label_NormalText;
begin
  { register variables optimization }
  cp := FCurrPos;
  cl := FCurrLine;

  { determine token type }
  case cp^ of

    { the buck stops here }
    #0:
    begin
      FCurrTok := ptEof;
      Exit;
    end;

    { possible Standard Pascal comment }
    '(':
    begin
      if (cp + 1)^ <> '*' then
        goto Label_NormalText;
      start := cp;
      Inc(cp, 2);
      while True do
      begin
        case cp^ of
          #0:
            Break;
          #10:
            Inc(cl);
          '*':
            if (cp + 1)^ = ')' then
              Break;
        end;
        Inc(cp);
      end;
      if cp^ = '*' then
        Inc(cp, 2); // get whole of comment, including trailing '*)'
      SetString(FTokenAsString, start, cp - start);
      FCurrTok := ptComment;
    end;

    { possible line comment }
    '/':
    begin
      if (cp + 1)^ <> '/' then
        goto Label_NormalText;
      start := cp;
      Inc(cp, 2);
      while True do
        case cp^ of
          #0, #10:
            Break;
        else
          Inc(cp);
        end;
      { if cp^ is #10, we leave it in, to avoid formatting cock-ups }
      SetString(FTokenAsString, start, cp - start);
      FCurrTok := ptComment;
    end;

    { pascal comment }
    '{':
    begin
      start := cp;
      while True do
      begin
        case cp^ of
          #0, '}':
            Break;
          #10:
            Inc(cl);
        end;
        Inc(cp);
      end;
      if cp^ = '}' then
        Inc(cp);
      SetString(FTokenAsString, start, cp - start);
      FCurrTok := ptComment;
    end;
  else
Label_NormalText:
    { process normal text; passes straight through until next comment or eof }
    start := cp;
    while True do
    begin
      case cp^ of
        #0:
          Break;
        #10:
          Inc(cl);
        '{':
          Break;
        '/':
          if (cp + 1)^ = '/' then
            Break;
        '(':
          if (cp + 1)^ = '*' then
            Break;

        { must handle strings seperately; there can be no comments in strings }
        '''':
        begin
          Inc(cp);
          while True do
            case cp^ of
              #0, #10:
              begin
                FCurrLine := cl;
                Error('String not terminated');
              end;
              '''':
                Break;
            else
              Inc(cp);
            end; { of '''' case }
        end;
      end;
      Inc(cp);
    end;
    SetString(FTokenAsString, start, cp - start);
    FCurrTok := ptText;
  end;

  { find out if we have a special directive }
  if FCurrTok = ptComment then
  begin
    FRawComment := FTokenAsString;
    case (start + 1)^ of
      '$': // {$
        HandleDirective(start + 1);

      '*': // (*$
        if (start + 2)^ = '$' then
          HandleDirective(start + 2);
      '/': // do nothing
        ;
    end;
  end;

  { restore register variables }
  FCurrPos := cp;
  FCurrLine := cl;
end;

procedure TPppLexer.Reset;
begin
  FCurrPos := PChar(FBuf);
  FCurrLine := 1;
  NextTok;
end;

end.
