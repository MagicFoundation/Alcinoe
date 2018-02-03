{ **************************************************************************** }
{                                                                              }
{    Pascal PreProcessor Parser                                                }
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
{    The Original Code is PppParser.pas                                        }
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

unit PppParser;

interface

uses
  SysUtils, Classes, PppState, PppLexer;

type
  EPppParserError = class(Exception);

  TPppParser = class
  private
    FLexer: TPppLexer;
    FState: TPppState;
    FResult: string;
  protected
    procedure Emit(const AText: string);

    function ParseText: string;
    function ParseIfdef: string;
    function ParseIfndef: string;
    function ParseInclude: string;

    procedure ParseDefine;
    procedure ParseUndef;

    property Lexer: TPppLexer read FLexer;
    property State: TPppState read FState;
  public
    constructor Create(AStream: TStream; APppState: TPppState);
    destructor Destroy; override;
    function Parse: string;
  end;

implementation

{ TPppParser }

constructor TPppParser.Create(AStream: TStream; APppState: TPppState);
begin
  Assert(AStream <> nil);
  Assert(APppState <> nil);

  FLexer := TPppLexer.Create(AStream);
  FState := APppState;
end;

destructor TPppParser.Destroy;
begin
  FLexer.Free;
  inherited;
end;

procedure TPppParser.Emit(const AText: string);
begin
  FResult := FResult + AText;
end;

function TPppParser.Parse: string;
begin
  FLexer.Reset;
  FResult := '';

  Result := ParseText;

//  Result := FResult;
end;

procedure TPppParser.ParseDefine;
begin
  State.Define(Lexer.TokenAsString);
  Lexer.NextTok;
end;

function TPppParser.ParseIfdef: string;
begin
  if State.IsDefined(Lexer.TokenAsString) then
  begin
    Lexer.NextTok;
    Result := ParseText;
    if Lexer.CurrTok = ptElse then
    begin
      Lexer.NextTok;
      ParseText;
    end;
  end
  else
  begin
    Lexer.NextTok;
    ParseText;
    if Lexer.CurrTok = ptElse then
    begin
      Lexer.NextTok;
      Result := ParseText;
    end
    else
      Result := '';
  end;
  if Lexer.CurrTok <> ptEndif then
    Lexer.Error('$ENDIF expected');
  Lexer.NextTok;
end;

function TPppParser.ParseIfndef: string;
begin
  if not State.IsDefined(Lexer.TokenAsString) then
  begin
    Lexer.NextTok;
    Result := ParseText;
    if Lexer.CurrTok = ptElse then
    begin
      Lexer.NextTok;
      ParseText;
    end;
  end
  else
  begin
    Lexer.NextTok;
    ParseText;
    if Lexer.CurrTok = ptElse then
    begin
      Lexer.NextTok;
      Result := ParseText;
    end
    else
      Result := '';
  end;
  if Lexer.CurrTok <> ptEndif then
    Lexer.Error('$ENDIF expected');
  Lexer.NextTok;
end;

function TPppParser.ParseInclude: string;
var
  oldLexer, newLexer: TPppLexer;
  fsIn: TStream;
begin
  Assert(Lexer.TokenAsString <> '');
  { we must prevent case of $I- & $I+ becoming file names }
  if Lexer.TokenAsString[1] in ['-', '+'] then
    Result := Lexer.RawComment
  else
  begin
    fsIn := nil;
    newLexer := nil;
    oldLexer := Lexer;
    try
      try
        fsIn := TFileStream.Create(Lexer.TokenAsString, fmOpenRead);
      except
        on e: Exception do
          Lexer.Error(e.Message);
      end;
      newLexer := TPppLexer.Create(fsIn);
      FLexer := newLexer;
      Result := Parse;
    finally
      FLexer := oldLexer;
      fsIn.Free;
      newLexer.Free;
    end;
  end;
  Lexer.NextTok;
end;

function TPppParser.ParseText: string;
var
  strBuilder: TStrings;

  function BuildResult: string;
  var
    i, total: Integer;
    cp: PChar;
  begin
    total := 0;
    for i := 0 to strBuilder.Count - 1 do
      total := total + Length(strBuilder[i]);
    SetLength(Result, total);
    cp := Pointer(Result);
    for i := 0 to strBuilder.Count - 1 do
    begin
      Move(strBuilder[i][1], cp^, Length(strBuilder[i]));
      cp := cp + Length(strBuilder[i]);
    end;
  end;

begin
  strBuilder := TStringList.Create;
  try
    while True do
      case Lexer.CurrTok of
        ptComment:
        begin
          if not (poStripComments in State.Options) then
            strBuilder.Add(Lexer.TokenAsString);
          Lexer.NextTok;
        end;

        ptText:
        begin
          strBuilder.Add(Lexer.TokenAsString);
          Lexer.NextTok;
        end;

        ptDefine, ptUndef, ptIfdef, ptIfndef:
          if poProcessDefines in State.Options then
            case Lexer.CurrTok of
              ptDefine:
                ParseDefine;
              ptUndef:
                ParseUndef;
              ptIfdef:
                strBuilder.Add(ParseIfdef);
              ptIfndef:
                strBuilder.Add(ParseIfndef);
            end
          else
          begin
            strBuilder.Add(Lexer.RawComment);
            Lexer.NextTok;
          end;

        ptElse, ptEndif:
          if poProcessDefines in State.Options then
            Break
          else
          begin
            strBuilder.Add(Lexer.RawComment);
            Lexer.NextTok;
          end;

        ptInclude:
          if poProcessIncludes in State.Options then
            strBuilder.Add(ParseInclude)
          else
          begin
            strBuilder.Add(Lexer.RawComment);
            Lexer.NextTok;
          end;
      else
        Break;
      end;

    Result := BuildResult;
  finally
    strBuilder.Free;
  end;
end;

procedure TPppParser.ParseUndef;
begin
  State.Undef(Lexer.TokenAsString);
  Lexer.NextTok;
end;

end.

