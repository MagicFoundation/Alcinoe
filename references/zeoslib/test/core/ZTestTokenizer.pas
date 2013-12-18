{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for Tokenizer Classes              }
{                                                         }
{         Originally written by Sergey Seroukhov          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZTestTokenizer;

interface

{$I ZCore.inc}

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZClasses, ZCollections, SysUtils, Classes,
  ZCompatibility, ZTokenizer, ZTestDefinitions;

type
  {** Implements an abstract test case for tokenizers. }
  TZAbstractTokenizerTestCase = class (TZCoreGenericTestCase)
  private
    FTokenizer: TZTokenizer;
  protected
    procedure CheckTokens(Tokens: TZTokenDynArray;
      TokenTypes: array of TZTokenType; TokenValues: array of string);
    procedure CheckTokenList(Tokens: TStrings;
      TokenTypes: array of TZTokenType; TokenValues: array of string);

    procedure SetUp; override;
    procedure TearDown; override;

    property Tokenizer: TZTokenizer read FTokenizer write FTokenizer;
  published
    procedure TestPerformance;
  end;

  {** Implements a test case for Tokenizer classes. }
  TZTestTokenizer = class(TZAbstractTokenizerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestWhitespace;
    procedure TestComment;
    procedure TestNumber;
    procedure TestSymbol;
    procedure TestString;
  end;

implementation

{ TZAbstractTokenizerTestCase }

{**
  Sets up the test environment before tests.
}
procedure TZAbstractTokenizerTestCase.SetUp;
begin
  Tokenizer := nil;
end;

{**
  Cleans up the test environment after tests.
}
procedure TZAbstractTokenizerTestCase.TearDown;
begin
  if Tokenizer <> nil then
    Tokenizer.Free;
end;

{**
  Checks tokens array with predefined token types and values.
  @param Tokens an array of tokens.
  @param TokenTypes an array of token types.
  @param TokenValues an array of token values.
}
procedure TZAbstractTokenizerTestCase.CheckTokens(Tokens: TZTokenDynArray;
  TokenTypes: array of TZTokenType; TokenValues: array of string);
var
  I: Integer;
begin
  CheckEquals(High(Tokens), High(TokenTypes));
  CheckEquals(High(Tokens), High(TokenValues));

  for I := 0 to High(Tokens) do
  begin
    CheckEquals(Ord(TokenTypes[I]), Ord(Tokens[I].TokenType));
    CheckEquals(TokenValues[I], Tokens[I].Value);
  end;
end;

{**
  Checks tokens list with predefined token types and values.
  @param Tokens a list of tokens.
  @param TokenTypes an array of token types.
  @param TokenValues an array of token values.
}
procedure TZAbstractTokenizerTestCase.CheckTokenList(Tokens: TStrings;
  TokenTypes: array of TZTokenType; TokenValues: array of string);
var
  I: Integer;
begin
  CheckEquals(Tokens.Count, High(TokenTypes));
  CheckEquals(Tokens.Count, High(TokenValues));

  for I := 0 to Tokens.Count - 1 do
  begin
    CheckEquals(Ord(TokenTypes[I]), Integer(Tokens.Objects[I]));
    CheckEquals(TokenValues[I], Tokens[I]);
  end;
end;

{**
  Runs a test for performance.
}
procedure TZAbstractTokenizerTestCase.TestPerformance;
var
  I: Integer;
  TestStr: string;
  StartTicks: Cardinal;
begin
  TestStr := 'INSERT INTO Table (Fld1, Fld2) VALUES (12345678,'
    + ' ''abcdefghijklm'')';

  StartTicks := GetTickCount;
  for I := 1 to 10000 do
    Tokenizer.TokenizeBufferToList(TestStr, []).Free;

  PrintLn('Tokenizing performance = ' + IntToStr(GetTickCount - StartTicks));
end;

{ TZTestTokenizer }

{**
  Sets up the test environment before tests.
}
procedure TZTestTokenizer.SetUp;
begin
  Tokenizer := TZTokenizer.Create;
end;

{**
  Runs a test for comments.
}
procedure TZTestTokenizer.TestComment;
const
  TokenString1: string = 'aaa/*bbb*//ccc//ddd'#10;
  TokenTypes1: array[0..4] of TZTokenType = (
    ttWord, ttComment, ttSymbol, ttWord, ttComment);
  TokenValues1: array[0..4] of string = (
    'aaa', '/*bbb*/', '/', 'ccc', '//ddd'#10);

  TokenString2: string = 'aaa/*bbb*//ccc//ddd'#10;
  TokenTypes2: array[0..2] of TZTokenType = (
    ttWord, ttSymbol, ttWord);
  TokenValues2: array[0..2] of string = (
    'aaa', '/', 'ccc');

begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1, [toSkipEOF]),
    TokenTypes1, TokenValues1);
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString2,
    [toSkipEOF, toSkipComments]), TokenTypes2, TokenValues2);
end;

{**
  Runs a test for whitespace.
}
procedure TZTestTokenizer.TestWhitespace;
const
  TokenString1: string = 'aaa  ccc'#9' ddd '#10;
  TokenTypes1: array[0..5] of TZTokenType = (
    ttWord, ttWhitespace, ttWord, ttWhitespace, ttWord, ttWhitespace);
  TokenValues1: array[0..5] of string = (
    'aaa', '  ', 'ccc', #9' ', 'ddd', ' '#10);

  TokenString2: string = 'aaa  ccc'#9'ddd '#10;
  TokenTypes2: array[0..5] of TZTokenType = (
    ttWord, ttWhitespace, ttWord, ttWhitespace, ttWord, ttWhitespace);
  TokenValues2: array[0..5] of string = (
    'aaa', ' ', 'ccc', ' ', 'ddd', ' ');

  TokenString3: string = 'aaa  ccc'#9' ddd '#10;
  TokenTypes3: array[0..2] of TZTokenType = (
    ttWord, ttWord, ttWord);
  TokenValues3: array[0..2] of string = (
    'aaa', 'ccc', 'ddd');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1, [toSkipEOF]),
    TokenTypes1, TokenValues1);
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString2,
    [toSkipEOF, toUnifyWhitespaces]), TokenTypes2, TokenValues2);
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString3,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes3, TokenValues3);
end;

{**
  Runs a test for numbers.
}
procedure TZTestTokenizer.TestNumber;
const
  TokenString1: string = '123 12.34.--12';
  TokenTypes1: array[0..5] of TZTokenType = (
    ttInteger, ttWhitespace, ttFloat, ttSymbol, ttSymbol, ttInteger);
  TokenValues1: array[0..5] of string = (
    '123', ' ', '12.34', '.', '-', '-12');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1, [toSkipEOF]),
    TokenTypes1, TokenValues1);
end;

{**
  Runs a test for symbols.
}
procedure TZTestTokenizer.TestSymbol;
const
  TokenString1: string = '+-<>=<=';
  TokenTypes1: array[0..4] of TZTokenType = (
    ttSymbol, ttSymbol, ttSymbol, ttSymbol, ttSymbol);
  TokenValues1: array[0..4] of string = (
    '+', '-', '<>', '=', '<=');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1, [toSkipEOF]),
    TokenTypes1, TokenValues1);
end;

{**
  Runs a test for strings.
}
procedure TZTestTokenizer.TestString;
const
  TokenString1: string = '''aaa''''bbb''ccc"ddd''eee''"';
  TokenTypes1: array[0..3] of TZTokenType = (
    ttQuoted, ttQuoted, ttWord, ttQuoted);
  TokenValues1: array[0..3] of string = (
    '''aaa''', '''bbb''', 'ccc', '"ddd''eee''"');
  TokenString2: string = '''aaa''010ccc"ddd''eee''"';
  TokenTypes2: array[0..2] of TZTokenType = (
    ttQuoted, ttWord, ttQuoted);
  TokenValues2: array[0..2] of string = (
    '''aaa''', '010ccc', '"ddd''eee''"');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1, [toSkipEOF]),
    TokenTypes1, TokenValues1);
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString2, [toSkipEOF]),
    TokenTypes2, TokenValues2);
end;

initialization
  RegisterTest('core',TZTestTokenizer.Suite);
end.

