{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Test Case for Sybase Tokenizer Classes         }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZTestSybaseToken;

interface
{$I ZParseSql.inc}
uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZClasses, ZTokenizer, ZSybaseToken, ZTestTokenizer;

type

  {** Implements a test case for SybaseTokenizer classes. }
  TZTestSybaseTokenizer = class(TZAbstractTokenizerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestWhitespaceState;
    procedure TestQuoteState;
    procedure TestCommentState;
    procedure TestSymbolState;
    procedure TestWordState;
    procedure TestNumberState;
  end;

implementation

{ TZTestSybaseTokenizer }

{**
  Sets up the test environment before tests.
}
procedure TZTestSybaseTokenizer.SetUp;
begin
  Tokenizer := TZSybaseTokenizer.Create;
end;

{**
  Runs a test for comments.
}
procedure TZTestSybaseTokenizer.TestCommentState;
const
  TokenString1: string = '-aaa/*bbb*/ccc--ddd'#10;
  TokenTypes1: array[0..4] of TZTokenType = (
    ttSymbol, ttWord, ttComment, ttWord, ttComment);
  TokenValues1: array[0..4] of string = (
    '-', 'aaa', '/*bbb*/', 'ccc', '--ddd'#10);
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1, [toSkipEOF]),
    TokenTypes1, TokenValues1);
end;

{**
  Runs a test for quoted strings.
}
procedure TZTestSybaseTokenizer.TestQuoteState;
const
  TokenString1: string = '"aaa" [aaa $1[2 bbb] ''c''''c''';
  TokenTypes1: array[0..2] of TZTokenType = (
    ttWord, ttWord, ttQuoted);
  TokenValues1: array[0..2] of string = (
    '"aaa"', '[aaa $1[2 bbb]', '''c''''c''');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

{**
  Runs a test for symbols.
}
procedure TZTestSybaseTokenizer.TestSymbolState;
const
  TokenString1: string = '=<>>=<= < > != !< !>';
  TokenTypes1: array[0..8] of TZTokenType = (
    ttSymbol, ttSymbol, ttSymbol, ttSymbol, ttSymbol, ttSymbol,
    ttSymbol, ttSymbol, ttSymbol);
  TokenValues1: array[0..8] of string = (
    '=', '<>', '>=', '<=', '<', '>', '!=', '!<', '!>');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

{**
  Runs a test for whitespaces.
}
procedure TZTestSybaseTokenizer.TestWhitespaceState;
const
  TokenString1: string = 'aaa '#9'ccc'#10#13;
  TokenTypes1: array[0..3] of TZTokenType = (
    ttWord, ttWhitespace, ttWord, ttWhitespace);
  TokenValues1: array[0..3] of string = (
    'aaa', ' '#9, 'ccc', #10#13);
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1, [toSkipEOF]),
    TokenTypes1, TokenValues1);
end;

{**
  Runs a test for words.
}
procedure TZTestSybaseTokenizer.TestWordState;
const
  TokenString1: string = ' _a_a. $b$b #p@2p';
  TokenTypes1: array[0..3] of TZTokenType = (
    ttWord, ttSymbol, ttWord, ttWord);
  TokenValues1: array[0..3] of string = (
    '_a_a', '.', '$b$b', '#p@2p');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

{**
  Runs a test for quoted strings.
}
procedure TZTestSybaseTokenizer.TestNumberState;
const
  TokenString1: string =
    '.A .123 123.456a 0xFfH 0x54BC 123.456e10 2E-12c';
  TokenTypes1: array[0..10] of TZTokenType = (
    ttSymbol, ttWord, ttFloat, ttFloat, ttWord, ttHexDecimal,
    ttWord, ttHexDecimal, ttFloat, ttFloat, ttWord);
  TokenValues1: array[0..10] of string = (
    '.', 'A', '.123', '123.456', 'a', '0xFf', 'H',
    '0x54BC', '123.456e10', '2E-12', 'c');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

initialization
  RegisterTest('parsesql',TZTestSybaseTokenizer.Suite);
end.

