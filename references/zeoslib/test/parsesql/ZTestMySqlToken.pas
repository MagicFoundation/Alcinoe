{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Test Case for MySQL Tokenizer Classes          }
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

unit ZTestMySqlToken;

interface
{$I ZParseSql.inc}
uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZTestCase, ZClasses, ZTokenizer, ZMySqlToken,
  ZTestTokenizer;

type

  {** Implements a test case for MySqlTokenizer classes. }
  TZTestMySQLTokenizer = class(TZAbstractTokenizerTestCase)
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

uses SysUtils;

{ TZTestMySQLTokenizer }

{**
  Sets up the test environment before tests.
}
procedure TZTestMySQLTokenizer.SetUp;
begin
  Tokenizer := TZMySQLTokenizer.Create;
end;

{**
  Runs a test for comments.
}
procedure TZTestMySQLTokenizer.TestCommentState;
const
  TokenString1: string = 'aaa/*bbb*/ccc#ddd'#10'--a'#10'/*!az*/';
  TokenTypes1: array[0..5] of TZTokenType = (
    ttWord, ttComment, ttWord, ttComment, ttComment, ttSymbol);
  TokenValues1: array[0..5] of string = (
    'aaa', '/*bbb*/', 'ccc', '#ddd'#10, '--a'#10, '/*!az*/');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1, [toSkipEOF]),
    TokenTypes1, TokenValues1);
end;

{**
  Runs a test for quoted strings.
}
procedure TZTestMySQLTokenizer.TestQuoteState;
const
  TokenString1: string = '"aaa" `b``b`''c\''c''''c''';
  TokenTypes1: array[0..2] of TZTokenType = (
    ttQuoted, ttQuotedIdentifier, ttQuoted);
  TokenValues1: array[0..2] of string = (
    '"aaa"', '`b``b`', '''c\''c''''c''');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

{**
  Runs a test for symbols.
}
procedure TZTestMySQLTokenizer.TestSymbolState;
const
  TokenString1: string = '=<>>=<< < <';
  TokenTypes1: array[0..5] of TZTokenType = (
    ttSymbol, ttSymbol, ttSymbol, ttSymbol, ttSymbol, ttSymbol);
  TokenValues1: array[0..5] of string = (
    '=', '<>', '>=', '<<', '<', '<');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

{**
  Runs a test for whitespaces.
}
procedure TZTestMySQLTokenizer.TestWhitespaceState;
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
procedure TZTestMySQLTokenizer.TestWordState;
const
  TokenString1: string = ' _a_a. $c$c p2p';
  TokenTypes1: array[0..3] of TZTokenType = (
    ttWord, ttSymbol, ttWord, ttWord);
  TokenValues1: array[0..3] of string = (
    '_a_a', '.', '$c$c', 'p2p');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

{**
  Runs a test for quoted strings.
}
procedure TZTestMySQLTokenizer.TestNumberState;
const
  TokenString1: string = 'A.E .123 123.456a 0xFfH 0x54BC 123.456e10 2E-12c';
  TokenTypes1: array[0..11] of TZTokenType = (
    ttWord, ttSymbol, ttWord, ttFloat, ttFloat, ttWord, ttHexDecimal, ttWord,
    ttHexDecimal, ttFloat, ttFloat, ttWord);
  TokenValues1: array[0..11] of string = (
    'A', '.', 'E', '.123', '123.456', 'a', '0xFf', 'H', '0x54BC', '123.456e10',
    '2E-12', 'c');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

initialization
  RegisterTest('parsesql',TZTestMySQLTokenizer.Suite);
end.

