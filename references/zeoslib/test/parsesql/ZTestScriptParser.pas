{*********************************************************}
{                                                         }
{                     Zeos SQL Shell                      }
{             Test Case for SQL Script Class              }
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

unit ZTestScriptParser;

interface
{$I ZParseSql.inc}
uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, SysUtils, ZTokenizer, ZScriptParser,
  ZTestDefinitions;

type

  {** Implements a test case for class TZSQLScriptParser. }

  { TZTestSQLScriptParserCase }

  TZTestSQLScriptParserCase = class(TZParseSQLGenericTestCase)
  private
    FParser: TZSQLScriptParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultProcessor;
    procedure TestGoProcessor;
    procedure TestEmptyLineProcessor;
    procedure TestSetTermProcessor;
    procedure TestCleanup;
  end;

implementation

uses Classes, ZDbcUtils, ZSysUtils, ZGenericSqlToken;

{ TZTestSQLScriptParserCase }

{**
  Prepares initial data before each test.
}
procedure TZTestSQLScriptParserCase.SetUp;
begin
  FParser := TZSQLScriptParser.Create;
  FParser.Tokenizer := TZGenericSQLTokenizer.Create;
end;

{**
  Removes data after each test.
}
procedure TZTestSQLScriptParserCase.TearDown;
begin
  FParser.Free;
end;

{**
  Runs a test for cleaning up SQL statements.
}
procedure TZTestSQLScriptParserCase.TestCleanup;
var
  EndOfLine: string;
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
begin
  CheckNotNull(FParser);
  FParser.DelimiterType := dtDefault;
  FParser.CleanupStatements := False;

  EndOfLine := #10#13;
  Line := '/AAA/ BBB CCC';
  Comment := '/* Comment... */';
  Delimiter := ';';

  Text := ' ' + Comment  + EndOfLine + Line + Delimiter
    + EndOfLine + '   ' + EndOfLine + Line + Comment + EndOfLine + Delimiter
    + Comment + EndOfLine + Line;
  FParser.ParseText(Text);

  CheckEquals(2, FParser.StatementCount);
  //--> ms, 20/10/2005: changed due to new behaviour (ignore CRLF and empty
  //                    lines or lines with only spaces, preceding an SQL-
  //                    Statement. Trailing CRLFs will also be ignored)
  //
  //CheckEquals(' ' + Comment  + EndOfLine + Line, FParser.Statements[0]);
  //CheckEquals(EndOfLine + '   ' + EndOfLine + Line + Comment + EndOfLine,
  //  FParser.Statements[1]);
  //CheckEquals(Comment + EndOfLine + Line, FParser.UncompletedStatement);
  //
  CheckEquals(Comment  + EndOfLine + Line, FParser.Statements[0]);
  CheckEquals(Line + Comment, FParser.Statements[1]);
  CheckEquals(Comment + EndOfLine + Line, FParser.UncompletedStatement);
  // <-- ms
end;

{**
  Runs a test for SQL Parser with default delimiters.
}
procedure TZTestSQLScriptParserCase.TestDefaultProcessor;
var
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
begin
  CheckNotNull(FParser);
  FParser.DelimiterType := dtDefault;

  Line := '/AAA/ BBB CCC';
  Comment := '/* Comment... */'#10#13;
  Delimiter := ';'#10#13;

  Text := Comment + Line + Delimiter + '   '#10#13 + Line + Comment + Delimiter
    + Comment + Line;
  FParser.ParseText(Text);

  CheckEquals(2, FParser.StatementCount);
  CheckEquals(Line, FParser.Statements[0]);
  CheckEquals(Line, FParser.Statements[1]);
  CheckEquals(Line, FParser.UncompletedStatement);

  FParser.ClearCompleted;
  CheckEquals(0, FParser.StatementCount);
  CheckEquals(Line, FParser.UncompletedStatement);

  FParser.Delimiter := '*';
  FParser.Clear;
  CheckEquals(0, FParser.StatementCount);
  CheckEquals('', FParser.UncompletedStatement);
  CheckEquals(';', FParser.Delimiter);
end;

{**
  Runs a test for SQL Parser with empty line delimiters.
}
procedure TZTestSQLScriptParserCase.TestEmptyLineProcessor;
var
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
begin
  CheckNotNull(FParser);
  FParser.DelimiterType := dtEmptyLine;

  Line := '/AAA/ BBB CCC';
  Comment := '/* Comment... */'#10#13;
  Delimiter := #10'  '#10#13;

  Text := Comment + Line + Delimiter + '   '#10#13 + Line + ';' + Comment
    + Delimiter + Comment + Line;
  FParser.ParseText(Text);

  CheckEquals(2, FParser.StatementCount);
  CheckEquals(Line, FParser.Statements[0]);
  CheckEquals(Line, FParser.Statements[1]);
  CheckEquals(Line, FParser.UncompletedStatement);
end;

{**
  Runs a test for SQL Parser with GO delimiter.
}
procedure TZTestSQLScriptParserCase.TestGoProcessor;
var
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
begin
  CheckNotNull(FParser);
  FParser.DelimiterType := dtGo;

  Line := '/AAA/ BBB CCC';
  Comment := '/* Comment... */'#10#13;
  Delimiter := #10#13'  Go  '#10#13;

  Text := Comment + Line + Delimiter + '   '#10#13 + Line + Comment + Delimiter
    + Comment + Line;
  FParser.ParseText(Text);

  CheckEquals(2, FParser.StatementCount);
  CheckEquals(Line, FParser.Statements[0]);
  CheckEquals(Line, FParser.Statements[1]);
  CheckEquals(Line, FParser.UncompletedStatement);
end;

{**
  Runs a test for SQL Parser with SET TERM delimiter.
}
procedure TZTestSQLScriptParserCase.TestSetTermProcessor;
var
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
begin
  CheckNotNull(FParser);
  FParser.DelimiterType := dtSetTerm;

  Line := '/AAA/ BBB CCC';
  Comment := '/* Comment... */'#10#13;
  Delimiter := '!!'#10#13;

  Text := ' Set Term !! ;'#10#13 + Comment + Line + Delimiter + '   '#10#13
    + Line + Comment + Delimiter + 'Set Term ; !!' + Comment + Line + ';';
  FParser.ParseText(Text);

  CheckEquals(3, FParser.StatementCount);
  CheckEquals(Line, FParser.Statements[0]);
  CheckEquals(Line, FParser.Statements[1]);
  CheckEquals(Line, FParser.Statements[2]);

  FParser.Clear;
  CheckEquals(0, FParser.StatementCount);
  CheckEquals('', FParser.UncompletedStatement);
  CheckEquals(';', FParser.Delimiter);

  Text := ' Set Term !! ;'#10#13 + Comment + Line + Delimiter + '   '#10#13
    + Line + Comment + Delimiter;
  FParser.ParseText(Text);
  CheckEquals(2, FParser.StatementCount);
  CheckEquals(Line, FParser.Statements[0]);
  CheckEquals(Line, FParser.Statements[1]);
  CheckEquals('!!', FParser.Delimiter);

  FParser.Clear;
  FParser.Delimiter:= '!!';
  Text := Comment + Line + Delimiter + '   '#10#13
    + Line + Comment + Delimiter;
  FParser.ParseText(Text);
  CheckEquals(2, FParser.StatementCount);
  CheckEquals(Line, FParser.Statements[0]);
  CheckEquals(Line, FParser.Statements[1]);
  CheckEquals('!!', FParser.Delimiter);

  FParser.Clear;
  FParser.Delimiter:= '!!';
  FParser.ParseText('9!!');
  CheckEquals(1, FParser.StatementCount,'1pos');
  CheckEquals('9', FParser.Statements[0]);
  CheckEquals('!!', FParser.Delimiter);
end;

initialization
  RegisterTest('parsesql',TZTestSQLScriptParserCase.Suite);
end.
